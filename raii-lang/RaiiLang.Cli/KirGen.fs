module rec RaiiLang.KirGen

open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.Syntax

[<Struct>]
type KirGenLoop =
  {
    BreakLabel: KLabel
    ContinueLabel: KLabel
  }

type KirGenContext =
  {
    FreshName: string -> string
    mutable LoopStack: KirGenLoop list
  }

let kgContextNew (): KirGenContext =
  {
    FreshName = freshNameFun ()
    LoopStack = []
  }

let kgArgList context exit args =
  let rec go exit args =
    match args with
    | [] ->
      exit []

    | AArg (passBy, Some arg, _) :: args ->
      arg |> kgTerm context (fun arg ->
        args |> go (fun args ->
          KArg (passBy, arg) :: args |> exit
        ))

    | _ ->
      failwithf "ERROR: 引数がありません %A" args

  go exit args

let kgParam context param =
  match param with
  | AParam (mode, Some (AName (Some name, _)), tyOpt, _) ->
    let name =
      context.FreshName name

    let ty =
      tyOpt
      |> Option.map (kgTy context)
      |> Option.defaultWith (fun () -> KInferTy (name, ref None))

    KParam (mode, name, ty)

  | _ ->
    failwithf "ERROR: パラメータ名がありません %A" param

let kgResult context result =
  match result with
  | AResult (Some ty, _) ->
    ty |> kgTy context

  | AResult (None, _) ->
    failwithf "ERROR: 結果型がありません %A" result

let kgTy _context ty =
  match ty with
  | ATy (Some "int", _) ->
    KIntTy

  | ATy (Some "string", _) ->
    KStrTy

  | ATy (Some name, _) ->
    // FIXME: 後で名前解決する
    KInferTy (name, ref None)

  | ATy (None, _) ->
    failwithf "ERROR: 型名がありません %A" ty

/// bodyFun: ループの残りの部分を生成する関数を受け取って、ループの本体を返す関数
let kgLoop context exit bodyFun =
  // loop { body }; k
  // ==> fix break() { k }
  //     fix continue() { let _ = body; jump continue() }
  //     jump continue()

  let breakLabel = context.FreshName "do_break"
  let continueLabel = context.FreshName "do_continue"

  let breakNode = KJump (KLabel breakLabel, [])
  let continueNode = KJump (KLabel continueLabel, [])

  let loopStack = context.LoopStack
  context.LoopStack <-
    {
      BreakLabel = KLabel breakLabel
      ContinueLabel = KLabel continueLabel
    } :: loopStack

  let onBreak = exit KNoop
  let onContinue = bodyFun breakNode continueNode (fun (_: KNode) -> continueNode)

  context.LoopStack <- loopStack

  KFix (
    breakLabel,
    KLabelFix,
    [],
    KResult KNeverTy,
    onBreak,
    KFix (
      continueLabel,
      KLabelFix,
      [],
      KResult KNeverTy,
      onContinue,
      continueNode
    ))

let kgTerm (context: KirGenContext) exit term =
  match term with
  | ABoolLiteral (value, _) ->
    let result = context.FreshName "b"
    KPrim (
      KBoolLiteralPrim value,
      [],
      KParam (MutMode, result, KBoolTy),
      exit (KName result)
    )

  | AIntLiteral (Some intText, _) ->
    let result = context.FreshName "n"
    KPrim (
      KIntLiteralPrim intText,
      [],
      KParam (MutMode, result, KIntTy),
      exit (KName result)
    )

  | AStrLiteral (segments, _) ->
    let result = context.FreshName "s"
    KPrim (
      KStrLiteralPrim segments,
      [],
      KParam (MutMode, result, KStrTy),
      exit (KName result)
    )

  | ANameTerm (AName (Some name, _)) ->
    KName name |> exit

  | AGroupTerm (Some term, _) ->
    kgTerm context exit term

  | ABlockTerm (stmts, _) ->
    kgStmts context exit stmts

  | ABreakTerm _ ->
    match context.LoopStack with
    | [] ->
      failwith "break out of loop"

    | { BreakLabel = breakLabel } :: _ ->
      KJump (breakLabel, [])

  | AContinueTerm _ ->
    match context.LoopStack with
    | [] ->
      failwith "continue out of loop"

    | { ContinueLabel = continueLabel } :: _ ->
      KJump (continueLabel, [])

  | ALoopTerm (Some body, _) ->
    kgLoop context exit (fun _ _ k -> body |> kgTerm context k)

  | ACallTerm (Some (ANameTerm (AName (Some funName, _))), args, _) ->
    let result = context.FreshName (sprintf "%s_res" funName)

    args |> kgArgList context (fun args ->
      KPrim (
        KFnPrim funName,
        args,
        KParam (MutMode, result, KInferTy (result, ref None)),
        exit (KName result)
      ))

  | ABinTerm (Some bin, Some first, Some second, _) ->
    let prim = kPrimFromBin bin
    let name = prim |> kPrimToString
    let resultName = context.FreshName (sprintf "%s_res" name)

    let paramList, resultTy = kPrimToSig prim

    first |> kgTerm context (fun first ->
      second |> kgTerm context (fun second ->
        let args = List.zip paramList [first; second] |> List.map KArg
        let result = KParam (MutMode, resultName, resultTy)

        KPrim (prim, args, result, exit (KName resultName))
      ))

  | AIfTerm (Some cond, body, alt, _) ->
    // body または alt の結果を受け取って後続の計算を行う関数 if_next をおく。
    // 条件式の結果に基づいて body または alt を計算して、
    // その結果をもって if_next にジャンプする。

    let nextLabel = context.FreshName "if_next"
    let next res = KJump (KLabel nextLabel, [KArg (ByMove, res)])

    let res = context.FreshName "res"

    let bodyFun next =
      body
      |> Option.map (kgTerm context next)
      |> Option.defaultWith (fun () -> next KNoop)

    let altFun next =
      alt
      |> Option.map (kgTerm context next)
      |> Option.defaultWith (fun () -> next KNoop)

    cond |> kgTerm context (fun cond ->
      KFix (
        nextLabel,
        KLabelFix,
        [KParam (MutMode, res, KIntTy)],
        KResult KNeverTy,
        exit (KName res),
        KIf (
          cond,
          bodyFun next,
          altFun next
        )))

  | AWhileTerm (Some cond, Some body, _) ->
    // cond while { body }
    // ==> loop { cond then { body } else { break } }

    kgLoop context exit (fun breakNode _ bodyExit ->
      cond |> kgTerm context (fun cond ->
        KIf (
          cond,
          body |> kgTerm context bodyExit,
          breakNode
        )))

  | _ ->
    failwithf "unimpl %A" term

let kgStmt context exit stmt =
  match stmt with
  | ATermStmt (Some term, _) ->
    kgTerm context exit term

  | ALetStmt (Some param, Some arg, _) ->
    // let x = body; y
    // ==> fix next(x) { y }; jump next(body)

    let nextLabel = context.FreshName "let_next"

    let param = kgParam context param

    [arg] |> kgArgList context (fun args ->
      KFix (
        nextLabel,
        KLabelFix,
        [param],
        KResult KNeverTy,
        exit KNoop,
        KJump (KLabel nextLabel, args)
    ))

  | AExternFnStmt (Some (AName (Some funName, _)), paramList, resultOpt, _) ->
    // 外部関数を呼び出すラッパー関数を定義する。

    // extern fn f(params)
    // ==> fix_fn f(params) { let res = extern_fn"f"(params); jump return(res) }

    let resultName = context.FreshName (sprintf "%s_res" funName)

    let passByList = paramList |> List.map (kgParam context)

    let fnResult =
      resultOpt
      |> Option.map (kgResult context)
      |> Option.defaultValue KUnitTy
      |> KResult

    let primArgs =
      passByList |> List.map (fun (KParam (mode, name, _)) ->
        KArg (mode |> modeToPassBy, KName name)
      )

    let primResult =
      match fnResult with
      | KResult resultTy ->
        KParam (MutMode, resultName, resultTy)

    KFix (
      funName,
      KFnFix,
      passByList,
      fnResult,
      KPrim (
        KExternFnPrim funName,
        primArgs,
        primResult,
        KJump (
          KReturnLabel,
          [KArg (ByMove, KName resultName)]
        )),
      exit KNoop
    )

  | AFnStmt (Some (AName (Some funName, _)), paramList, resultOpt, Some body, _) ->
    // 関数を fix で定義して、後続の計算を行う。

    // fn f(params) { return body }; exit
    // ==> fix fn f(params) { jump return(body) }; exit

    let paramList = paramList |> List.map (kgParam context)

    let result =
      resultOpt
      |> Option.map (kgResult context)
      |> Option.defaultValue KUnitTy
      |> KResult

    KFix (
      funName,
      KFnFix,
      paramList,
      result,
      body |> kgTerm context (fun body ->
        KJump (KReturnLabel, [KArg (ByMove, body)])
      ),
      exit KNoop
    )

  | ASemiStmt (stmts, _) ->
    kgStmts context exit stmts

  | _ ->
    failwithf "unimpl %A" stmt

let kgStmts context exit stmts =
  match stmts with
  | [] ->
    exit KNoop

  | [stmt] ->
    kgStmt context exit stmt

  | stmt :: stmts ->
    stmt |> kgStmt context (fun _ -> kgStmts context exit stmts)

let kirGen (stmt: AStmt) =
  let context = kgContextNew ()
  kgStmt context id stmt
