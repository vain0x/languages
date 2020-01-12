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

let noop = "__noop"

let kgArgList context exit args =
  let rec go exit args =
    match args with
    | [] ->
      exit []

    | AArg (passBy, Some arg, _) :: args ->
      arg |> kgTerm context (fun arg ->
        args |> go (fun args ->
          KArg (passBy, arg, ref None) :: args |> exit
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

  let breakBody = ref KNoop
  let continueBody = ref KNoop

  let breakLabel =
    KLabel (
      context.FreshName "do_break",
      [],
      breakBody
    )

  let continueLabel =
    KLabel (
      context.FreshName "do_continue",
      [],
      continueBody
    )

  let continueNode = KPrim (KJumpPrim, [], [KLabelCont continueLabel])

  let loopStack = context.LoopStack
  context.LoopStack <-
    {
      BreakLabel = breakLabel
      ContinueLabel = continueLabel
    } :: loopStack

  breakBody := exit noop
  continueBody := bodyFun breakLabel continueLabel (fun _ -> continueNode)

  context.LoopStack <- loopStack

  KFix (
    KLabelFix breakLabel,
    KFix (
      KLabelFix continueLabel,
      continueNode
    ))

/// 1つの結果と1つの継続を持つプリミティブノードを作る。
let kPrim1 context prim args exit =
  let labelName = context.FreshName "next"
  let resultName = context.FreshName "result"

  let body = exit resultName

  let label =
    KLabel (
      labelName,
      // FIXME: モード
      [KParam (MutMode, resultName, KInferTy (resultName, ref None))],
      ref body
    )

  KFix (
    KLabelFix label,
    KPrim (prim, args, [KLabelCont label])
  )

let kgTerm (context: KirGenContext) exit term =
  match term with
  | ABoolLiteral (value, _) ->
    kPrim1 context (KBoolLiteralPrim value) [] exit

  | AIntLiteral (Some intText, _) ->
    kPrim1 context (KIntLiteralPrim intText) [] exit

  | AStrLiteral (segments, _) ->
    kPrim1 context (KStrLiteralPrim segments) [] exit

  | ANameTerm (AName (Some name, _)) ->
    exit name

  | AGroupTerm (Some term, _) ->
    kgTerm context exit term

  | ABlockTerm (stmts, _) ->
    kgStmts context exit stmts

  | ABreakTerm _ ->
    match context.LoopStack with
    | [] ->
      failwith "break out of loop"

    | { BreakLabel = breakLabel } :: _ ->
      KPrim (KJumpPrim, [], [KLabelCont breakLabel])

  | AContinueTerm _ ->
    match context.LoopStack with
    | [] ->
      failwith "continue out of loop"

    | { ContinueLabel = continueLabel } :: _ ->
      KPrim (KJumpPrim, [], [KLabelCont continueLabel])

  | ALoopTerm (Some body, _) ->
    kgLoop context exit (fun _ _ k -> body |> kgTerm context k)

  | ACallTerm (Some (ANameTerm (AName (Some funName, _))), args, _) ->
    args |> kgArgList context (fun args ->
      let prim = KFnPrim (funName, ref None)
      kPrim1 context prim args exit
    )

  | ABinTerm (Some bin, Some first, Some second, _) ->
    let prim = kPrimFromBin bin
    let passByList, _ = kPrimToSig prim

    first |> kgTerm context (fun first ->
      second |> kgTerm context (fun second ->
        let args =
          List.zip passByList [first; second]
          |> List.map (fun (passBy, arg) -> KArg (passBy, arg, ref (Some (passByToMode passBy))))

        kPrim1 context prim args exit
      ))

  | AIfTerm (Some cond, body, alt, _) ->
    // body または alt の結果を受け取って後続の計算を行う関数 if_next をおく。
    // 条件式の結果に基づいて body または alt のラベルにジャンプし、
    // その結果をもって if_next にジャンプする。

    let labelName = context.FreshName "if_next"
    let resultName = context.FreshName "res"

    let labelBody = ref KNoop

    let nextLabel =
      KLabel (
        labelName,
        // FIXME: モード
        [KParam (MutMode, resultName, KInferTy (resultName, ref None))],
        labelBody
      )

    // FIXME: モード
    let next result = KPrim (KJumpPrim, [KArg (ByMove, result, ref None)], [KLabelCont nextLabel])

    let bodyLabel =
      KLabel (
        context.FreshName "if_body",
        [],
        body
        |> Option.map (kgTerm context next)
        |> Option.defaultWith (fun () -> next "0") // FIXME: unit
        |> ref
      )

    let altLabel =
      KLabel (
        context.FreshName "if_alt",
        [],
        alt
        |> Option.map (kgTerm context next)
        |> Option.defaultWith (fun () -> next "0") // FIXME: unit
        |> ref
      )

    cond |> kgTerm context (fun cond ->
      labelBody := exit resultName

      KFix (
        KLabelFix nextLabel,
        KFix (
          KLabelFix altLabel,
          KFix (
            KLabelFix bodyLabel,
            KIf (
              cond,
              KLabelCont bodyLabel,
              KLabelCont altLabel
            )))))

  | AWhileTerm (Some cond, Some body, _) ->
    // cond while { body }
    // ==> loop { cond then { body } else { break } }

    kgLoop context exit (fun breakLabel _ bodyExit ->
      cond |> kgTerm context (fun cond ->
        let bodyLabel =
          KLabel (
            context.FreshName "body",
            [],
            body |> kgTerm context bodyExit |> ref
          )

        KFix (
          KLabelFix bodyLabel,
          KIf (
            cond,
            KLabelCont bodyLabel,
            KLabelCont breakLabel
          ))))

  | _ ->
    failwithf "unimpl %A" term

let kgStmt context exit stmt =
  match stmt with
  | ATermStmt (Some term, _) ->
    kgTerm context exit term

  | ALetStmt (Some param, Some arg, _) ->
    // let x = body; y
    // ==> fix next(x) { y }; jump next(body)

    let labelName = context.FreshName "let_next"

    let param = kgParam context param

    [arg] |> kgArgList context (fun args ->
      let labelBody = exit noop
      let nextLabel = KLabel (labelName, [param], ref labelBody)

      KFix (
        KLabelFix nextLabel,
        KPrim (KJumpPrim, args, [KLabelCont nextLabel])
    ))

  | AExternFnStmt (Some (AName (Some funName, _)), paramList, resultOpt, _) ->
    // 外部関数を呼び出すラッパー関数を定義する。

    // extern fn f(params)
    // ==> fix_fn f(params) { let res = extern_fn"f"(params); jump return(res) }

    let paramList = paramList |> List.map (kgParam context)

    let fnResult =
      resultOpt
      |> Option.map (kgResult context)
      |> Option.defaultValue KUnitTy
      |> KResult

    let primArgs =
      paramList |> List.map (fun (KParam (mode, name, _)) ->
        KArg (modeToPassBy mode, name, ref (Some mode))
      )

    let externFn = KExternFn (funName, paramList, fnResult)

    let fnBody = ref KNoop
    let fn = KFn (funName, paramList, fnResult, fnBody)

    fnBody :=
      KPrim (
        KExternFnPrim externFn,
        primArgs,
        [KReturnCont fn]
      )

    KFix (
      KFnFix fn,
      exit noop
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

    // FIXME: モード
    let fnBody = ref KNoop
    let fn = KFn (funName, paramList, result, fnBody)

    fnBody :=
      body |> kgTerm context (fun body ->
        KPrim (KJumpPrim, [KArg (ByMove, body, ref None)], [KReturnCont fn])
      )

    KFix (
      KFnFix fn,
      exit noop
    )

  | ASemiStmt (stmts, _) ->
    kgStmts context exit stmts

  | _ ->
    failwithf "unimpl %A" stmt

let kgStmts context exit stmts =
  match stmts with
  | [] ->
    exit noop

  | [stmt] ->
    kgStmt context exit stmt

  | stmt :: stmts ->
    stmt |> kgStmt context (fun _ -> kgStmts context exit stmts)

let kirGen (stmt: AStmt) =
  let context = kgContextNew ()
  kgStmt context (fun _ -> KNoop) stmt
