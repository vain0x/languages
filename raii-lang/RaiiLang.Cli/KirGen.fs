module rec RaiiLang.KirGen

open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.Syntax

let unitNode = KInt "0"

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

/// bodyFun: ループの残りの部分を生成する関数を受け取って、ループの本体を返す関数
let kgLoop context exit bodyFun =
  // loop { body }; k
  // ==> fix break() { k }
  //     fix continue() { let _ = body; jump continue() }
  //     jump continue()

  let breakLabel = context.FreshName "break"
  let continueLabel = context.FreshName "continue"

  let breakNode = KJump (KLabel breakLabel, [])
  let continueNode = KJump (KLabel continueLabel, [])

  let loopStack = context.LoopStack
  context.LoopStack <-
    {
      BreakLabel = KLabel breakLabel
      ContinueLabel = KLabel continueLabel
    } :: loopStack

  let onBreak = exit unitNode
  let onContinue = bodyFun breakNode continueNode (fun (_: KNode) -> continueNode)

  context.LoopStack <- loopStack

  KFix (
    breakLabel,
    KLabelFix,
    [],
    onBreak,
    KFix (
      continueLabel,
      KLabelFix,
      [],
      onContinue,
      continueNode
    ))

let kgTerm (context: KirGenContext) exit term =
  match term with
  | ABoolLiteral (value, _) ->
    KBool value |> exit

  | AIntLiteral (Some intText, _) ->
    KInt intText |> exit

  | AStrLiteral (segments, _) ->
    KStr segments |> exit

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
    // 関数から戻ってきた後の計算を中間関数 ret と定める。
    // ret を追加の引数として渡して、関数にジャンプする。

    let res = context.FreshName (sprintf "%s_res" funName)
    let ret = context.FreshName (sprintf "%s_ret" funName)

    let rec go exit args =
      match args with
      | [] ->
        exit []

      | AArg (passBy, Some arg, _) :: args ->
        arg |> kgTerm context (fun arg ->
          args |> go (fun args ->
            KArg (passBy, arg) :: args |> exit
          ))

      | _ :: args ->
        go exit args

    args |> go (fun args ->
      let args = KArg (ByMove, KName ret) :: args
      KPrim (KFnPrim funName, args, res, exit (KName res))
    )

  | ABinTerm (Some bin, Some first, Some second, _) ->
    let prim = kPrimFromBin bin
    let name = prim |> kPrimToString
    let res = context.FreshName (sprintf "%s_res" name)

    first |> kgTerm context (fun first ->
      second |> kgTerm context (fun second ->
        let args =
          List.zip (kPrimToSig prim) [first; second]
          |> List.map KArg

        KPrim (prim, args, res, exit (KName res))
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
      |> Option.defaultWith (fun () -> next unitNode)

    let altFun next =
      alt
      |> Option.map (kgTerm context next)
      |> Option.defaultWith (fun () -> next unitNode)

    cond |> kgTerm context (fun cond ->
      KFix (
        nextLabel,
        KLabelFix,
        [KParam (MutMode, res)],
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

  | ALetStmt
      (
        Some (AParam (mode, Some (AName (Some varName, _)), _)),
        Some (AArg (passBy, Some body, _)),
        _
      ) ->
    // let x = body; y
    // ==> fix next(x) { y }; jump next(body)

    // 右辺を計算する。
    // 後続の計算を行う中間関数 next を定義する。
    // 計算結果を引数に渡して next にジャンプする。

    let nextLabel = context.FreshName (sprintf "%s_next" varName)

    body |> kgTerm context (fun body ->
      KFix (
        nextLabel,
        KLabelFix,
        [KParam (mode, varName)],
        (exit (KName varName)),
        KJump (KLabel nextLabel, [KArg (passBy, body)])
    ))

  | AExternFnStmt (Some (AName (Some funName, _)), args, _) ->
    // extern fn f(params)
    // ==> fix_fn f(params) { let res = extern_fn"f"(params); jump return(res) }
    let res = context.FreshName (sprintf "%s_res" funName)

    let paramList =
      args |> List.choose (fun arg ->
        match arg with
        | AParam (mode, Some (AName (Some name, _)), _) ->
          KParam (mode, context.FreshName name) |> Some

        | _ ->
          None
      )

    let args =
      paramList |> List.map (fun (KParam (mode, name)) ->
        KArg (mode |> modeToPassBy, KName name)
      )

    KFix (
      funName,
      KFnFix,
      paramList,
      KPrim (
        KExternFnPrim funName,
        args,
        res,
        KJump (
          KReturnLabel,
          [KArg (ByMove, KName res)]
        )),
      exit unitNode
    )

  | AFnStmt (Some (AName (Some funName, _)), args, Some body, _) ->
    // 関数を fix で定義して、後続の計算を行う。

    // fn f(params) { return body }; exit
    // => fix_fn f(params) { return body }; exit

    let args =
      args |> List.choose (fun arg ->
        match arg with
        | AParam (callBy, Some (AName (Some name, _)), _) ->
          KParam (callBy, context.FreshName name) |> Some

        | _ ->
          None
      )

    KFix (
      funName,
      KFnFix,
      args,
      body |> kgTerm context (fun res ->
        KJump (KReturnLabel, [KArg (ByMove, res)])
      ),
      exit unitNode
    )

  | ASemiStmt (stmts, _) ->
    kgStmts context exit stmts

  | _ ->
    failwithf "unimpl %A" stmt

let kgStmts context exit stmts =
  match stmts with
  | [] ->
    exit unitNode

  | [stmt] ->
    kgStmt context exit stmt

  | stmt :: stmts ->
    stmt |> kgStmt context (fun _ -> kgStmts context exit stmts)

let kirGen (stmt: AStmt) =
  let context = kgContextNew ()
  kgStmt context id stmt
