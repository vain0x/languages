module rec PhancieLang.KirGen

open PhancieLang.Ast
open PhancieLang.Helpers
open PhancieLang.Kir
open PhancieLang.Syntax

type KirGenContext =
  {
    FreshName: string -> string
    LoopMap: HashMap<ALoop, KLoop>
    FnMap: HashMap<AFn, KFnKind>
  }

let kgContextNew (): KirGenContext =
  {
    FreshName = freshNameFun ()
    LoopMap = HashMap()
    FnMap = HashMap()
  }

let noop syn: KTermData =
  KIntLiteral "/* noop */ 0", syn

let kUnit syn: KTermData =
  KIntLiteral "/* unit */ 0", syn

let kgError syn: KTermData =
  KIntLiteral "/* error */", syn

let kgContextTouchLoop aLoop (context: KirGenContext) =
  match context.LoopMap.TryGetValue(aLoop) with
  | true, kLoop ->
    kLoop

  | false, _ ->
    let syn = aLoop.Syn

    let breakLabel =
      KLabel ("do_break", [], ref None, syn)

    let continueLabel =
      KLabel ("do_continue", [], ref None, syn)

    let kLoop = KLoop (breakLabel, continueLabel)
    context.LoopMap.Add(aLoop, kLoop)
    kLoop

let kgContextTouchFn aFn (context: KirGenContext) =
  match context.FnMap.TryGetValue(aFn) with
  | true, kFnKind ->
    kFnKind

  | false, _ ->
    let syn = aFn.Node

    let kFnKind =
      match aFn.Kind with
      | AFnKind _ ->
        let paramList =
          aFn.Params |> List.map (kgParam context)

        let result =
          aFn.ResultTy |> kgTyInfo

        KFn (aFn.Name, paramList, result, ref None, syn) |> KFnKind

      | AExternFnKind ->
        let paramList =
          aFn.Params |> List.map (kgParam context)

        let result =
          aFn.ResultTy |> kgTyInfo

        KExternFn (aFn.Name, paramList, result, syn) |> KExternFnKind

    context.FnMap.Add(aFn, kFnKind)
    kFnKind

let kgParamTy paramTy: KParamTy =
  match paramTy with
  | AParamTy (mode, ty, syn) ->
    let ty = kgTyInfo ty
    KParamTy (mode, ty, syn)

let kgArgTy argTy: KArgTy =
  match argTy with
  | AArgTy (passBy, ty, syn) ->
    let ty = kgTyInfo ty
    KArgTy (passBy, ty, syn)

let kgTyInfo ty: KTyData =
  match ty with
  | ANameTy (_, symbolSlot, syn) ->
    match !symbolSlot with
    | Some (ATySymbol (_, ty)) ->
      kgTyInfo ty

    | _ ->
      KNeverTy, syn

  | ABoolTy syn ->
    KBoolTy, syn

  | AIntTy syn ->
    KIntTy, syn

  | AStringTy syn ->
    KStrTy, syn

  | AFnTy (paramTys, resultTy, syn) ->
    let paramTys =
      paramTys |> List.map kgParamTy

    let resultTy =
      resultTy |> kgTyInfo

    KFnTy (paramTys, resultTy), syn

let kgParam _context param =
  match param with
  | AParam (mode, nameOpt, _, syn) ->
    let paramOpt =
      match nameOpt with
      | Some (AName (_, symbolSlot, _)) ->
        match !symbolSlot with
        | Some (ALocalSymbol (name, mode, ty)) ->
          let ty =
            kgTyInfo ty
          KParam (mode, name, ty, syn) |> Some

        | _ ->
          None

      | None ->
        None

    paramOpt |> Option.defaultWith (fun () ->
      KParam (mode, "__param", (KNeverTy, syn), syn)
    )

let kgArgList context exit args =
  let rec go exit args =
    match args with
    | [] ->
      exit []

    | AArg (passBy, argOpt, syn) :: args ->
      let exit arg =
        args |> go (fun args ->
          KArg (passBy, arg, syn) :: args |> exit
        )

      match argOpt with
      | Some arg ->
        arg |> kgTerm context exit

      | None ->
        exit (noop syn)

  go exit args

let kLabelSetBody body label =
  match label with
  | KLabel (_, _, bodySlot, _) ->
    bodySlot := Some body

/// bodyFn: ループの残りの部分を生成する関数を受け取って、ループの本体を返す関数
let kgLoop context exit aLoop syn bodyFn: KNodeData =
  // loop { body }; k
  // ==> fix label break() { k }
  //     and label continue() { let _ = body; jump continue() }
  //     jump continue()

  let kLoop = context |> kgContextTouchLoop aLoop

  match kLoop with
  | KLoop (breakLabel, continueLabel) ->
    let continueJump: KNodeData =
      KJump (KLabelCont continueLabel, []), syn

    breakLabel |> kLabelSetBody (
      exit (noop syn)
    )

    continueLabel |> kLabelSetBody (
      bodyFn breakLabel continueLabel (fun _ -> continueJump)
    )

    KFix (
      [
        KLabelFix continueLabel
        KLabelFix breakLabel
      ],
      continueJump
    ), syn

/// 1つの結果と1つの継続を持つプリミティブノードを作る。
let kPrim1 context prim args resultTy syn (exit: KTermData -> KNodeData): KNodeData =
  // FIXME: モード？
  let resultName = context.FreshName "result"
  let result = KParam (MutMode, resultName, resultTy, syn)

  let body =
    let result: KTermData = KLocalTerm result, syn
    exit result

  KPrim (
    prim,
    args,
    [result],
    [body]),
  syn

let kgTerm (context: KirGenContext) (exit: KTermData -> KNodeData) (term: ATerm): KNodeData =
  match term with
  | ABoolLiteral (value, syn) ->
    exit (KBoolLiteral value, syn)

  | AIntLiteral (textOpt, syn) ->
    let text = textOpt |> Option.defaultValue "0"
    exit (KIntLiteral text, syn)

  | AStrLiteral (segments, syn) ->
    exit (KStrLiteral segments, syn)

  | ANameTerm (AName (_, symbolSlot, syn)) ->
    let name, mode, ty =
      match !symbolSlot with
      | Some (ALocalSymbol (name, mode, ty)) ->
        let ty = kgTyInfo ty
        name, mode, ty

      | _ ->
        "__local", MutMode, (KNeverTy, syn)

    exit (KLocalTerm (KParam (mode, name, ty, syn)), syn)

  | AGroupTerm (bodyOpt, syn) ->
    match bodyOpt with
    | Some body ->
      kgTerm context exit body

    | None ->
      exit (kgError syn)

  | ABlockTerm (stmts, syn) ->
    kgStmts context exit stmts syn

  | ABreakTerm (loopSlot, syn) ->
    match !loopSlot with
    | Some aLoop ->
      match context |> kgContextTouchLoop aLoop with
      | KLoop (breakLabel, _) ->
        KJump (KLabelCont breakLabel, []), syn

    | None ->
      exit (kgError syn)

  | AContinueTerm (loopSlot, syn) ->
    match !loopSlot with
    | Some aLoop ->
      match context |> kgContextTouchLoop aLoop with
      | KLoop (breakLabel, _) ->
        KJump (KLabelCont breakLabel, []), syn

    | None ->
      exit (kgError syn)

  | ALoopTerm (bodyOpt, loopSlot, syn) ->
    match bodyOpt, !loopSlot with
    | Some body, Some aLoop ->
      kgLoop context exit aLoop syn (fun _ _ k -> body |> kgTerm context k)

    | _ ->
      exit (kgError syn)

  | ACallTerm (nameOpt, args, syn) ->
    match nameOpt with
    | Some (ANameTerm (AName (_, symbolSlot, _))) ->
      match !symbolSlot with
      | Some (AFnSymbol fn) ->
        let fnKind = context |> kgContextTouchFn fn

        let kFnToResultTy fn =
          match fn with
          | KFn (_, _, resultTy, _, _) ->
            resultTy

        let kExternFnToResultTy externFn =
          match externFn with
          | KExternFn (_, _, resultTy, _) ->
            resultTy

        args |> kgArgList context (fun args ->
          match fnKind with
          | KFnKind fn ->
            let prim = KFnPrim fn
            let resultTy = fn |> kFnToResultTy
            kPrim1 context prim args resultTy syn exit

          | KExternFnKind externFn ->
            let prim = KExternFnPrim externFn
            let resultTy = externFn |> kExternFnToResultTy
            kPrim1 context prim args resultTy syn exit
        )

      | _ ->
        exit (kgError syn)

    | _ ->
      exit (kgError syn)

  | ABinTerm (binOpt, firstOpt, secondOpt, slot, syn) ->
    match binOpt, firstOpt, secondOpt, !slot with
    | Some bin, Some first, Some second, Some (firstTy, secondTy, resultTy) ->
      let prim = kPrimFromBin bin

      first |> kgTerm context (fun first ->
        second |> kgTerm context (fun second ->
          let toArg arg argTy =
            match kgArgTy argTy with
            | KArgTy (passBy, _, _) ->
              KArg (passBy, arg, syn)

          let first = toArg first firstTy
          let second = toArg second secondTy
          let resultTy = kgTyInfo resultTy

          kPrim1 context prim [first; second] resultTy syn exit
        ))

    | _ ->
      exit (kgError syn)

  | AIfTerm (condOpt, bodyOpt, altOpt, tySlot, syn) ->
    // body または alt の結果を受け取って後続の計算を行う関数 if_next をおく。
    // 条件式の結果に基づいて body または alt のラベルにジャンプし、
    // その結果を継続に渡す。

    match condOpt, !tySlot with
    | Some cond, Some resultTy ->
      // FIXME: モード
      let passBy = ByMove
      let mode = MutMode

      let resultTy = kgTyInfo resultTy

      let labelName = context.FreshName "if_next"

      let resultName = context.FreshName "result"
      let resultTerm: KTermData =
        KLocalTerm (KParam (mode, resultName, resultTy, syn)), syn

      let nextLabel =
        KLabel (
          labelName,
          [KParam (mode, resultName, resultTy, syn)],
          ref None,
          syn
        )

      // FIXME: モード
      let next result: KNodeData =
        KJump (
          KLabelCont nextLabel,
          [KArg (ByMove, result, syn)]
        ), syn

      let bodyLabel =
        KLabel (
          context.FreshName "if_body",
          [],
          bodyOpt
          |> Option.map (kgTerm context next)
          |> Option.defaultWith (fun () -> next (kUnit syn))
          |> Some
          |> ref,
          syn
        )

      let altLabel =
        KLabel (
          context.FreshName "if_alt",
          [],
          altOpt
          |> Option.map (kgTerm context next)
          |> Option.defaultWith (fun () -> next (kUnit syn))
          |> Some
          |> ref,
          syn
        )

      cond |> kgTerm context (fun cond ->
        nextLabel |> kLabelSetBody (exit resultTerm)

        KFix (
          [
            KLabelFix bodyLabel
            KLabelFix altLabel
            KLabelFix nextLabel
          ],
          (KPrim (
            KIfPrim,
            [KArg (passBy, cond, syn)],
            [],
            [
              KJump (KLabelCont bodyLabel, []), syn
              KJump (KLabelCont altLabel, []), syn
            ]), syn
          )), syn
      )

    | _ ->
      exit (kgError syn)

  | AWhileTerm (condOpt, bodyOpt, loopSlot, syn) ->
    // cond while { body }
    // ==> loop { cond then { body } else { break } }

    match condOpt, bodyOpt, !loopSlot with
    | Some cond, Some body, Some loop ->
      // FIXME: モード
      let passBy = ByMove

      kgLoop context exit loop syn (fun breakLabel _ bodyExit ->
        cond |> kgTerm context (fun cond ->
          let bodyLabel =
            KLabel (
              context.FreshName "body",
              [],
              body
              |> kgTerm context bodyExit
              |> Some
              |> ref,
              syn
            )

          KFix (
            [KLabelFix bodyLabel],
            (KPrim (
              KIfPrim,
              [KArg (passBy, cond, syn)],
              [],
              [
                KJump (KLabelCont bodyLabel, []), syn
                KJump (KLabelCont breakLabel, []), syn
              ]), syn)
          ), syn
        ))

    | _ ->
      exit (kgError syn)

let kgStmt context exit stmt =
  match stmt with
  | ATermStmt (bodyOpt, syn) ->
    match bodyOpt with
    | Some body ->
      kgTerm context exit body

    | None ->
      exit (kgError syn)

  | ALetStmt (paramOpt, argOpt, syn) ->
    // let x = body; y
    // ==> fix next(x) { y }; jump next(body)

    match paramOpt, argOpt with
    | Some param, Some arg ->
      let param = kgParam context param

      let result: KTermData =
        match param with
        | KParam (mode, name, ty, syn) ->
          KLocalTerm (KParam (mode, name, ty, syn)), syn

      [arg] |> kgArgList context (fun args ->
        KPrim (
          KIdPrim,
          args,
          [param],
          [exit result]
        ), syn
      )

    | _ ->
      exit (kgError syn)

  | AExternFnStmt (_, _, _, fnSlot, syn) ->
    // 外部関数を呼び出すラッパー関数を定義する。

    // extern fn f(params)
    // ==> fix_fn f(params) { let res = extern_fn"f"(params); jump return(res) }

    match !fnSlot with
    | Some fn ->
      let paramList = fn.Params |> List.map (kgParam context)
      let resultTy = fn.ResultTy |> kgTyInfo

      let resultName = context.FreshName "result"
      let mode = MutMode // FIXME: mode
      let passBy = ByMove
      let result = KParam (mode, resultName, resultTy, syn)

      let args =
        fn.Params |> List.map (fun param ->
          let param = param |> kgParam context
          KArg (passBy, (KLocalTerm param, syn), syn)
        )

      let externFn = KExternFn (fn.Name, paramList, resultTy, syn)
      let bodySlot = ref None
      let fn = KFn (fn.Name, paramList, resultTy, bodySlot, syn)

      bodySlot := struct
        (KPrim (
          KExternFnPrim externFn,
          args,
          [result],
          [
            KJump (
              KReturnCont fn,
              [KArg (passBy, (KLocalTerm result, syn), syn)]
            ), syn
          ]), syn)
        |> Some

      KFix (
        [KFnFix fn],
        exit (noop syn)
      ), syn

    | _ ->
      exit (kgError syn)

  | AFnStmt (_, _, _, bodyOpt, fnSlot, syn) ->
    // 関数を fix で定義して、後続の計算を行う。

    // fn f(params) { return body }; exit
    // ==> fix fn f(params) { jump return(body) }; exit

    match bodyOpt, !fnSlot with
    | Some body, Some fn ->
      // FIXME: mode
      let passBy = ByMove

      let paramList = fn.Params |> List.map (kgParam context)
      let resultTy = fn.ResultTy |> kgTyInfo

      let bodySlot = ref None
      let fn = KFn (fn.Name, paramList, resultTy, bodySlot, syn)

      bodySlot :=
        body
        |> kgTerm context (fun result ->
          KJump (
            KReturnCont fn,
            [KArg (passBy, result, syn)]
          ), syn
        )
        |> Some

      KFix (
        [KFnFix fn],
        exit (noop syn)
      ), syn

    | _ ->
      exit (kgError syn)

  | AStructStmt (_, _, syn) ->
    exit (noop syn)

  | ASemiStmt (stmts, syn) ->
    kgStmts context exit stmts syn

let kgStmts context exit stmts syn =
  match stmts with
  | [] ->
    exit (noop syn)

  | [stmt] ->
    kgStmt context exit stmt

  | stmt :: stmts ->
    stmt |> kgStmt context (fun _ -> kgStmts context exit stmts syn)

let kirGen (stmt: AStmt) =
  let context = kgContextNew ()

  let exit ((_, syn): KTermData): KNodeData =
    KNoop, syn

  kgStmt context exit stmt
