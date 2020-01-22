module rec PhancieLang.CirGen

open PhancieLang.Cir
open PhancieLang.Helpers
open PhancieLang.Kir
open PhancieLang.KirGen

type LVal =
  | LVal
  | RVal

type CirGenContext =
  {
    Stmts: ResizeArray<CStmt>
    Decls: ResizeArray<CDecl>
  }

let cgContextNew (): CirGenContext =
  {
    Stmts = ResizeArray()
    Decls = ResizeArray()
  }

let cgContextDerive (context: CirGenContext) =
  {
    context with
      Stmts = ResizeArray()
  }

let cgCaptureStmts (context: CirGenContext) body =
  let newStmts = ResizeArray()
  body { context with Stmts = newStmts }
  newStmts

let neverTerm = CName "__never__"

let addPrefix name = sprintf "fn_%s" name

let kTyToCTy (ty: KTyData) =
  match ty with
  | KNeverTy, _ ->
    CVoidTy

  | KUnitTy, _
  | KBoolTy, _
  | KIntTy, _ ->
    CIntTy

  | KStrTy, _ ->
    CPtrTy CCharTy

  | KFnTy (paramTys, resultTy), _ ->
    let paramTys =
      paramTys |> List.map (fun (KParamTy (mode, ty, _)) ->
        match mode with
        | ValMode
        | MutMode
        | InMode ->
          ty |> kTyToCTy

        | RefMode ->
          ty |> kTyToCTy |> CPtrTy
      )

    let resultTy =
      resultTy |> kTyToCTy

    CFunTy (paramTys, resultTy)

let cgParam _context (KParam (mode, name, ty, _)) =
  let ty = kTyToCTy ty

  match mode with
  | ValMode
  | MutMode
  | InMode ->
    CParam (name, ty)

  | RefMode ->
    CParam (name, CPtrTy ty)

let cgArg context lval (KArg (passBy, arg, _)) =
  let mode, arg = arg |> cgTerm context

  match passBy, mode, lval with
  | ByRef, _, RVal ->
    CUni (CRefUni, arg)

  | _, RefMode, LVal
  | _, RefMode, RVal ->
    CUni (CDerefUni, arg)

  | _ ->
    arg

let cgLocal (context: CirGenContext) param =
  match param with
  | KParam (_, paramName, paramTy, _) ->
    let paramTy = paramTy |> kTyToCTy

    // void 型の変数は生成しない。
    match paramTy with
    | CVoidTy ->
      ()

    | _ ->
      let localStmt = CLocalStmt (paramName, paramTy, None)
      context.Stmts.Add(localStmt)

let cgLocalAssign (context: CirGenContext) arg param =
  let (CParam (paramName, paramTy)) = cgParam context param

  // void 型の変数には代入しない。
  match paramTy with
  | CVoidTy ->
    ()

  | _ ->
    let assignStmt = CTermStmt (CBin (CAssignBin, CName paramName, arg))
    context.Stmts.Add(assignStmt)

let cgJump context cont args =
  match cont with
  | KLabelCont (KLabel (labelName, paramList, _, _)) ->
    for param, arg in List.zip paramList args do
      param |> cgLocalAssign context arg

    let gotoStmt = CGotoStmt labelName
    context.Stmts.Add(gotoStmt)

  | KReturnCont _ ->
    assert (args |> List.length <= 1)

    let argOpt = args |> List.tryHead
    let returnStmt = CReturn argOpt
    context.Stmts.Add(returnStmt)

let cgPrimTerm context prim args results nexts =
  let onBin bin =
    match args, results, nexts with
    | [first; second], [result], [next] ->
      let first = first |> cgArg context RVal
      let second = second |> cgArg context RVal
      let body = CBin (bin, first, second)
      result |> cgLocal context
      result |> cgLocalAssign context body
      next |> cgNode context

    | _ ->
      failwithf "ERROR: 二項演算は2つの引数と1個の継続を持つ %A" (args, results, nexts)

  let onAssign bin =
    match args, results, nexts with
    | [first; second], [result], [next] ->
      let first = first |> cgArg context LVal
      let second = second |> cgArg context RVal
      let body = CBin (bin, first, second)
      result |> cgLocal context
      result |> cgLocalAssign context body
      next |> cgNode context

    | _ ->
      failwithf "ERROR: 代入演算は2つの引数と1個の継続を持つ %A" (args, results, nexts)

  let onCall fnName =
    match results, nexts with
    | [result], [next] ->
      let args = args |> List.map (cgArg context RVal)
      let body = CCall (CName fnName, args)
      result |> cgLocal context
      result |> cgLocalAssign context body
      next |> cgNode context

    | _ ->
      failwithf "ERROR: 関数呼び出しは1個の継続を持つ %A" (fnName, results, nexts)

  match prim with
  | KIdPrim ->
    match args, results, nexts with
    | [arg], [result], [next] ->
      let arg = arg |> cgArg context RVal
      result |> cgLocal context
      result |> cgLocalAssign context arg
      next |> cgNode context

    | _ ->
      failwithf "ERROR: 束縛は1つの引数と1つの継続を持つ %A" (args, results, nexts)

  | KEqPrim ->
    onBin CEqBin

  | KAddPrim ->
    onBin CAddBin

  | KAssignPrim ->
    onAssign CAssignBin

  | KIfPrim ->
    match args, results, nexts with
    | [cond], [], [body; alt] ->
      let cond = cond |> cgArg context RVal
      let bodyStmts = cgCaptureStmts context (fun context -> cgNode context body)
      let altStmts = cgCaptureStmts context (fun context -> cgNode context alt)

      let ifStmt = CIfStmt (cond, bodyStmts |> Seq.toList, altStmts |> Seq.toList)
      context.Stmts.Add(ifStmt)

    | _ ->
      failwithf "ERROR: if は1個の引数と2個の継続を持つ %A" (args, nexts)

  | KFnPrim (KFn (fnName, _, _, _, _)) ->
    onCall (addPrefix fnName)

  | KExternFnPrim (KExternFn (fnName, paramList, resultTy, _)) ->
    let paramList = paramList |> List.map (cgParam context)
    let resultTy = resultTy |> kTyToCTy
    context.Decls.Add(CExternFnDecl (fnName, paramList, resultTy))

    onCall fnName

let cgTerm _context (term: KTermData) =
  match term with
  | KBoolLiteral false, _ ->
    ValMode, CInt "0"

  | KBoolLiteral true, _ ->
    ValMode, CInt "1"

  | KIntLiteral intText, _ ->
    ValMode, CInt intText

  | KStrLiteral segments, _ ->
    InMode, CStr segments

  | KLocalTerm (KParam (mode, name, _, _)), _ ->
    mode, CName name

let cgNode (context: CirGenContext) (node: KNodeData) =
  match node with
  | KNoop, _ ->
    ()

  | KPrim (prim, args, results, nexts), _ ->
    cgPrimTerm context prim args results nexts

  | KJump (cont, args), _ ->
    let args = args |> List.map (cgArg context RVal)
    cgJump context cont args

  | KFix (fixes, next), _ ->
    // 関数とラベルで生成するコードが全く異なる。
    // まず関数の定義を生成する。
    for fix in fixes do
      match fix with
      | KLabelFix _ ->
        ()

      | KFnFix (KFn (fnName, paramList, resultTy, bodySlot, _)) ->
        let paramList = paramList |> List.map (cgParam context)
        let resultTy = resultTy |> kTyToCTy

        let bodyContext = cgContextDerive context
        cgNodeOpt bodyContext !bodySlot
        let body = bodyContext.Stmts |> Seq.toList

        let fnDecl = CFnDecl (addPrefix fnName, paramList, resultTy, body)
        context.Decls.Add(fnDecl)

    // 各ラベルのパラメータを宣言する。
    for fix in fixes do
      match fix with
      | KLabelFix (KLabel (_, paramList, _, _)) ->
        for param in paramList do
          param |> cgLocal context

      | KFnFix _ ->
        ()

    // 次の処理を生成する。
    next |> cgNode context

    // 各ラベルの実体を配置する。
    for fix in fixes do
      match fix with
      | KLabelFix (KLabel (fnName, _, bodySlot, _)) ->
        let labelStmt = CLabelStmt fnName
        context.Stmts.Add(labelStmt)
        cgNodeOpt context !bodySlot

      | KFnFix _ ->
        ()

let cgNodeOpt context nodeOpt =
  match nodeOpt with
  | Some node ->
    node |> cgNode context

  | None ->
    ()

let cirGen (node: KNodeData) =
  let context = cgContextNew ()
  cgNode context node
  context.Decls |> Seq.toList
