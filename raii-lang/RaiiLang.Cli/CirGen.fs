module rec RaiiLang.CirGen

open RaiiLang.Cir
open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.KirGen

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

let neverTerm = CName "__never__"

let addPrefix name = sprintf "fn_%s" name

let kTyToCTy ty =
  match ty |> kTyDeref with
  | KInferTy _
  | KNeverTy ->
    CVoidTy

  | KUnitTy
  | KBoolTy
  | KIntTy ->
    CIntTy

  | KStrTy ->
    CPtrTy CCharTy

  | KFunTy (paramList, KResult resultTy) ->
    let paramList =
      paramList |> List.map (fun (KParam (mode, _, ty)) ->
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
    CFunTy (paramList, resultTy)

let cgParam _context (KParam (mode, name, ty)) =
  let ty = kTyToCTy ty

  match mode with
  | ValMode
  | MutMode
  | InMode ->
    CParam (name, ty)

  | RefMode ->
    CParam (name, CPtrTy ty)

let cgArg _context (KArg (passBy, arg)) =
  let arg = CName arg

  match passBy with
  | ByMove
  | ByIn ->
    arg

  | ByRef ->
    CUni (CRefUni, arg)

let cgJump context cont args =
  match cont with
  | KLabelCont (KLabel (labelName, paramList, _)) ->
    for param, arg in List.zip paramList args do
      let (CParam (paramName, _)) = cgParam context param
      let assignStmt = CTermStmt (CBin (CAssignBin, CName paramName, arg))
      context.Stmts.Add(assignStmt)

    let gotoStmt = CGotoStmt labelName
    context.Stmts.Add(gotoStmt)

  | KReturnCont _ ->
    assert (args |> List.length <= 1)

    let argOpt = args |> List.tryHead
    let returnStmt = CReturn argOpt
    context.Stmts.Add(returnStmt)

let cgPrimTerm context prim args next =
  let onLiteral body =
    cgJump context next [body]

  let onBin bin first second =
    let first = first |> cgArg context
    let second = second |> cgArg context
    let body = CBin (bin, first, second)
    cgJump context next [body]

  let onCall fnName =
    let args = args |> List.map (cgArg context)
    let body = CCall (CName fnName, args)
    cgJump context next [body]

  match prim, args with
  | KBoolLiteralPrim false, [] ->
    onLiteral (CInt "0")

  | KBoolLiteralPrim true, [] ->
    onLiteral (CInt "1")

  | KIntLiteralPrim intText, [] ->
    onLiteral (CInt intText)

  | KStrLiteralPrim segments, [] ->
    onLiteral (CStr segments)

  | KEqPrim, [first; second] ->
    onBin CEqBin first second

  | KAddPrim, [first; second] ->
    onBin CAddBin first second

  | KAssignPrim, [first; second] ->
    onBin CAssignBin first second

  | KJumpPrim, _ ->
    let args = args |> List.map (cgArg context)
    cgJump context next args

  | KFnPrim (fnName, _), _ ->
    onCall (addPrefix fnName)

  | KExternFnPrim (KExternFn (fnName, _, _)), _ ->
    onCall fnName

  | _ ->
    failwithf "unimpl %A" (prim, args)

let cgTerm (context: CirGenContext) (node: KNode) =
  match node with
  | KName name ->
    CName name

  | KPrim (prim, args, next) ->
    cgPrimTerm context prim args next
    neverTerm

  | KIf (cond, body, alt) ->
    // FIXME: 実装
    body |> cgTerm context

  | KFix (KLabelFix (KLabel (funName, paramList, body)), next) ->
    for KParam (_mode, paramName, paramTy) in paramList do
      let paramTy = paramTy |> kTyToCTy
      let localStmt = CLocalStmt (paramName, paramTy, None)
      context.Stmts.Add(localStmt)

    cgTerm context next |> ignore

    let labelStmt = CLabelStmt funName
    context.Stmts.Add(labelStmt)
    cgTerm context !body |> ignore

    neverTerm

  | KFix (KFnFix (KFn (funName, paramList, KResult resultTy, body)), next) ->
    let paramList = paramList |> List.map (cgParam context)
    let resultTy = resultTy |> kTyToCTy

    let bodyContext = cgContextDerive context
    cgNode bodyContext !body
    let body = bodyContext.Stmts |> Seq.toList

    let fnDecl = CFnDecl (addPrefix funName, paramList, resultTy, body)
    context.Decls.Add(fnDecl)

    cgTerm context next

let cgNode context (node: KNode) =
  match node with
  | KName _ ->
    ()

  | KPrim _
  | KIf _
  | KFix _ ->
    cgTerm context node |> ignore

let cirGen (node: KNode) =
  let context = cgContextNew ()
  cgNode context node
  context.Decls |> Seq.toList
