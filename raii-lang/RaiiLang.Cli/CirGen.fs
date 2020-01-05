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
  match ty with
  | KNameTy _
  | KInferTy
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
        | MutMode ->
          ty |> kTyToCTy

        | InMode
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
  | MutMode ->
    CParam (name, ty)

  | InMode
  | RefMode ->
    CParam (name, CPtrTy ty)

let cgArg context (KArg (passBy, arg)) =
  let arg = cgTerm context arg

  match passBy with
  | ByMove ->
    arg

  | ByIn
  | ByRef ->
    CUni (CRefUni, arg)

let cgPrimTerm context prim args result next =
  let onLiteral body ty =
    let local = CLocalStmt (result, ty, Some body)
    context.Stmts.Add(local)
    cgNode context next
    CName result

  let onBin bin first second =
    let first = first |> cgArg context
    let second = second |> cgArg context
    let body = CBin (bin, first, second)
    let local = CLocalStmt (result, CIntTy, Some body)
    context.Stmts.Add(local)
    cgNode context next
    CName result

  let onCall fnName =
    let args = args |> List.map (cgArg context)
    let local = CLocalStmt (result, CIntTy, Some (CCall (CName fnName, args)))
    context.Stmts.Add(local)
    cgNode context next
    CName result

  match prim, args with
  | KBoolLiteralPrim false, [] ->
    onLiteral (CInt "0") CIntTy

  | KBoolLiteralPrim true, [] ->
    onLiteral (CInt "1") CIntTy

  | KIntLiteralPrim intText, [] ->
    onLiteral (CInt intText) CIntTy

  | KStrLiteralPrim segments, [] ->
    onLiteral (CStr segments) (CPtrTy CCharTy)

  | KEqPrim, [first; second] ->
    onBin CEqBin first second

  | KAddPrim, [first; second] ->
    onBin CAddBin first second

  | KAssignPrim, [first; second] ->
    onBin CAssignBin first second

  | KFnPrim fnName, _ ->
    onCall (addPrefix fnName)

  | KExternFnPrim fnName, _ ->
    onCall fnName

  | _ ->
    failwithf "unimpl %A" (prim, args)

let cgTerm (context: CirGenContext) (node: KNode) =
  match node with
  | KNoop ->
    CName "// noop"

  | KName name ->
    CName name

  | KPrim (prim, args, result, next) ->
    cgPrimTerm context prim args result next

  | KIf (cond, body, alt) ->
    // FIXME: 実装
    body |> cgTerm context

  | KJump (KLabel label, args) ->
    let args = args |> List.map (cgArg context)

    // FIXME: ジャンプ先のパラメータに args を代入する

    let gotoStmt = CGotoStmt label
    context.Stmts.Add(gotoStmt)

    neverTerm

  | KJump (KReturnLabel, args) ->
    assert (args |> List.length <= 1)

    let argOpt = args |> List.map (cgArg context) |> List.tryHead
    let returnStmt = CReturn argOpt
    context.Stmts.Add(returnStmt)

    neverTerm

  | KJump (KExitLabel, args) ->
    assert (args |> List.isEmpty)

    let exitStmt = CTermStmt (CCall (CName "exit", [CInt "0"]))
    context.Stmts.Add(exitStmt)

    neverTerm

  | KFix (funName, KLabelFix, paramList, _, body, next) ->
    for KParam (_mode, paramName, paramTy) in paramList do
      let paramTy = paramTy |> kTyToCTy
      let localStmt = CLocalStmt (paramName, paramTy, None)
      context.Stmts.Add(localStmt)

    cgTerm context next |> ignore

    let labelStmt = CLabelStmt funName
    context.Stmts.Add(labelStmt)
    cgTerm context body |> ignore

    neverTerm

  | KFix (funName, KFnFix, paramList, KResult resultTy, body, next) ->
    let paramList = paramList |> List.map (cgParam context)
    let resultTy = resultTy |> kTyToCTy

    let bodyContext = cgContextDerive context
    cgNode bodyContext body
    let body = bodyContext.Stmts |> Seq.toList

    let fnDecl = CFnDecl (addPrefix funName, paramList, resultTy, body)
    context.Decls.Add(fnDecl)

    cgTerm context next

let cgNode context (node: KNode) =
  match node with
  | KNoop _
  | KName _ ->
    ()

  | KPrim _
  | KJump _
  | KIf _
  | KFix _ ->
    cgTerm context node |> ignore

let cirGen (node: KNode) =
  let context = cgContextNew ()
  cgNode context node
  context.Decls |> Seq.toList
