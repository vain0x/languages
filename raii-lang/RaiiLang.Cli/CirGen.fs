module rec RaiiLang.CirGen

open RaiiLang.Cir
open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.KirGen

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

let cgArg _context lval (KArg (passBy, arg, modeOpt)) =
  let arg = CName arg
  let mode = !modeOpt |> Option.defaultValue ValMode

  match passBy, mode, lval with
  | ByRef, _, RVal ->
    CUni (CRefUni, arg)

  | _, RefMode, LVal
  | _, RefMode, RVal ->
    CUni (CDerefUni, arg)

  | _ ->
    arg

let cgJump context cont args =
  match cont with
  | KLabelCont (KLabel (labelName, paramList, _)) ->
    for param, arg in List.zip paramList args do
      let (CParam (paramName, paramTy)) = cgParam context param

      match paramTy with
      | CVoidTy ->
        ()

      | _ ->
        let assignStmt = CTermStmt (CBin (CAssignBin, CName paramName, arg))
        context.Stmts.Add(assignStmt)

    let gotoStmt = CGotoStmt labelName
    context.Stmts.Add(gotoStmt)

  | KReturnCont _ ->
    assert (args |> List.length <= 1)

    let argOpt = args |> List.tryHead
    let returnStmt = CReturn argOpt
    context.Stmts.Add(returnStmt)

let cgPrimTerm context prim args conts =
  let onLiteral body =
    match args, conts with
    | [], [cont] ->
      cgJump context cont [body]

    | _ ->
      failwithf "ERROR: リテラルは0個の引数と1個の継続を持つ %A" (args, conts)

  let onBin bin =
    match args, conts with
    | [first; second], [cont] ->
      let first = first |> cgArg context RVal
      let second = second |> cgArg context RVal
      let body = CBin (bin, first, second)
      cgJump context cont [body]

    | _ ->
      failwithf "ERROR: 二項演算は2つの引数と1個の継続を持つ %A" (args, conts)

  let onAssign bin =
    match args, conts with
    | [first; second], [cont] ->
      let first = first |> cgArg context LVal
      let second = second |> cgArg context RVal
      let body = CBin (bin, first, second)
      cgJump context cont [body]

    | _ ->
      failwithf "ERROR: 代入演算は2つの引数と1個の継続を持つ %A" (args, conts)

  let onCall fnName =
    match conts with
    | [cont] ->
      let args = args |> List.map (cgArg context RVal)
      let body = CCall (CName fnName, args)
      cgJump context cont [body]

    | _ ->
      failwithf "ERROR: 関数呼び出しは1個の継続を持つ %A" (fnName, conts)

  match prim with
  | KBoolLiteralPrim false ->
    onLiteral (CInt "0")

  | KBoolLiteralPrim true ->
    onLiteral (CInt "1")

  | KIntLiteralPrim intText ->
    onLiteral (CInt intText)

  | KStrLiteralPrim segments ->
    onLiteral (CStr segments)

  | KEqPrim ->
    onBin CEqBin

  | KAddPrim ->
    onBin CAddBin

  | KAssignPrim ->
    onAssign CAssignBin

  | KJumpPrim ->
    match conts with
    | [cont] ->
      let args = args |> List.map (cgArg context RVal)
      cgJump context cont args

    | _ ->
      failwithf "ERROR: jump は1個の継続を持つ %A" (conts)

  | KFnPrim (fnName, _) ->
    onCall (addPrefix fnName)

  | KExternFnPrim (KExternFn (fnName, paramList, KResult resultTy)) ->
    let paramList = paramList |> List.map (cgParam context)
    let resultTy = resultTy |> kTyToCTy
    context.Decls.Add(CExternFnDecl (fnName, paramList, resultTy))

    onCall fnName

let cgTerm (context: CirGenContext) (node: KNode) =
  match node with
  | KNoop ->
    CName "__noop"

  | KPrim (prim, args, conts) ->
    cgPrimTerm context prim args conts
    neverTerm

  | KIf (cond, body, alt) ->
    let bodyStmts = cgCaptureStmts context (fun context -> cgJump context body [])
    let altStmts = cgCaptureStmts context (fun context -> cgJump context alt [])

    let ifStmt = CIfStmt (CName cond, bodyStmts |> Seq.toList, altStmts |> Seq.toList)
    context.Stmts.Add(ifStmt)
    neverTerm

  | KFix (KLabelFix (KLabel (funName, paramList, body)), next) ->
    for KParam (_mode, paramName, paramTy) in paramList do
      let paramTy = paramTy |> kTyToCTy

      match paramTy with
      | CVoidTy ->
        ()

      | _ ->
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
  | KNoop _ ->
    ()

  | KPrim _
  | KIf _
  | KFix _ ->
    cgTerm context node |> ignore

let cirGen (node: KNode) =
  let context = cgContextNew ()
  cgNode context node
  context.Decls |> Seq.toList
