module rec RaiiLang.KirInfer

open RaiiLang.Helpers
open RaiiLang.Kir

type KirInferContext =
  {
    mutable TyMap: Map<string, KTy>
  }

let kiContextNew (): KirInferContext =
  {
    TyMap = Map.empty
  }

let kiUnify context first second =
  match first, second with
  | KNeverTy, _
  | _, KNeverTy
  | KUnitTy, KUnitTy
  | KBoolTy, KBoolTy
  | KIntTy, KIntTy
  | KStrTy, KStrTy ->
    ()

  | KInferTy (_, first),
    KInferTy (_, second)
    when first === second ->
    ()

  | KInferTy (_, first), _ ->
    match !first with
    | Some first ->
      kiUnify context first second

    | None ->
      first := Some second

  | _, KInferTy _ ->
    kiUnify context second first

  | KFunTy (firstParams, firstResult),
    KFunTy (secondParams, secondResult) ->
    if List.length firstParams = List.length secondParams then
      for KParam (_, _, first), KParam (_, _, second) in firstParams |> List.zip secondParams do
        // FIXME: モードをチェックする。型によっては ref と in/mut/val が両立しない
        kiUnify context first second |> ignore

      match firstResult, secondResult with
      | KResult first,
        KResult second ->
        kiUnify context first second
    else
      eprintfn "WARN: Type error %A" (first, second)

  | _ ->
    eprintfn "WARN: Type error %A" (first, second)

let kiArg context arg =
  match arg with
  | KArg (_, arg) ->
    arg |> kiNode context

let kiParam (context: KirInferContext) param =
  match param with
  | KParam (_, name, ty) ->
    context.TyMap <- context.TyMap |> Map.add name ty

let kiCall context name args =
  let argTys = args |> List.map (kiArg context)

  match context.TyMap.TryGetValue(name) with
  | true, KFunTy (paramList, KResult resultTy)
    when List.length paramList = List.length args ->

    // FIXME: 引数の型や渡し方を検査
    for argTy, KParam (_, _, paramTy) in List.zip argTys paramList do
      kiUnify context argTy paramTy

    resultTy

  | true, _ ->
    eprintfn "WARN: 関数ではないものを関数呼び出ししています %s" name
    KNeverTy

  | false, _ ->
    // 未定義の関数？
    KNeverTy

let kiPrim context prim args =
  let argTys = args |> List.map (kiArg context)

  match prim, argTys with
  | KBoolLiteralPrim _, _ ->
    KBoolTy

  | KIntLiteralPrim _, _ ->
    KIntTy

  | KStrLiteralPrim _, _ ->
    KStrTy

  | KEqPrim, [first; second] ->
    kiUnify context first second
    first

  | KAddPrim, [first; second] ->
    kiUnify context first second
    first

  | KAssignPrim, [first; second] ->
    kiUnify context first second
    first

  | KFnPrim name, _ ->
    kiCall context name args

  | KExternFnPrim name, _ ->
    KInferTy (name, ref None)

  | _ ->
    failwithf "unimpl %A" (prim, argTys)

let kiNode (context: KirInferContext) node =
  match node with
  | KNoop ->
    KNeverTy

  | KName name ->
    match context.TyMap.TryGetValue(name) with
    | true, ty ->
      ty

    | false, _ ->
      // 未定義の変数？
      KNeverTy

  | KPrim (prim, args, KParam (_, resultName, resultTy), next) ->
    // FIXME: モードを検査
    let ty = kiPrim context prim args
    kiUnify context ty resultTy

    context.TyMap <- context.TyMap |> Map.add resultName resultTy

    next |> kiNode context

  | KIf (cond, body, alt) ->
    cond |> kiNode context |> kiUnify context KBoolTy

    // 継続だからnever型？
    let bodyTy = kiNode context body
    let altTy = kiNode context alt
    kiUnify context bodyTy altTy

    bodyTy

  | KJump (KLabel name, args) ->
    kiCall context name args

  | KJump _ ->
    KNeverTy

  | KFix (funName, _, paramList, result, body, next) ->
    context.TyMap <- context.TyMap |> Map.add funName (KFunTy (paramList, result))

    let tyMap = context.TyMap
    body |> kiNode context |> ignore
    context.TyMap <- tyMap

    next |> kiNode context

let kirInfer (node: KNode) =
  let context = kiContextNew ()
  node |> kiNode context |> ignore
  node
