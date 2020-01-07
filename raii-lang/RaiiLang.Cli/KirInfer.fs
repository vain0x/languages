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

let kiName context name =
  match context.TyMap.TryGetValue(name) with
  | true, ty ->
    kTyDeref ty

  | false, _ ->
    // FIXME: 未定義の変数？
    KNeverTy

let kiLabel context label =
  match label with
  | KLabel label ->
    kiName context label

  | KReturnLabel ->
    // FIXME: 現在の関数の結果型を引数とする関数型
    KNeverTy

let kiArg context arg =
  match arg with
  | KArg (passBy, arg) ->
    let arg = kiName context arg
    passBy, arg

let kiCall context funTy args =
  // 渡された引数から関数型を逆算する。
  let paramList =
    args |> List.map (fun (passBy, argTy) ->
      KParam (passByToMode passBy, "_", argTy)
    )
  let result =
    KInferTy ("result", ref None)
    |> KResult
  let expectedTy =
    KFunTy (paramList, result)

  // 呼び出し先の関数の型と単一化する。
  kiUnify context funTy expectedTy

  expectedTy

let kiJump context label args =
  let labelTy = kiLabel context label
  kiCall context labelTy args

let kiPrim context prim args next =
  let args = args |> List.map (kiArg context)

  let unifyNext funTy =
    match funTy with
    | KFunTy (_, KResult resultTy) ->
      let labelTy = kiLabel context next
      kiUnify context labelTy (
        KFunTy (
          [KParam (MutMode, "result", resultTy)],
          KResult (KInferTy ("_", ref None))
        ))

    | _ ->
      ()

  // 1個の結果と1個の継続を持つケース
  let onPrim1 paramList result =
    let funTy = KFunTy (paramList, result)

    // 引数の型の推論
    kiCall context funTy args |> ignore

    // 継続の型の推論
    unifyNext funTy

    funTy

  let onBin funTyFun =
    match args with
    | [(_, first); (_, second)] ->
      let paramList, result = funTyFun first second
      onPrim1 paramList result

    | _ ->
      failwithf "NEVER: 二項演算の引数は2個"

  match prim with
  | KBoolLiteralPrim _ ->
    onPrim1 [] (KResult KBoolTy)

  | KIntLiteralPrim _ ->
    onPrim1 [] (KResult KIntTy)

  | KStrLiteralPrim _ ->
    onPrim1 [] (KResult KStrTy)

  | KEqPrim ->
    onBin (fun first second ->
      kiUnify context first second
      (
        [
          KParam (InMode, "first", first)
          KParam (InMode, "second", second)
        ],
        KResult KBoolTy
      ))

  | KAddPrim ->
    onBin (fun first second ->
      kiUnify context first second
      (
        [
          KParam (ValMode, "first", first)
          KParam (ValMode, "second", second)
        ],
        KResult first
      ))

  | KAssignPrim ->
    onBin (fun first second ->
      kiUnify context first second
      (
        [
          KParam (InMode, "first", first)
          KParam (InMode, "second", second)
        ],
        KResult KUnitTy
      ))

  | KJumpPrim ->
    kiJump context next []

  | KFnPrim name ->
    let funTy = kiName context name
    unifyNext funTy
    kiCall context funTy args

  | KExternFnPrim name ->
    let funTy = KInferTy (name, ref None)
    unifyNext funTy
    kiCall context funTy args

let kiNode (context: KirInferContext) node =
  match node with
  | KName name ->
    kiName context name

  | KPrim (prim, args, next) ->
    kiPrim context prim args next

  | KIf (cond, body, alt) ->
    KName cond |> kiNode context |> kiUnify context KBoolTy

    // 継続だからnever型？
    let bodyTy = kiNode context body
    let altTy = kiNode context alt
    kiUnify context bodyTy altTy

    bodyTy

  | KFix (funName, _, paramList, result, body, next) ->
    context.TyMap <- context.TyMap |> Map.add funName (KFunTy (paramList, result))

    let tyMap = context.TyMap

    for KParam (_, paramName, paramTy) in paramList do
      context.TyMap <- context.TyMap |> Map.add paramName paramTy

    body |> kiNode context |> ignore

    context.TyMap <- tyMap

    next |> kiNode context

let kirInfer (node: KNode) =
  let context = kiContextNew ()
  node |> kiNode context |> ignore
  node
