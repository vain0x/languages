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

let kiArg context arg =
  match arg with
  | KArg (passBy, arg) ->
    let arg = kiName context arg
    passBy, arg

let kiLabel _context label =
  match label with
  | KLabel (_, paramList, _) ->
    KFunTy (paramList, KResult KNeverTy)

let kiFn _context fn =
  match fn with
  | KFn (_, paramList, result, _) ->
    KFunTy (paramList, result)

let kiExternFn _context externFn =
  match externFn with
  | KExternFn (_, paramList, result) ->
    KFunTy (paramList, result)

let kiFix context fix =
  match fix with
  | KLabelFix label ->
    kiLabel context label

  | KFnFix fn ->
    kiFn context fn

let kiCont context cont =
  match cont with
  | KLabelCont label ->
    kiLabel context label

  | KReturnCont (KFn (_, _, KResult resultTy, _)) ->
    // FIXME: モード
    KFunTy ([KParam (MutMode, "result", resultTy)], KResult KNeverTy)

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

let kiJump context cont args =
  let nextTy = kiCont context cont
  kiCall context nextTy args

let kiPrim context prim args next =
  let args = args |> List.map (kiArg context)

  let unifyNext funTy =
    match funTy with
    | KFunTy (_, KResult resultTy) ->
      // FIXME: モード
      let nextTy = kiCont context next
      kiUnify context nextTy (
        KFunTy (
          [KParam (MutMode, "result", resultTy)],
          KResult (KInferTy ("_", ref None))
        ))

    | _ ->
      failwithf "ERROR: 継続の型が一致しません %A" (prim, args, next)

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
    kiJump context next args

  | KFnPrim (name, _) ->
    // FIXME: 名前解決
    let funTy = kiName context name
    unifyNext funTy
    kiCall context funTy args

  | KExternFnPrim externFn ->
    let funTy = kiExternFn context externFn
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

    kiNode context body |> ignore
    kiNode context alt |> ignore
    KNeverTy

  | KFix (fix, next) ->
    let name, paramList, result, body =
      match fix with
      | KLabelFix (KLabel (name, paramList, body)) ->
        name, paramList, KResult KNeverTy, !body

      | KFnFix (KFn (name, paramList, result, body)) ->
        name, paramList, result, !body

    context.TyMap <- context.TyMap |> Map.add name (KFunTy (paramList, result))

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
