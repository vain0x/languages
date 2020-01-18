module rec PhancieLang.KirInfer

open PhancieLang.Helpers
open PhancieLang.Kir

type KirInferContext =
  {
    mutable SymbolMap: Map<string, struct (Mode * KTy)>
  }

let kiContextNew (): KirInferContext =
  {
    SymbolMap = Map.empty
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
        kiUnify context first second

      match firstResult, secondResult with
      | KResult first,
        KResult second ->
        kiUnify context first second
    else
      eprintfn "WARN: Type error %A" (first, second)

  | _ ->
    eprintfn "WARN: Type error %A" (first, second)

let kiName context name =
  match context.SymbolMap.TryGetValue(name) with
  | true, (mode, ty) ->
    mode, kTyDeref ty

  | false, _ ->
    // FIXME: () の代わり
    if name = "0" then
      ValMode, KUnitTy
    else
      // FIXME: 未定義の変数？
      ValMode, KNeverTy

let kiArg context arg =
  match arg with
  | KArg (passBy, arg, modeOpt) ->
    let mode, arg = kiName context arg
    modeOpt := Some mode
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

let kiJump context cont args =
  let nextTy = kiCont context cont
  kiCall context nextTy args

let kiPrim context prim args conts =
  let args = args |> List.map (kiArg context)

  let unifyNext funTy cont =
    match funTy with
    | KFunTy (_, KResult resultTy) ->
      // FIXME: モード
      let nextTy = kiCont context cont
      kiUnify context nextTy (
        KFunTy (
          [KParam (MutMode, "result", resultTy)],
          KResult (KInferTy ("_", ref None))
        ))

    | _ ->
      failwithf "ERROR: 継続の型が一致しません %A" (prim, args, cont)

  // 1個の結果と1個の継続を持つケース
  let onPrim1 paramList result =
    match conts with
    | [cont] ->
      let funTy = KFunTy (paramList, result)

      // 引数の型の推論
      kiCall context funTy args

      // 継続の型の推論
      unifyNext funTy cont

    | _ ->
      failwithf "ERROR: 継続は1個 %A" (conts)

  let onBin funTyFun =
    match args with
    | [(_, first); (_, second)] ->
      let paramList, result = funTyFun first second
      onPrim1 paramList result

    | _ ->
      failwithf "NEVER: 二項演算の引数は2個 %A" args

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
    match conts with
    | [cont] ->
      kiJump context cont args

    | _ ->
      failwithf "ERROR: 継続は1個 %A" conts

  | KIfPrim ->
    match args, conts with
    | [(_, condTy)], [body; alt] ->
      kiUnify context condTy KBoolTy

      let resultTy = KInferTy ("result", ref None)
      let contTy = KFunTy ([KParam (MutMode, "result", resultTy)], KResult KNeverTy)
      unifyNext contTy body
      unifyNext contTy alt

    | _ ->
      failwithf "ERROR: 引数は1個、継続は2個 %A" (args, conts)

  | KFnPrim (name, _) ->
    match conts with
    | [cont] ->
      // FIXME: 名前解決
      let _, funTy = kiName context name
      unifyNext funTy cont
      kiCall context funTy args

    | _ ->
      failwithf "ERROR: 継続は1個 %A" conts

  | KExternFnPrim externFn ->
    match conts with
    | [cont] ->
      let funTy = kiExternFn context externFn
      unifyNext funTy cont
      kiCall context funTy args

    | _ ->
      failwithf "ERROR: 継続は1個 %A" conts

let kiNode (context: KirInferContext) node =
  match node with
  | KNoop ->
    ()

  | KPrim (prim, args, conts) ->
    kiPrim context prim args conts

  | KFix (fixes, next) ->
    // 相互再帰のため、まず各関数を識別子として定義する。
    for fix in fixes do
      let name, paramList, result =
        match fix with
        | KLabelFix (KLabel (name, paramList, _)) ->
          name, paramList, KResult KNeverTy

        | KFnFix (KFn (name, paramList, result, _)) ->
          name, paramList, result

      context.SymbolMap <- context.SymbolMap |> Map.add name (struct (MutMode, KFunTy (paramList, result)))

    let oldMap = context.SymbolMap

    // 各関数の本体を処理する。
    for fix in fixes do
      let paramList, body =
        match fix with
        | KLabelFix (KLabel (_, paramList, body)) ->
          paramList, !body

        | KFnFix (KFn (_, paramList, _, body)) ->
          paramList, !body

      // 外側の環境に引数を付加したもの。
      let mutable newMap = oldMap
      for KParam (mode, paramName, paramTy) in paramList do
        newMap <- newMap |> Map.add paramName (mode, paramTy)

      context.SymbolMap <- newMap
      body |> kiNode context

    context.SymbolMap <- oldMap
    next |> kiNode context

let kirInfer (node: KNode) =
  let context = kiContextNew ()
  node |> kiNode context
  node
