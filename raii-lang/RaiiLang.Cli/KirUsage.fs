module rec RaiiLang.KirUsage

open RaiiLang.Helpers
open RaiiLang.Kir

type KirUsageContext =
  {
    /// 現在位置における識別子の定義・使用の状態
    mutable Current: KUsage

    /// グローバル関数や定数など
    KnownSet: HashSet<KIdent>

    /// 関数ごとの定義・使用文脈
    CaptureMap: HashMap<KIdent, KUsage>
  }

// -----------------------------------------------
// KirUsageContext
// -----------------------------------------------

let kuContextNew (): KirUsageContext =
  {
    Current = kUsageEmpty ()
    KnownSet = HashSet()
    CaptureMap = HashMap()
  }

let kuContextAddDef ident (context: KirUsageContext) =
  context.Current <- context.Current |> kUsageAddDef ident

let kuContextAddUse ident (context: KirUsageContext) =
  context.Current <- context.Current |> kUsageAddUse ident

let kuContextAddKnown ident (context: KirUsageContext) =
  context.KnownSet.Add(ident) |> ignore

let kuContextSave funName (context: KirUsageContext) =
  context.CaptureMap.Add(funName, context.Current)

let kuContextGetCaptureMap funName (context: KirUsageContext) =
  let knownContext =
    match context.CaptureMap.TryGetValue(funName) with
    | true, knownContext ->
      knownContext

    | false, _ ->
      kUsageEmpty ()

  knownContext |> kUsageToCaptureMap context.KnownSet

/// 使用関係の推移閉包を取る。
let kuContextMakeFixPoint (context: KirUsageContext) =
  let mutable stuck = false
  let mutable updateList = ResizeArray()

  while not stuck do
    stuck <- true
    updateList.Clear()

    for KeyValue (fnName, usage) in context.CaptureMap do
      let mutable usage = usage
      let mutable modified = false

      for ident in usage.UseSet do
        let useSet = context |> kuContextGetCaptureMap ident
        if Set.isSubset useSet usage.UseSet |> not then
          usage <-
            { usage with
                UseSet = usage.UseSet |> Set.union useSet
            }
          modified <- true

      if modified then
        stuck <- false
        updateList.Add(fnName, usage)

    for fnName, usage in updateList do
      context.CaptureMap.[fnName] <- usage

// -----------------------------------------------
// Analyze
// -----------------------------------------------

let kuArg context (KArg (_, arg, _)) =
  context |> kuContextAddUse arg

let kuCont context cont =
  match cont with
  | KLabelCont (KLabel (fnName, _, _)) ->
    context |> kuContextAddUse fnName

  | KReturnCont _ ->
    ()

let kuNode context node =
  match node with
  | KNoop ->
    ()

  | KPrim (_, args, conts) ->
    for arg in args do
      arg |> kuArg context

    for cont in conts do
      cont |> kuCont context

  | KFix (fixes, next) ->
    let oldUsage = context.Current

    for fix in fixes do
      context.Current <- kUsageEmpty ()

      let fnName, paramList =
        match fix with
        | KLabelFix (KLabel (fnName, paramList, _)) ->
          fnName, paramList

        | KFnFix (KFn (fnName, paramList, _, _)) ->
          fnName, paramList

      for KParam (_, param, _) in paramList do
        context |> kuContextAddDef param

      context |> kuContextSave fnName

    context.Current <- oldUsage
    next |> kuNode context

let kirUsage (node: KNode) =
  let context = kuContextNew ()

  node |> kuNode context
  context |> kuContextMakeFixPoint
