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
    FnUsageMap: HashMap<KIdent, KUsage>
  }

// -----------------------------------------------
// KirUsageContext
// -----------------------------------------------

let kuContextNew (): KirUsageContext =
  {
    Current = kUsageEmpty ()
    KnownSet = HashSet()
    FnUsageMap = HashMap()
  }

let kuContextAddDef ident mode (context: KirUsageContext) =
  context.Current <- context.Current |> kUsageAddDef ident mode

let kuContextAddUse ident passBy (context: KirUsageContext) =
  context.Current <- context.Current |> kUsageAddUse ident passBy

let kuContextAddKnown ident (context: KirUsageContext) =
  context.KnownSet.Add(ident) |> ignore

let kuContextSave funName (context: KirUsageContext) =
  context.FnUsageMap.Add(funName, context.Current)

let kuContextGetFreeVars fnName (context: KirUsageContext) =
  let usage =
    match context.FnUsageMap.TryGetValue(fnName) with
    | true, usage ->
      usage

    | false, _ ->
      kUsageEmpty ()

  usage |> kUsageToFreeVars context.KnownSet

/// 自由変数の推移閉包を取る。
let kuContextMakeFixPoint (context: KirUsageContext) =
  let mutable stuck = false
  let mutable updateList = ResizeArray()

  while not stuck do
    stuck <- true
    updateList.Clear()

    for KeyValue (fnName, usage) in context.FnUsageMap do
      let mutable usage = usage
      let mutable usageModified = false

      for KeyValue (ident, _) in usage.Uses do
        let freeVars = context |> kuContextGetFreeVars ident

        let mutable uses = usage.Uses
        let mutable usesModified = false

        for ident, passByList in freeVars do
          match uses |> Map.tryFind ident with
          | Some _ ->
            ()

          | None ->
            usesModified <- true
            uses <- uses |> Map.add ident passByList

        if usesModified then
          usageModified <- true
          usage <- { usage with Uses = uses }

      if usageModified then
        stuck <- false
        updateList.Add(fnName, usage)

    for fnName, usage in updateList do
      context.FnUsageMap.[fnName] <- usage

// -----------------------------------------------
// Analyze
// -----------------------------------------------

let kuArg context (KArg (passBy, arg, _)) =
  context |> kuContextAddUse arg passBy

let kuCont context cont =
  match cont with
  | KLabelCont (KLabel (fnName, _, _)) ->
    context |> kuContextAddUse fnName ByMove // FIXME: by move?

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

      for KParam (mode, paramName, _) in paramList do
        context |> kuContextAddDef paramName mode

      context |> kuContextSave fnName

    context.Current <- oldUsage
    next |> kuNode context

let kirUsage (node: KNode) =
  let context = kuContextNew ()

  node |> kuNode context
  context |> kuContextMakeFixPoint
