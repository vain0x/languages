module rec RaiiLang.KirBorrowCheck

open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.KirUsage

type BorrowState =
  | ValState
  | MutState
  | InState
  | RefState
  | RefBorrowedState
  | MovedState

let borrowStateFromMode mode =
  match mode with
  | ValMode ->
    ValState

  | MutMode ->
    MutState

  | InMode ->
    InState

  | RefMode ->
    RefState

    // let mode = !modeOpt |> Option.defaultValue ValMode

    // // FIXME: copy な型なら val/ref, in/ref, ref/ref のみエラー

    // match mode, passBy with
    // | ValMode, ByMove
    // | MutMode, ByMove
    // | _, ByIn
    // | MutMode, ByRef
    // | RefMode, ByRef ->
    //   ()

    // | InMode, ByMove
    // | RefMode, ByMove ->
    //   failwithf "借用している値は move できません (変数・パラメータを in/ref 以外で宣言してください) %A" arg

    // | ValMode, ByRef
    // | InMode, ByRef ->
    //   failwithf "イミュータブルな変数は ref で渡せません (変数・パラメータを mut または ref で宣言してください) %A" arg

    // let state =
    //   match context.BorrowMap |> Map.tryFind arg with
    //   | None ->
    //     borrowStateFromMode mode

    //   | Some ByIn ->
    //     InState

    //   | Some ByRef ->
    //     RefBorrowedState

    //   | Some ByMove ->
    //     MovedState

    // match state, passBy with
    // | MovedState, _ ->
    //   failwithf "move された変数は使用できません %A" arg

    // | RefBorrowedState, _ ->
    //   // FIXME: 自由変数に arg が含まれていなければ
    //   failwithf "ref で渡した変数は使用できません %A" arg

    // | ValState, ByIn
    // | MutState, ByIn
    // | InState, ByIn
    // | RefState, ByIn
    // | InBorrowedState, ByIn
    // | MutState, ByRef
    // | RefState, ByRef
    // | ValState, ByMove
    // | MutState, ByMove ->
    //   ()

let kbcNode context usage node =
  match node with
  | KNoop _ ->
    ()

  | KPrim (_, args, conts) ->
    let mutable freeVars = ResizeArray()

    for cont in conts do
      match cont with
      | KLabelCont (KLabel (fnName, _, _)) ->
        match context.FnUsageMap.TryGetValue(fnName) with
        | true, usage ->
          freeVars.AddRange(usage.Uses |> Map.toSeq |> Seq.map fst)

        | false, _ ->
          ()

      | KReturnCont _ ->
        ()

    let freeVars = set freeVars

    for KArg (passBy, arg, _) in args do
      printfn "arg(%A %s) -> %A" passBy arg freeVars

  | KFix (fixes, next) ->
    for fix in fixes do
      let fnName, body =
        match fix with
        | KLabelFix (KLabel (fnName, _, body)) ->
          fnName, !body

        | KFnFix (KFn (fnName, _, _, body)) ->
          fnName, !body

      match context.FnUsageMap.TryGetValue(fnName) with
      | true, usage ->
        body |> kbcNode context usage

      | false, _ ->
        assert false
        ()

    next |> kbcNode context usage

let kirBorrowCheck (node: KNode) =
  let context = node |> kirUsage
  node |> kbcNode context (kUsageEmpty ())
  node
