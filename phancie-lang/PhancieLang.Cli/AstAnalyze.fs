/// 名前解決や型検査など
module rec PhancieLang.AstAnalyze

open PhancieLang.Ast
open PhancieLang.Helpers
open PhancieLang.Syntax

type AstAnalyzeContext =
  {
    FreshName: string -> string
    Symbols: Map<string, ASymbol> ref
    Loops: ResizeArray<ALoop>
    Fns: ResizeArray<AFn>
    Errors: ResizeArray<AError>
  }

let aaContextNew (): AstAnalyzeContext =
  {
    FreshName = freshNameFun ()
    Symbols = ref Map.empty
    Loops = ResizeArray()
    Fns = ResizeArray()
    Errors = ResizeArray()
  }

let aaContextResolveLocal name (context: AstAnalyzeContext) =
  !context.Symbols |> Map.tryFind name

let aaContextToCurrentLoop (context: AstAnalyzeContext) =
  let n = context.Loops.Count
  if n = 0 then
    None
  else
    context.Loops.[n - 1] |> Some

let aaContextToCurrentFn (context: AstAnalyzeContext) =
  let n = context.Fns.Count
  if n = 0 then
    None
  else
    context.Fns.[n - 1] |> Some

/// 型検査。first が second の型に適合すれば OK。
let aaContextCheckTy (first: ATyInfo) (second: ATyInfo) (context: AstAnalyzeContext) =
  let rec go first second =
    match first, second with
    | ANameTy (name, _, _),
      ANameTy (secondName, _, _)
      when name = secondName ->
      ()

    | ANameTy (_, symbolSlot, _), _ ->
      match !symbolSlot with
      | Some symbol ->
        go (symbol |> aSymbolToTy) second

      | None ->
        ()

    | _, ANameTy (_, secondSymbolSlot, _) ->
      match !secondSymbolSlot with
      | Some secondSymbol ->
        go first (secondSymbol |> aSymbolToTy)

      | None ->
        ()

    | ABoolTy _, ABoolTy _
    | AIntTy _, AIntTy _
    | AStringTy _, AStringTy _ ->
      ()

    | AFnTy (paramList, resultTy, syn),
      AFnTy (secondParamList, secondResultTy, _) ->
      go resultTy secondResultTy

      if List.length paramList <> List.length secondParamList then
        context |> aaContextAddError (AError ("パラメータ数が一致しません", syn))
      else
        for param, secondParam in List.zip paramList secondParamList do
          match param, secondParam with
          | AParamTy (mode, paramTy),
            AParamTy (secondMode, _)
            when modeIsDerivedFrom mode secondMode |> not ->
            let syn = paramTy |> aTyToSyn
            context |> aaContextAddError (AError ("モードが適合しません", syn))

          | AParamTy (_, paramTy),
            AParamTy (_, secondParamTy) ->
            go paramTy secondParamTy

    | ABoolTy _, _
    | AIntTy _, _
    | AStringTy _, _
    | AFnTy _, _ ->
      context |> aaContextAddError (ATyMismatchError (first, second))

  go first second

/// 引数渡しのモード・型検査。
let aaContextCheckPass argTy paramTy context =
  match argTy, paramTy with
  | AArgTy (passBy, argTy),
    AParamTy (mode, paramTy) ->
    if passByIsFor mode passBy |> not then
      let syn = argTy |> aTyToSyn
      context |> aaContextAddError (APassByMismatchError (mode, passBy, syn))

    context |> aaContextCheckTy paramTy argTy

let aaContextCheckPassList argTys paramTys syn context =
  if List.length argTys <> List.length paramTys then
    context |> aaContextAddError (AError ("引数の個数が一致しません", syn))
  else
    for argTy, paramTy in List.zip argTys paramTys do
      context |> aaContextCheckPass argTy paramTy

let aaContextAddError error (context: AstAnalyzeContext) =
  context.Errors.Add(error)

let aaContextAddSymbol name symbol (context: AstAnalyzeContext) =
  context.Symbols := !context.Symbols |> Map.add name symbol

let aaContextEnterScope bodyFn (context: AstAnalyzeContext) =
  let innerContext = { context with Symbols = ref (!context.Symbols) }
  bodyFn innerContext

let aaContextEnterFn bodyFn (context: AstAnalyzeContext) =
  let innerContext =
    {
      context with
        Loops = ResizeArray()
        Symbols = ref !context.Symbols
    }

  let result = bodyFn innerContext
  assert (innerContext.Loops.Count = 0)

  result

let aaName context name =
  match name with
  | AName (Some name, symbolSlot, syn) ->
    match context |> aaContextResolveLocal name with
    | Some symbol ->
      symbolSlot := Some symbol
      symbol |> aSymbolToTy

    | None ->
      context |> aaContextAddError (AError ("未定義の名前", syn))
      aErrorTy syn

  | AName (None, _, syn) ->
    aErrorTy syn

let aaTy context ty =
  match ty with
  | ATy (Some (AName (Some name, symbolSlot, syn)), _) ->
    let ty =
      match name with
      | "bool" ->
        ABoolTy syn

      | "int" ->
        AIntTy syn

      | "string" ->
        AStringTy syn

      | _ ->
        context |> aaContextAddError (AError ("未定義の型", syn))
        aErrorTy syn

    symbolSlot := Some (ATySymbol (name, ty))
    ty

  | ATy (_, syn) ->
    aErrorTy syn

let aaParam context param argTyOpt =
  match param with
  | AParam (mode, nameOpt, tyOpt, syn) ->
    let paramTyOpt =
      match tyOpt with
      | Some ty ->
        let ty = aaTy context ty
        Some (AParamTy (mode, ty))

      | None ->
        None

    let paramTy =
      match paramTyOpt, argTyOpt with
      | Some paramTy, Some argTy ->
        context |> aaContextCheckPass argTy paramTy
        paramTy

      | Some paramTy, None ->
        paramTy

      | None, Some (AArgTy (_, argTy)) ->
        AParamTy (mode, argTy)

      | None, None ->
        AParamTy (mode, aErrorTy syn)

    let ty =
      match paramTy with
      | AParamTy (_, ty) ->
        ty

    match nameOpt with
    | Some (AName (nameOpt, symbolSlot, _)) ->
      let name = nameOpt |> Option.defaultValue "???"

      let symbol = ALocalSymbol (name, mode, ty)
      context |> aaContextAddSymbol name symbol
      symbolSlot := Some symbol

    | None ->
      ()

    paramTy

let aaParamList context paramList =
  paramList |> List.map (fun param -> aaParam context param None)

let aaArg context arg =
  match arg with
  | AArg (passBy, Some arg, _) ->
    let argTy = aaTerm context arg
    AArgTy (passBy, argTy)

  | AArg (passBy, None, syn) ->
    AArgTy (passBy, aErrorTy syn)

let aaArgList context args =
  args |> List.map (aaArg context)

let aaFnStmt (context: AstAnalyzeContext) kind name paramList resultOpt bodyOpt fnSlot syn =
  let fnNameOpt, symbolSlot =
    match name with
    | Some (AName (fnNameOpt, symbolSlot, _)) ->
      fnNameOpt, symbolSlot

    | _ ->
      None, ref None

  let fnName = fnNameOpt |> Option.defaultValue "__fn"

  context |> aaContextEnterFn (fun context ->
    let paramTys =
      aaParamList context paramList

    let resultTy =
      match resultOpt with
      | Some (AResult (Some ty, _)) ->
        aaTy context ty

      | _->
        aUnitTy syn

    let fn: AFn =
      {
        Name = fnName
        Kind = kind
        Params = paramList
        ResultOpt = resultOpt
        ParamTys = paramTys
        ResultTy = resultTy
        Node = syn
      }
    fnSlot := Some fn
    context.Fns |> vecPush fn

    let symbol = AFnSymbol fn
    symbolSlot := Some symbol
    context |> aaContextAddSymbol fnName symbol

    let bodyTy =
      match bodyOpt with
      | Some body ->
        aaTerm context body

      | None ->
        aNeverTy syn

    context |> aaContextCheckTy bodyTy resultTy

    context.Fns |> vecPop |> ignore
  )

  let symbol = !symbolSlot |> Option.get
  context |> aaContextAddSymbol fnName symbol

  aUnitTy syn

let aaTerm context term =
  match term with
  | ABoolLiteral (_, syn) ->
    ABoolTy syn

  | AIntLiteral (_, syn) ->
    AIntTy syn

  | AStrLiteral (_, syn) ->
    AStringTy syn

  | ANameTerm name ->
    aaName context name

  | AGroupTerm (bodyOpt, syn) ->
    match bodyOpt with
    | Some body ->
      aaTerm context body

    | None ->
      aUnitTy syn

  | ABlockTerm (stmts, syn) ->
    context |> aaContextEnterScope (fun context ->
      aaStmts context stmts syn
    )

  | ABreakTerm (loopSlot, syn) ->
    let loopOpt = context |> aaContextToCurrentLoop
    loopSlot := loopOpt
    aNeverTy syn

  | AContinueTerm (loopSlot, syn) ->
    let loopOpt = context |> aaContextToCurrentLoop
    loopSlot := loopOpt
    aNeverTy syn

  | ALoopTerm (bodyOpt, loopSlot, syn) ->
    let loop: ALoop =
      {
        LoopOpt = Some term
        FnOpt = context |> aaContextToCurrentFn
      }
    loopSlot := Some loop
    context.Loops |> vecPush loop

    match bodyOpt with
    | Some body ->
      aaTerm context body |> ignore

    | None ->
      ()

    context.Loops |> vecPop |> ignore

    // FIXME: loop の結果は break の引数型による
    let resultTy = aNeverTy syn

    resultTy

  | ACallTerm (calOpt, args, syn) ->
    let calTy =
      match calOpt with
      | Some cal ->
        aaTerm context cal

      | None ->
        aNeverTy syn

    let argTys =
      aaArgList context args

    let paramTys = calTy |> aTyToParamTys
    context |> aaContextCheckPassList argTys paramTys syn

    match calTy |> aTyToResultTy with
    | Some ty ->
      ty

    | None ->
      aErrorTy syn

  | ABinTerm (binOpt, firstOpt, secondOpt, syn) ->
    match binOpt, firstOpt, secondOpt with
    | Some bin, Some first, Some second ->
      let firstTy = aaTerm context first
      let secondTy = aaTerm context second

      let firstParam, secondParam, resultTy =
        aBinToSig bin firstTy secondTy syn

      let check argTy paramTy =
        match paramTy with
        | AParamTy (mode, _) ->
          let argTy = AArgTy (modeToPassBy mode, argTy)
          context |> aaContextCheckPass argTy paramTy

      check firstTy firstParam
      check secondTy secondParam
      resultTy

    | _ ->
      aNeverTy syn

  | AIfTerm (condOpt, bodyOpt, altOpt, tySlot, syn) ->
    match condOpt with
    | Some cond ->
      let condTy = aaTerm context cond
      context |> aaContextCheckTy condTy (ABoolTy syn)

    | None ->
      ()

    let bodyTy =
      match bodyOpt with
      | Some body ->
        aaTerm context body

      | None ->
        aUnitTy syn

    let altTy =
      match altOpt with
      | Some alt ->
        aaTerm context alt

      | None ->
        aUnitTy syn

    context |> aaContextCheckTy altTy bodyTy
    tySlot := Some bodyTy

    bodyTy

  | AWhileTerm (condOpt, bodyOpt, loopSlot, syn) ->
    match condOpt with
    | Some cond ->
      let condTy = aaTerm context cond
      context |> aaContextCheckTy condTy (ABoolTy syn)

    | None ->
      ()

    let loop: ALoop =
      {
        LoopOpt = Some term
        FnOpt = context |> aaContextToCurrentFn
      }
    loopSlot := Some loop
    context.Loops |> vecPush loop

    match bodyOpt with
    | Some body ->
      aaTerm context body |> ignore

    | None ->
      ()

    context.Loops |> vecPop |> ignore

    aUnitTy syn

let aaStmt context stmt =
  match stmt with
  | ATermStmt (bodyOpt, syn) ->
    match bodyOpt with
    | Some body ->
      aaTerm context body

    | None ->
      aUnitTy syn

  | ALetStmt (paramOpt, argOpt, syn) ->
    let argTy =
      match argOpt with
      | Some arg ->
        aaArg context arg

      | None ->
        AArgTy (ByMove, aErrorTy syn)

    let paramTy =
      match paramOpt with
      | Some param ->
        aaParam context param (Some argTy)

      | None ->
        AParamTy (ValMode, aErrorTy syn)

    context |> aaContextCheckPass argTy paramTy

    aUnitTy syn

  | AExternFnStmt (name, paramList, resultOpt, fnSlot, syn) ->
    let kind = AExternFnKind
    aaFnStmt context kind name paramList resultOpt None fnSlot syn

  | AFnStmt (name, paramList, resultOpt, bodyOpt, fnSlot, syn) ->
    let kind = AFnKind bodyOpt
    aaFnStmt context kind name paramList resultOpt bodyOpt fnSlot syn

  | AStructStmt (name, paramList, syn) ->
    aUnitTy syn

  | ASemiStmt (stmts, syn) ->
    aaStmts context stmts syn

let aaStmts context stmts syn =
  match stmts with
  | [] ->
    aUnitTy syn

  | [stmt] ->
    aaStmt context stmt

  | stmt :: stmts ->
    aaStmt context stmt |> ignore
    aaStmts context stmts syn

let astAnalyze (stmt: AStmt) =
  let context = aaContextNew ()
  aaStmt context stmt |> ignore
