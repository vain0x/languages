module rec Linear.TypeCheck

open Linear.Ast
open Linear.Location
open Linear.TypedIR

exception TyError of message: string * range: Range with
  override this.ToString() =
    $"Type error: {this.range} {this.message}"

type TSymbol = TSymbolKind * TName

let private unreachable () : 'A =
  assert false
  failwith "unreachable"

let private unwrap (opt: option<'T>) : 'T = Option.get opt

let private identOf (name: AName) =
  let (AName (_, ident, _)) = name
  ident

let private posOf (name: AName) =
  let (AName (_, _, pos)) = name
  pos.Start

let private idOf (name: TName) =
  let (TName (_, id, _)) = name
  id

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private TValueSymbol =
  | Prim of TPrim
  | Var of TName
  | Fun of TName
  | NewtypeVariant of variantName: TName * unionName: TName

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private TTySymbol =
  | Int
  | Unit
  | Bool
  | NewtypeUnion of TName

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private TyCheckState =
  { SymbolTys: Map<Id, TTy>
    LinearUnions: Set<Id>
    /// Linear values: id -> (true if alive)
    Linearity: Map<Id, bool>
    ValueEnv: Map<TIdent, TValueSymbol>
    TyEnv: Map<TIdent, TTySymbol> }

type private Sx = TyCheckState

let private fail message (pos: Pos) : 'A =
  raise (TyError(message, Range.ofPos pos))

let private fail2 message range : 'A = raise (TyError(message, range))

let private emptyState () : TyCheckState =
  { SymbolTys = Map.empty
    LinearUnions = Set.empty
    Linearity = Map.empty
    ValueEnv =
      Map.ofList [ "assert", TValueSymbol.Prim TPrim.Assert
                   "__acquire", TValueSymbol.Prim TPrim.UUAcquire
                   "__dispose", TValueSymbol.Prim TPrim.UUDispose ]
    TyEnv =
      Map.ofList [ "int", TTySymbol.Int
                   "unit", TTySymbol.Unit
                   "bool", TTySymbol.Bool ] }

let private isLinearTy (state: Sx) ty =
  match ty with
  | TLinearTy _ -> true
  | TUnionTy name -> state.LinearUnions |> Set.contains (idOf name)
  | TPairTy (lTy, rTy) -> isLinearTy state lTy || isLinearTy state rTy

  | TIntTy
  | TUnitTy
  | TBoolTy
  | TFunTy _ -> false

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Branch =
  | Start of parent: Map<Id, bool>
  | Next of parent: Map<Id, bool> * prev: Map<Id, bool>

let private startBranch (state: Sx) : Branch * Sx = (Branch.Start state.Linearity), state

let private nextBranch branch (state: Sx) : Branch * Sx =
  let parent, prevOpt =
    match branch with
    | Branch.Start parent -> parent, None
    | Branch.Next (parent, prev) -> parent, Some prev

  let delta =
    state.Linearity
    |> Map.fold
         (fun delta id alive ->
           match parent |> Map.tryFind id with
           | Some parentAlive ->
             if parentAlive <> alive then
               //  printfn "debug: id=%d parent %A alive=%A" id parentAlive alive
               assert (parentAlive && not alive)
               (id, false) :: delta
             else
               delta
           | None -> (id, alive) :: delta)
         []

  // check matching
  match prevOpt with
  | Some prev ->
    let rest =
      delta
      |> List.fold
           (fun prev (id, alive) ->
             match prev |> Map.tryFind id with
             | Some prevAlive when alive = prevAlive -> prev |> Map.remove id
             | _ -> prev)
           prev

    if rest |> Map.exists (fun _ alive -> alive) then
      // eprintfn "debug: prev=%A, delta=%A rest=%A" prev delta rest
      fail "linearity violation" Pos.zero

  | None -> ()

  let branch =
    match branch with
    | Branch.Start _ -> Branch.Next(parent, Map.ofList delta)
    | _ -> branch

  branch, { state with Linearity = parent }

let private endBranch branch (state: Sx) : Sx =
  let linearity = state.Linearity
  let _, state = nextBranch branch state
  { state with Linearity = linearity }

// -----------------------------------------------
// Name resolution
// -----------------------------------------------

let private resolveValueSymbol (state: Sx) name =
  match state.ValueEnv |> Map.tryFind (identOf name) with
  | Some symbol -> symbol
  | _ -> fail "Undefined value" (posOf name)

let private resolveTySymbol (state: Sx) name =
  match state.TyEnv |> Map.tryFind (identOf name) with
  | Some symbol -> symbol
  | _ -> fail "Undefined type" (posOf name)

let private resolveAscribedTy (state: Sx) (ty: ATy) =
  match ty with
  | ANameTy name ->
    match resolveTySymbol state name with
    | TTySymbol.Int -> TIntTy
    | TTySymbol.Unit -> TUnitTy
    | TTySymbol.Bool -> TBoolTy
    | TTySymbol.NewtypeUnion tName -> TUnionTy tName

  | APairTy (lTy, rTy) ->
    let lTy = resolveAscribedTy state lTy
    let rTy = resolveAscribedTy state rTy
    TPairTy(lTy, rTy)

  | AFunTy (lTy, rTy) ->
    let lTy = resolveAscribedTy state lTy
    let rTy = resolveAscribedTy state rTy
    TFunTy(lTy, rTy)

  | ALinearTy itemTy -> TLinearTy(resolveAscribedTy state itemTy)

// -----------------------------------------------
// Misc
// -----------------------------------------------

let private genName (name: AName) : TName =
  let (AName (_, ident, range)) = name
  let pos = range.Start
  let id = (pos.Row <<< 16) ||| pos.Column
  TName(ident, id, pos)

// -----------------------------------------------
// Type Unification
// -----------------------------------------------

let private unify pos lTy rTy : unit =
  let fail () : 'A =
    raise (TyError($"can't unify {lTy}, {rTy}", Range.ofPos pos))

  let children ty =
    match ty with
    | TPairTy (lTy, rTy) -> [ lTy; rTy ]
    | TFunTy (lTy, rTy) -> [ lTy; rTy ]
    | TLinearTy itemTy -> [ itemTy ]

    | TIntTy
    | TBoolTy
    | TUnitTy
    | TUnionTy _ -> []

  let rec go lTy rTy =
    match lTy, rTy with
    | TIntTy, TIntTy
    | TBoolTy, TBoolTy
    | TUnitTy, TUnitTy -> ()

    | TUnionTy l, TUnionTy r -> if idOf l <> idOf r then fail ()

    | TPairTy _, TPairTy _
    | TFunTy _, TFunTy _
    | TLinearTy _, TLinearTy _ ->
      List.zip (children lTy) (children rTy)
      |> List.iter (fun (lTy, rTy) -> go lTy rTy)

    | TIntTy, _
    | TBoolTy, _
    | TUnitTy, _
    | TUnionTy _, _
    | TPairTy _, _
    | TFunTy _, _
    | TLinearTy _, _ -> fail ()

  go lTy rTy

let private unify2 (range: Range) lTy rTy : unit =
  try
    unify range.Start lTy rTy
  with
  | :? TyError as ex -> raise (TyError(ex.message, range))

// -----------------------------------------------
// Type Check Patterns
// -----------------------------------------------

let private tcPat targetTy (state: Sx) (pat: APat) : TPat * Sx =
  match pat with
  | ANamePat name ->
    let tName = genName name

    let state =
      { state with
          ValueEnv =
            state.ValueEnv
            |> Map.add (identOf name) (TValueSymbol.Var tName)
          SymbolTys = state.SymbolTys |> Map.add (idOf tName) targetTy
          Linearity =
            if isLinearTy state targetTy then
              printfn "trace: linear var %s defined at %A" (identOf name) (posOf name)
              state.Linearity |> Map.add (idOf tName) true
            else
              state.Linearity }

    TVarPat(tName, targetTy), state

  | AWildcardPat pos ->
    if isLinearTy state targetTy then
      fail "Linear value can't be bound to wildcard" pos

    TWildcardPat(targetTy, pos), state

  | AUnitPat pos ->
    match targetTy with
    | TUnitTy -> TUnitPat pos, state
    | _ -> fail "Expected unit" pos

  | APairPat (lPat, rPat, pos) ->
    match targetTy with
    | TPairTy (lTy, rTy) ->
      let lPat, state = tcPat lTy state lPat
      let rPat, state = tcPat rTy state rPat
      TPairPat(lPat, rPat), state

    | _ -> fail "Expected pair type" pos

  | AWrapPat (name, payloadPat) ->
    let variantName, unionName =
      match resolveValueSymbol state name with
      | TValueSymbol.NewtypeVariant (variantName, unionName) -> variantName, unionName
      | _ -> failwithf "Expected variant name %A" (posOf name)

    match targetTy with
    | TUnionTy u when idOf u = idOf unionName -> ()
    | _ -> fail "Expected union" (posOf name)

    let defPayloadTy =
      let variantTy =
        state.SymbolTys
        |> Map.tryFind (idOf variantName)
        |> unwrap

      match variantTy with
      | TFunTy (payloadTy, _) -> payloadTy
      | _ -> unreachable ()

    let payloadPat, state = tcPat defPayloadTy state payloadPat
    TWrapPat(variantName, payloadPat), state

// -----------------------------------------------
// Type Check Expressions
// -----------------------------------------------

let private exprToTy (expr: TExpr) : TTy =
  match expr with
  | TIntExpr _ -> TIntTy
  | TBoolExpr _ -> TBoolTy
  | TUnitExpr _ -> TUnitTy
  | TSymbolExpr (_, _, ty, _) -> ty

  | TAppExpr (lExpr, _) ->
    match exprToTy lExpr with
    | TFunTy (_, rTy) -> rTy
    | _ -> unreachable ()

  | TUnaryExpr (unary, arg, _) ->
    match unary with
    | TUnary.Assert -> TUnitTy
    | TUnary.UUAcquire -> TLinearTy(exprToTy arg)

    | TUnary.UUDispose ->
      match exprToTy arg with
      | TLinearTy itemTy -> itemTy
      | _ -> unreachable ()

  | TBinaryExpr (binary, lExpr, rExpr) ->
    match binary with
    | TBinary.Add -> TIntTy
    | TBinary.Equal -> TBoolTy
    | TBinary.Pair -> TPairTy(exprToTy lExpr, exprToTy rExpr)

  | TIfExpr (_, thenClause, _) -> exprToTy thenClause
  | TLetExpr _ -> TUnitTy
  | TBlockExpr (_, last) -> exprToTy last

let private rangeOf (expr: AExpr) : Range =
  match expr with
  | AIntExpr (_, range) -> range
  | AUnitExpr range -> range
  | ANameExpr (AName (_, _, range)) -> range
  | AAppExpr (_, _, pos) -> Range.ofPos pos
  | ABinaryExpr (_, lExpr, rExpr) -> Range.join (rangeOf lExpr) (rangeOf rExpr)
  | AIfExpr (_, _, _, range) -> range
  | ALetExpr (_, _, range) -> range
  | ABlockExpr (_, _, range) -> range

let private tcNameExpr state name : TExpr * Sx =
  match resolveValueSymbol state name with
  | TValueSymbol.Var varName ->
    let defTy =
      state.SymbolTys
      |> Map.tryFind (idOf varName)
      |> unwrap

    let state =
      match state.Linearity |> Map.tryFind (idOf varName) with
      | Some true ->
        printfn "trace: linear var %s used at %A" (identOf name) (posOf name)
        { state with Linearity = state.Linearity |> Map.add (idOf varName) false }

      | Some false -> fail "Already disposed" (posOf name)
      | None -> state

    TSymbolExpr(TSymbolKind.Var, idOf varName, defTy, posOf name), state

  | TValueSymbol.Prim _ ->
    // Some primitive has polymorphic type that can't be represented in TTy.
    fail "Must appear in function application" (posOf name)

  | TValueSymbol.Fun funName ->
    let defTy =
      state.SymbolTys
      |> Map.tryFind (idOf funName)
      |> unwrap

    TSymbolExpr(TSymbolKind.Fun, idOf funName, defTy, posOf name), state

  | TValueSymbol.NewtypeVariant (variantName, _) ->
    let defTy =
      state.SymbolTys
      |> Map.tryFind (idOf variantName)
      |> unwrap

    TSymbolExpr(TSymbolKind.Variant, idOf variantName, defTy, posOf name), state

let private tcExpr (state: Sx) (expr: AExpr) : TExpr * Sx =
  match expr with
  | ANameExpr name -> tcNameExpr state name

  | AIntExpr (value, pos) -> TIntExpr(value, pos), state
  | AUnitExpr pos -> TUnitExpr pos, state

  | AAppExpr (lExpr, rExpr, pos) ->
    let onDefault () =
      let rRange = rangeOf rExpr
      let lExpr, state = tcExpr state lExpr
      let rExpr, state = tcExpr state rExpr

      let lTy = exprToTy lExpr
      let rTy = exprToTy rExpr

      match lTy with
      | TFunTy (argTy, _) -> unify2 rRange argTy rTy
      | _ -> fail2 "Expected function" rRange

      TAppExpr(lExpr, rExpr), state

    match lExpr with
    | ANameExpr name ->
      match resolveValueSymbol state name with
      | TValueSymbol.Prim prim ->
        let rRange = rangeOf rExpr
        let rExpr, state = tcExpr state rExpr

        match prim with
        | TPrim.Assert ->
          unify (posOf name) (exprToTy rExpr) TBoolTy
          TUnaryExpr(TUnary.Assert, rExpr, pos), state

        | TPrim.UUAcquire -> TUnaryExpr(TUnary.UUAcquire, rExpr, pos), state

        | TPrim.UUDispose ->
          match exprToTy rExpr with
          | TLinearTy _ -> ()
          | _ -> fail2 "Expected __linear<_>" rRange

          TUnaryExpr(TUnary.UUDispose, rExpr, pos), state

      | _ -> onDefault ()
    | _ -> onDefault ()

  | ABinaryExpr (binary, lExpr, rExpr) ->
    let lRange = rangeOf lExpr
    let rRange = rangeOf rExpr
    let lExpr, state = tcExpr state lExpr
    let rExpr, state = tcExpr state rExpr

    let binary, lTy, rTy =
      match binary with
      | Binary.Add -> TBinary.Add, TIntTy, TIntTy
      | Binary.Equal -> TBinary.Equal, TIntTy, TIntTy
      | Binary.Pair -> TBinary.Pair, exprToTy lExpr, exprToTy rExpr

    unify2 lRange (exprToTy lExpr) lTy
    unify2 rRange (exprToTy rExpr) rTy
    TBinaryExpr(binary, lExpr, rExpr), state

  | AIfExpr (cond, thenClause, elseClause, range) ->
    let cond, state = tcExprTopDown TBoolTy state cond
    let branch, state = startBranch state
    let thenClause, state = tcExpr state thenClause
    let branch, state = nextBranch branch state
    let elseClause, state = tcExpr state elseClause
    let state = endBranch branch state
    unify range.Start (exprToTy thenClause) (exprToTy elseClause)
    TIfExpr(cond, thenClause, elseClause), state

  | ALetExpr (pat, init, _) ->
    let init, state = tcExpr state init
    let pat, state = tcPat (exprToTy init) state pat
    TLetExpr(pat, init), state

  | ABlockExpr (exprs, last, _) ->
    let exprs, state =
      exprs
      |> List.mapFold (tcExprTopDown TUnitTy) state

    let last, state = tcExpr state last
    TBlockExpr(exprs, last), state

/// Type-checks an expression and validates it has expected target type.
let private tcExprTopDown targetTy state expr : TExpr * Sx =
  let range = rangeOf expr
  let expr, state = tcExpr state expr
  unify2 range (exprToTy expr) targetTy
  expr, state

let private tcExprBody targetTy (state: Sx) expr : TExpr * Sx =
  assert (state.Linearity |> Map.isEmpty)
  let branch, state = startBranch state
  let expr, state = tcExprTopDown targetTy state expr

  state.Linearity
  |> Map.iter (fun id alive ->
    if alive then
      // printfn "debug: id=%d" id
      fail "Leaked resource" Pos.zero)

  let state = endBranch branch state
  let state = { state with Linearity = Map.empty }

  expr, state

// -----------------------------------------------
// Collect Declarations
// -----------------------------------------------

/// FunId -> (paramTy list, resultTy)
/// VariantId -> ([], payloadTy)
///
/// Stores resolved semantic type of syntactic type ascriptions.
type private SigMap = Map<Id, TTy list * TTy>

/// Collects top-level declarations.
///
/// This extends global scope
/// so that these symbols can recursively reference.
///
/// This collects function signatures.
let private collectDecls (state: Sx) root : SigMap * Sx =
  let (ARoot decls) = root

  // Add top-level declarations to global scope.
  let state =
    decls
    |> List.fold
         (fun (state: Sx) decl ->
           match decl with
           | ANewtypeDecl (name, variant, _, _) ->
             let tName = genName name
             let variantName = genName variant

             { state with
                 ValueEnv =
                   state.ValueEnv
                   |> Map.add (identOf variant) (TValueSymbol.NewtypeVariant(variantName, tName))
                 TyEnv =
                   state.TyEnv
                   |> Map.add (identOf name) (TTySymbol.NewtypeUnion tName) }

           | AFunDecl _
           | AExpectDecl _
           | AExpectErrorDecl _ -> state)
         state

  // Resolve type ascriptions in top-level declarations.
  let sigMap, state =
    decls
    |> List.fold
         (fun (sigMap: SigMap, state: Sx) decl ->
           match decl with
           | AFunDecl (name, paramList, result, _, _) ->
             let tName = genName name // idempotent

             let paramTys =
               paramList
               |> List.map (fun (_, ty) -> resolveAscribedTy state ty)

             let resultTy = resolveAscribedTy state result

             let funTy =
               paramTys
               |> List.rev
               |> List.fold (fun acc sTy -> TFunTy(sTy, acc)) resultTy

             let state =
               { state with
                   SymbolTys = state.SymbolTys |> Map.add (idOf tName) funTy
                   ValueEnv =
                     state.ValueEnv
                     |> Map.add (identOf name) (TValueSymbol.Fun tName) }

             let sigMap =
               sigMap
               |> Map.add (idOf tName) (paramTys, resultTy)

             sigMap, state

           | ANewtypeDecl (name, variant, payload, _) ->
             let unionName = genName name
             let variantName = genName variant

             let unionTy = TUnionTy unionName
             let payloadTy = resolveAscribedTy state payload
             let variantTy = TFunTy(payloadTy, unionTy)

             let sigMap =
               sigMap
               |> Map.add (idOf variantName) ([], payloadTy)

             let state =
               { state with
                   SymbolTys =
                     state.SymbolTys
                     |> Map.add (idOf variantName) variantTy
                   LinearUnions =
                     if isLinearTy state payloadTy then
                       printfn "trace: linear type %s" (identOf name)
                       state.LinearUnions |> Set.add (idOf unionName)
                     else
                       state.LinearUnions }

             sigMap, state

           | AExpectDecl _
           | AExpectErrorDecl _ -> sigMap, state)
         (Map.empty, state)

  sigMap, state

// -----------------------------------------------
// Type Check Declarations
// -----------------------------------------------

let private tcFunDecl (sigMap: SigMap) (state: Sx) funDecl =
  let name, paramList, body, range =
    match funDecl with
    | AFunDecl (name, paramList, _, body, range) -> name, paramList, body, range
    | _ -> unreachable ()

  // Save global scope to remove local names (parameters) later.
  let parentEnv = state.ValueEnv

  let tName = genName name

  // Recollect already resolved type of type ascriptions.
  let paramTys, result = sigMap |> Map.tryFind (idOf tName) |> unwrap

  // Add params to scope.
  let paramList, state =
    let paramList =
      assert (List.length paramList = List.length paramTys)
      List.zip (paramList |> List.map fst) paramTys

    paramList
    |> List.mapFold
         (fun (state: Sx) (name, ty) ->
           let paramName = genName name

           let state =
             { state with
                 SymbolTys = state.SymbolTys |> Map.add (idOf paramName) ty
                 ValueEnv =
                   state.ValueEnv
                   |> Map.add (identOf name) (TValueSymbol.Var paramName) }

           (paramName, ty), state)
         state

  let body, state = tcExprBody result state body

  // Rollback scope.
  let state = { state with ValueEnv = parentEnv }

  let funDef: TFunDef =
    { Name = tName
      ParamList = paramList
      ResultTy = result
      Body = body
      Range = range }

  funDef, state

let private tcNewtypeDecl (sigMap: SigMap) (state: Sx) newtypeDecl =
  let name, variant, range =
    match newtypeDecl with
    | ANewtypeDecl (name, variant, _, range) -> name, variant, range
    | _ -> unreachable ()

  let variantName = genName variant

  let _, payload = sigMap |> Map.tryFind (idOf variantName) |> unwrap

  let newtypeDef: TNewtypeDef =
    { Name = genName name
      Variant = genName variant
      PayloadTy = payload
      Range = range }

  newtypeDef, state

let private tcDecl (sigMap: SigMap) funs newtypes declAcc (state: Sx) (decl: ADecl) =
  match decl with
  | AFunDecl _ ->
    let funDef, state = tcFunDecl sigMap state decl
    let funs = funs |> Map.add (idOf funDef.Name) funDef
    funs, newtypes, declAcc, state

  | ANewtypeDecl _ ->
    let newtypeDef, state = tcNewtypeDecl sigMap state decl

    let newtypes =
      newtypes
      |> Map.add (idOf newtypeDef.Name) newtypeDef

    funs, newtypes, declAcc, state

  | AExpectDecl (desc, body, range) ->
    let body, state = tcExprBody TUnitTy state body
    let declAcc = TExpectDecl(desc, body, range) :: declAcc
    funs, newtypes, declAcc, state

  | AExpectErrorDecl (desc, body, range) ->
    try
      let body, state = tcExprBody TUnitTy state body
      let declAcc = TExpectErrorDecl(desc, body, range) :: declAcc
      funs, newtypes, declAcc, state
    with
    | :? TyError as ex ->
      printfn "trace: expect_error %s at %A is resolved in type check\n  %s at %A" desc range ex.message ex.range
      funs, newtypes, declAcc, state

// -----------------------------------------------
// Interface
// -----------------------------------------------

let typeCheck (ast: ARoot) : TModule =
  let (ARoot decls) = ast

  let sigMap, state = collectDecls (emptyState ()) ast

  let funs, newtypes, declAcc, _ =
    decls
    |> List.fold
         (fun (funs, newtypes, declAcc, state) -> tcDecl sigMap funs newtypes declAcc state)
         (Map.empty, Map.empty, [], state)

  let m: TModule =
    { Decls = List.rev declAcc
      Funs = funs
      Newtypes = newtypes }

  m
