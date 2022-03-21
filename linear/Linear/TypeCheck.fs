module rec Linear.TypeCheck

open Linear.Ast
open Linear.Location
open Linear.TypedIR

exception TyError of message: string * pos: Pos with
  override this.ToString() =
    $"Type error: {this.pos} {this.message}"

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
    ValueEnv: Map<TIdent, TValueSymbol>
    TyEnv: Map<TIdent, TTySymbol> }

type private Sx = TyCheckState

let private fail message (pos: Pos) : 'A = raise (TyError(message, pos))

let private emptyState () : TyCheckState =
  { SymbolTys = Map.empty
    ValueEnv =
      Map.ofList [ "assert", TValueSymbol.Prim TPrim.Assert
                   "__acquire", TValueSymbol.Prim TPrim.UUAcquire
                   "__dispose", TValueSymbol.Prim TPrim.UUDispose ]
    TyEnv =
      Map.ofList [ "int", TTySymbol.Int
                   "unit", TTySymbol.Unit
                   "bool", TTySymbol.Bool ] }

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
    raise (TyError($"can't unify {lTy}, {rTy}", pos))

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
          SymbolTys = state.SymbolTys |> Map.add (idOf tName) targetTy }

    TVarPat(tName, targetTy), state

  | AWildcardPat pos -> TWildcardPat(targetTy, pos), state

  | AUnitPat pos ->
    match targetTy with
    | TUnitTy -> TUnitPat pos, state
    | _ -> fail "Expected unit" pos

  | APairPat (lPat, rPat) ->
    match targetTy with
    | TPairTy (lTy, rTy) ->
      let lPat, state = tcPat lTy state lPat
      let rPat, state = tcPat rTy state rPat
      TPairPat(lPat, rPat), state

    | _ -> fail "Expected pair type" Pos.zero // FIXME: pos

  | AWrapPat (name, payloadPat) ->
    let variantName, unionName =
      match resolveValueSymbol state name with
      | TValueSymbol.NewtypeVariant (variantName, unionName) -> variantName, unionName
      | _ -> failwithf "Expected variant name %A" (posOf name)

    match targetTy with
    | TUnionTy u when idOf u <> idOf unionName -> ()
    | _ -> fail "Expected union" (posOf name)

    let defPayloadTy =
      state.SymbolTys
      |> Map.tryFind (idOf variantName)
      |> unwrap

    let payloadPat, state = tcPat defPayloadTy state payloadPat
    TWrapPat(variantName, payloadPat), state

// -----------------------------------------------
// Type Check Expressions
// -----------------------------------------------

// Computes expression type bottom-up.
let private tcExprInfer (state: Sx) (expr: AExpr) : TTy =
  match expr with
  | AIntExpr _ -> TIntTy
  | AUnitExpr _ -> TUnitTy

  | ANameExpr name ->
    match resolveValueSymbol state name with
    | TValueSymbol.Var varName ->
      state.SymbolTys
      |> Map.tryFind (idOf varName)
      |> unwrap

    | _ ->
      // Functions, primitives and variants appear only as function in application.
      fail "Can't appear as value" (posOf name)

  | AAppExpr (lExpr, rExpr) ->
    let onDefault () =
      match tcExprInfer state lExpr with
      | TFunTy (_, rTy) -> rTy
      | _ -> fail "Expected function type" Pos.zero

    match lExpr with
    | ANameExpr name ->
      // Some primitive has polymorphic type that can't be represented in TTy.
      // Here compute the use-site type of primitive by using argument type.
      match resolveValueSymbol state name with
      | TValueSymbol.Prim prim ->
        let rTy = tcExprInfer state rExpr

        match prim with
        | TPrim.Assert
        | TPrim.UUDispose -> TUnitTy

        | TPrim.UUAcquire -> TLinearTy rTy

      | _ -> onDefault ()
    | _ -> onDefault ()

  | ABinaryExpr (binary, lExpr, rExpr) ->
    match binary with
    | Binary.Add -> TIntTy
    | Binary.Equal -> TBoolTy

    | Binary.Pair ->
      let lTy = tcExprInfer state lExpr
      let rTy = tcExprInfer state rExpr
      TPairTy(lTy, rTy)

  | AIfExpr (_, thenClause, _) -> tcExprInfer state thenClause
  | ALetExpr _ -> TUnitTy
  | ABlockExpr (_, last) -> tcExprInfer state last

let private tcNameExpr targetTy state name : TExpr * Sx =
  match resolveValueSymbol state name with
  | TValueSymbol.Var varName ->
    let defTy =
      state.SymbolTys
      |> Map.tryFind (idOf varName)
      |> unwrap

    unify (posOf name) targetTy defTy
    TSymbolExpr(TSymbolKind.Var, idOf varName, defTy, posOf name), state

  | TValueSymbol.Prim _ ->
    // Type of some primitive is polymorphic and not representable.
    fail "Must appear in function application" (posOf name)

  | TValueSymbol.Fun funName ->
    let defTy =
      state.SymbolTys
      |> Map.tryFind (idOf funName)
      |> unwrap

    unify (posOf name) targetTy defTy
    TSymbolExpr(TSymbolKind.Fun, idOf funName, defTy, posOf name), state

  | TValueSymbol.NewtypeVariant (variantName, _) ->
    let defTy =
      state.SymbolTys
      |> Map.tryFind (idOf variantName)
      |> unwrap

    unify (posOf name) targetTy defTy
    TSymbolExpr(TSymbolKind.Variant, idOf variantName, defTy, posOf name), state

let private tcExpr targetTy (state: Sx) (expr: AExpr) : TExpr * Sx =
  match expr with
  | ANameExpr name -> tcNameExpr targetTy state name

  | AIntExpr (value, pos) ->
    match targetTy with
    | TIntTy -> TIntExpr(value, pos), state
    | _ -> fail "Expected int" pos

  | AUnitExpr pos ->
    match targetTy with
    | TUnitTy -> TUnitExpr pos, state
    | _ -> fail "Expected unit" pos

  | AAppExpr (lExpr, rExpr) ->
    let onDefault () =
      let lTy, rTy =
        match targetTy with
        | TFunTy (lTy, rTy) -> lTy, rTy
        | _ -> fail "Expected function" Pos.zero // FIXME: pos

      let lExpr, state = tcExpr lTy state lExpr
      let rExpr, state = tcExpr rTy state rExpr
      TAppExpr(lExpr, rExpr), state

    match lExpr with
    | ANameExpr (AName (_, name, _)) ->
      match resolveValueSymbol state (identOf name) with
      | TValueSymbol.Prim prim -> ()
      | _ -> onDefault ()
    | _ -> onDefault ()



  | ABinaryExpr (binary, lExpr, rExpr) ->
    let binary, lTy, rTy, resultTy =
      match binary with
      | Binary.Add -> TBinary.Add, TIntTy, TIntTy, TIntTy
      | Binary.Equal -> TBinary.Equal, TIntTy, TIntTy, TBoolTy
      | Binary.Pair ->
        match targetTy with
        | TPairTy (lTy, rTy) -> TBinary.Pair, lTy, rTy, TPairTy(lTy, rTy)
        | _ -> fail "Expected pair" Pos.zero // FIXME: pos

    let pos = Pos.zero // FIXME: pos
    unify pos targetTy resultTy
    let lExpr, state = tcExpr lTy state lExpr
    let rExpr, state = tcExpr rTy state rExpr
    TBinaryExpr(binary, lExpr, rExpr), state

  | AIfExpr (cond, thenClause, elseClause) ->
    let cond, state = tcExpr TBoolTy state cond
    let thenClause, state = tcExpr targetTy state thenClause
    let elseClause, state = tcExpr targetTy state elseClause
    TIfExpr(cond, thenClause, elseClause), state

  | ALetExpr (pat, init) ->
    let pat, state = tcPat () state pat
    let init, state = tcExpr () state init
    TLetExpr(pat, init), state

  | ABlockExpr (exprs, last) -> failwith "Not Implemented"

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
                   |> Map.add (identOf variant) (TValueSymbol.NewtypeVariant variantName)
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
                     |> Map.add (idOf variantName) variantTy }

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

  let body, state = tcExpr result state body

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

  let unionName = genName name

  let _, payload = sigMap |> Map.tryFind (idOf unionName) |> unwrap

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
    let body, state = tcExpr TUnitTy state body
    let declAcc = TExpectDecl(desc, body, range) :: declAcc
    funs, newtypes, declAcc, state

  | AExpectErrorDecl (desc, body, range) ->
    try
      let body, state = tcExpr TUnitTy state body
      let declAcc = TExpectErrorDecl(desc, body, range) :: declAcc
      funs, newtypes, declAcc, state
    with
    | :? TyError -> funs, newtypes, declAcc, state

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
