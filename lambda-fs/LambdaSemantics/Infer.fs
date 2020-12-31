/// # Infer
///
/// Type inference. (Sound, but not optimal.)
///
/// ## See
///
/// - [Efficient and Insightful Generalization](http://okmij.org/ftp/ML/generalization.html)
/// - http://okmij.org/ftp/ML/generalization/sound_eager.ml
module rec LambdaSemantics.Infer

open LambdaDomain.Hir

let private refEq (l: 'T) (r: 'T): bool = System.Object.ReferenceEquals(l, r)

// -----------------------------------------------
// Types
// -----------------------------------------------

type private VarName = string

type private Level = int

type private QName = string

[<NoEquality; NoComparison>]
type private Ty =
  | TVar of TyVar ref
  | QVar of QName
  | TArrow of Ty * Ty

[<NoEquality; NoComparison>]
type private TyVar =
  | Unbound of hint: string * Level
  | Link of Ty

let private tyUnit = QVar "unit"

let private getLevel (tyVar: TyVar) =
  match tyVar with
  | Unbound (_, level) -> Some level
  | Link _ -> None

// -----------------------------------------------
// Context
// -----------------------------------------------

type private InferCtx =
  { LastId: int
    Level: Level
    SymbolMap: Map<int, Ty> }

let private newCtx (): InferCtx =
  { LastId = 0
    Level = 1
    SymbolMap = Map.empty }

let private dropCtx (ctx: InferCtx): unit = assert (ctx.Level = 1)

let private nextId (ctx: InferCtx) =
  let id = ctx.LastId + 1
  id, { ctx with LastId = id }

let private genSymbol (ctx: InferCtx): string * InferCtx =
  let n, ctx = nextId ctx

  let name =
    if n <= 26 then
      string ('a' + char (n - 1))
    else
      "t" + string n

  name, ctx

let private enterLevel (ctx: InferCtx): InferCtx = { ctx with Level = ctx.Level + 1 }

let private leaveLevel (ctx: InferCtx): InferCtx = { ctx with Level = ctx.Level - 1 }

let private freshTyVar (ctx: InferCtx): Ty * InferCtx =
  let name, ctx = genSymbol ctx
  TVar(ref (Unbound(name, ctx.Level))), ctx

// -----------------------------------------------
// Core inference logic
// -----------------------------------------------

exception private RecursiveTypeException of TyVar ref

/// Rejects recursive types.
let private occurs (tvr: TyVar ref) (ty: Ty) =
  match ty with
  | TVar other when refEq tvr other -> raise (RecursiveTypeException tvr)

  | TVar other ->
      match !other with
      | Unbound (otherName, otherLevel) ->
          let minLevel =
            match getLevel !tvr with
            | Some level -> min level otherLevel
            | None -> otherLevel

          other := Unbound(otherName, minLevel)

      | Link ty -> occurs tvr ty

  | QVar _ -> ()
  | TArrow (t1, t2) ->
      occurs tvr t1
      occurs tvr t2

/// Performs unification.
let private unify (l: Ty) (r: Ty): unit =
  let rec go t1 t2 =
    if refEq t1 t2 |> not then
      match t1, t2 with
      | TVar tv, other
      | other, TVar tv ->
          match !tv with
          | Link ty -> go ty other

          | Unbound _ ->
              occurs tv other
              tv := Link other

      | TArrow (tyl1, tyl2), TArrow (tyr1, tyr2) ->
          unify tyl1 tyr1
          unify tyl2 tyr2

      | QVar _, _
      | _, QVar _ -> failwith "NEVER: QVar must be instantiated before unification."

  go l r

/// Performs generalization.
let private doGeneralize (currentLevel: int) ty =
  let rec go ty =
    match ty with
    | TVar tvRef ->
        match !tvRef with
        | Unbound (name, level) when level > currentLevel -> QVar name

        | Unbound _ -> ty
        | Link ty -> go ty

    | QVar _ -> ty

    | TArrow (t1, t2) -> TArrow(go t1, go t2)

  go ty

/// Performs instantiation.
let private instantiate (ctx: InferCtx) ty =
  let rec go subst ctx ty =
    match ty with
    | QVar name ->
        match subst |> Map.tryFind name with
        | Some tv -> tv, subst, ctx

        | None ->
            let tv, ctx = freshTyVar ctx
            let subst = subst |> Map.add name tv
            tv, subst, ctx

    | TVar tvRef ->
        match !tvRef with
        | Link ty -> go subst ctx ty
        | Unbound _ -> ty, subst, ctx

    | TArrow (t1, t2) ->
        let t1, subst, ctx = go subst ctx t1
        let t2, subst, ctx = go subst ctx t2
        TArrow(t1, t2), subst, ctx

  let subst = Map.empty
  let ty, _, ctx = go subst ctx ty
  ty, ctx

// -----------------------------------------------
// Type checking
// -----------------------------------------------

let private tySchemeToTy (ctx: InferCtx) (tyScheme: HTyScheme): Ty =
  let rec go subst ty =
    match ty with
    | HVarTy (_, symbol, _) ->
        let name = subst |> Map.find symbol
        QVar name

    | HArrowTy (l, r, _) ->
        let l = go subst l
        let r = go subst r
        TArrow(l, r)

  let tyVars, ty = tyScheme

  let subst, ctx =
    tyVars
    |> Array.fold
         (fun (subst, ctx) tyVar ->
           let _, symbol, _ = tyVar

           let name, ctx = genSymbol ctx
           let subst = subst |> Map.add symbol name
           subst, ctx)
         (Map.empty, ctx)

  go subst ty

let private tyIsCompatibleTo (ctx: InferCtx) (expected: HTyScheme) (actual: Ty) =
  let expectedTy = tySchemeToTy ctx expected

  try
    let expected, ctx = instantiate ctx expectedTy
    let actual, _ = instantiate ctx actual
    unify expected actual
    true
  with RecursiveTypeException _ -> false

// -----------------------------------------------
// Control
// -----------------------------------------------

type InferResult = { SymbolTysMap: Map<int, HTyScheme> }

let private inferExpr (ctx: InferCtx) (expr: HExpr): Ty * InferCtx =
  match expr with
  | HNameExpr (_, symbol, _) ->
      ctx.SymbolMap
      |> Map.find symbol
      |> instantiate ctx

  | HLambdaExpr ((_, symbol, _), body, _) ->
      // Generate fresh type variable to parameter. (Depends on current level.)
      let paramTy, ctx = freshTyVar ctx

      let ctx =
        { ctx with
            SymbolMap = ctx.SymbolMap |> Map.add symbol paramTy }

      let bodyTy, ctx = inferExpr ctx body

      // Type information of parameter is no longer necessary.
      // let ctx = {ctx with SymbolMap = ctx.SymbolMap |> Map.remove symbol }

      TArrow(paramTy, bodyTy), ctx

  | HAppExpr (e1, e2) ->
      let funTy, ctx = inferExpr ctx e1
      let argTy, ctx = inferExpr ctx e2
      let resultTy, ctx = freshTyVar ctx
      unify funTy (TArrow(argTy, resultTy))
      resultTy, ctx

  | HLetExpr ((_, symbol, _), init, _) ->
      let ctx = enterLevel ctx
      let initTy, ctx = inferExpr ctx init
      let ctx = leaveLevel ctx

      // Generalize outside of init.
      let initTy = doGeneralize ctx.Level initTy

      let ctx =
        { ctx with
            SymbolMap = ctx.SymbolMap |> Map.add symbol initTy }

      tyUnit, ctx

  | HTypeAssertExpr (expr, expected, pos) ->
      printfn "\ntype_assert: expr=%A tyScheme=%A pos=%A" expr expected pos

      let ty, ctx =
        let ctx = enterLevel ctx
        let ty, ctx = inferExpr ctx expr
        let ctx = leaveLevel ctx
        ty, ctx

      let ty = doGeneralize ctx.Level ty

      if tyIsCompatibleTo ctx expected ty then
        printfn "    OK"
      else
        printfn "    ERROR: ty=%A is incompatible." ty

      tyUnit, ctx

  | HTypeErrorExpr (expr, pos) ->
      printfn "\ntype_error: expr=%A pos=%A" expr pos

      try
        let ty, _ = inferExpr ctx expr
        printfn "    ERROR: Expected type error, but inferred: %A" ty
      with RecursiveTypeException _ -> printfn "    OK"

      tyUnit, ctx

  | HBlockExpr (stmts, last, _) ->
      let ctx = inferStmts ctx stmts
      inferExpr ctx last

let private inferStmts ctx stmts =
  stmts
  |> Array.fold
       (fun ctx stmt ->
         let _, ctx = inferExpr ctx stmt
         // unify ty tyUnit
         ctx)
       ctx

let infer (ast: HRoot): InferResult =
  let ctx = newCtx ()

  let (HRoot stmts) = ast
  let ctx = inferStmts ctx stmts

  dropCtx ctx
  { SymbolTysMap = Map.empty }
