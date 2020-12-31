/// # HirGen
///
/// Lowers AST to HIR.
module rec LambdaSyntax.HirGen

open LambdaDomain.Location
open LambdaDomain.Hir
open LambdaSyntax.Token
open LambdaSyntax.Syntax
open LambdaSyntax.ScopeRes

let private posOfName name =
  let (AName (_, _, range)) = name
  range.Start

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Ctx =
  { Scope: ScopeResult
    SymbolMap: Map<Pos, int>
    Stmts: ResizeArray<HStmt> }

let private newCtx (scopeRes: ScopeResult): Ctx =
  let symbolMap =
    scopeRes.Symbols
    |> Array.mapi (fun i posArray -> posArray |> Array.map (fun pos -> pos, i))
    |> Array.collect id
    |> Map.ofArray

  { Scope = scopeRes
    SymbolMap = symbolMap
    Stmts = ResizeArray() }

let private addStmt stmt (ctx: Ctx) =
  ctx.Stmts.Add(stmt)
  ctx

let private getName (ctx: Ctx) name =
  let (AName (_, text, range)) = name
  let pos = range.Start

  match ctx.SymbolMap |> Map.tryFind pos with
  | Some symbol -> text, symbol, pos
  | None -> failwith "NEVER: unresolved variable"

let private hgTy ctx ty =
  match ty with
  | AUniversalTy name ->
      let text, symbol, pos = getName ctx name
      HVarTy((text, symbol), pos), ctx

  | AArrowTy (l, r) ->
      let l, ctx = hgTy ctx l
      let r, ctx = hgTy ctx r
      HArrowTy(l, r, Pos.zero), ctx

let private collectVarTys ty =
  let varTys = ResizeArray()

  let rec go ty =
    match ty with
    | HVarTy (v, _) -> varTys.Add(v)

    | HArrowTy (l, r, _) ->
        go l
        go r

  go ty

  varTys |> Seq.distinctBy snd |> Seq.toArray

let private hgTyScheme ctx ty =
  let ty, ctx = hgTy ctx ty
  let varTys = collectVarTys ty
  (varTys, ty), ctx

let private hgExprOrUnit ctx expr =
  let exprOpt, ctx = hgExpr ctx expr

  match exprOpt with
  | Some it -> it, ctx
  | None -> HUnitExpr Pos.zero, ctx

let private hgExprBlock (ctx: Ctx) expr =
  let stmts = ctx.Stmts
  let ctx = { ctx with Stmts = ResizeArray() }

  let exprOpt, (ctx: Ctx) = hgExpr ctx expr

  let expr =
    match exprOpt with
    | Some it -> it
    | None -> HUnitExpr Pos.zero

  let stmts, ctx = ctx.Stmts, { ctx with Stmts = stmts }

  if stmts.Count = 0 then
    expr, ctx
  else
    HBlockExpr(stmts.ToArray(), expr, Pos.zero), ctx

let private hgExpr (ctx: Ctx) expr =
  match expr with
  | ANameExpr name ->
      let text, symbol, pos = getName ctx name
      Some(HNameExpr((text, symbol), pos)), ctx

  | ALambdaExpr (name, body) ->
      let text, symbol, pos = getName ctx name
      let body, ctx = hgExprBlock ctx body
      Some(HLambdaExpr((text, symbol), body, pos)), ctx

  | ALetExpr (name, init, nextOpt) ->
      let text, symbol, pos = getName ctx name
      let init, ctx = hgExprOrUnit ctx init

      let ctx =
        ctx
        |> addStmt (HLetStmt((text, symbol), init, pos))

      match nextOpt with
      | Some next -> hgExpr ctx next
      | None -> None, ctx

  | ATypeAssertExpr (arg, ty) ->
      let arg, ctx = hgExprBlock ctx arg
      let ty, ctx = hgTyScheme ctx ty

      let ctx =
        ctx
        |> addStmt (HTypeAssertStmt(arg, ty, Pos.zero))

      None, ctx

  | ATypeErrorExpr arg ->
      let arg, ctx = hgExprBlock ctx arg

      let ctx =
        ctx |> addStmt (HTypeErrorStmt(arg, Pos.zero))

      None, ctx

  | AAppExpr (l, r) ->
      let l, ctx = hgExprOrUnit ctx l
      let r, ctx = hgExprOrUnit ctx r
      Some(HAppExpr(l, r, Pos.zero)), ctx

  | ABlockExpr (stmts, last) ->
      let ctx = stmts |> hgStmts ctx
      hgExpr ctx last

let private hgStmts ctx stmts =
  stmts
  |> Array.fold
       (fun ctx expr ->
         let exprOpt, ctx = hgExpr ctx expr

         match exprOpt with
         | Some it -> ctx |> addStmt (HExprStmt(it, Pos.zero))
         | None -> ctx)
       ctx

let hirGen (ast: ARoot) (scopeRes: ScopeResult): HRoot =
  let ctx = newCtx scopeRes

  let (ARoot stmts) = ast
  let ctx = hgStmts ctx stmts
  let stmts = ctx.Stmts.ToArray()

  HRoot stmts
