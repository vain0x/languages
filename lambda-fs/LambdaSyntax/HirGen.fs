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
    SymbolMap: Map<Pos, int> }

let private newCtx (scopeRes: ScopeResult): Ctx =
  let symbolMap =
    scopeRes.Symbols
    |> Array.mapi (fun i posArray -> posArray |> Array.map (fun pos -> pos, i))
    |> Array.collect id
    |> Map.ofArray

  { Scope = scopeRes
    SymbolMap = symbolMap }

let private getName (ctx: Ctx) name =
  let (AName (_, text, range)) = name
  let pos = range.Start

  match ctx.SymbolMap |> Map.tryFind pos with
  | Some symbol -> text, symbol, pos
  | None -> failwith "NEVER: unresolved variable"

let private hgTy ctx ty =
  match ty with
  | AUniversalTy name -> HVarTy(getName ctx name)

  | AArrowTy (l, r, pos) ->
      let l = hgTy ctx l
      let r = hgTy ctx r
      HArrowTy(l, r, pos)

let private collectVarTys ty =
  let varTys = ResizeArray()

  let rec go ty =
    match ty with
    | HVarTy v -> varTys.Add(v)

    | HArrowTy (l, r, _) ->
        go l
        go r

  go ty

  varTys
  |> Seq.distinctBy (fun (_, id, _) -> id)
  |> Seq.toArray

let private hgTyScheme ctx ty =
  let ty = hgTy ctx ty
  let varTys = collectVarTys ty
  varTys, ty

let private hgLetExpr (ctx: Ctx) letExpr =
  match letExpr with
  | ALetExpr (name, init, None, pos) ->
      let init = hgExpr ctx init
      HLetExpr(getName ctx name, init, pos)

  | ALetExpr (name, init, Some next, pos) ->
      // Flatten chain of let-in.
      let stmts, last =
        let items = ResizeArray()
        items.Add(name, init, pos)

        let rec go expr =
          match expr with
          | ALetExpr (name, init, Some next, pos) ->
              items.Add(name, init, pos)
              go next

          | _ -> expr

        let next = go next
        items.ToArray(), next

      let stmts =
        stmts
        |> Array.map
             (fun (name, init, pos) ->
               let init = hgExpr ctx init
               HLetExpr(getName ctx name, init, pos))

      let last = hgExpr ctx last
      HBlockExpr(stmts, last, pos)

  | _ -> failwith "NEVER"

let private hgExpr (ctx: Ctx) expr =
  match expr with
  | ANameExpr name -> HNameExpr(getName ctx name)

  | ALambdaExpr (name, body, pos) ->
      let body = hgExpr ctx body
      HLambdaExpr(getName ctx name, body, pos)

  | ALetExpr _ -> hgLetExpr ctx expr

  | ATypeAssertExpr (arg, ty, pos) ->
      let arg = hgExpr ctx arg
      let ty = hgTyScheme ctx ty
      HTypeAssertExpr(arg, ty, pos)

  | ATypeErrorExpr (arg, pos) ->
      let arg = hgExpr ctx arg
      HTypeErrorExpr(arg, pos)

  | AAppExpr (l, r) ->
      let l = hgExpr ctx l
      let r = hgExpr ctx r
      HAppExpr(l, r)

  | ABlockExpr (stmts, last, pos) ->
      let stmts = stmts |> hgStmts ctx
      let last = hgExpr ctx last
      HBlockExpr(stmts, last, pos)

let private hgStmts ctx stmts = stmts |> Array.map (hgExpr ctx)

let hirGen (ast: ARoot) (scopeRes: ScopeResult): HRoot =
  let ctx = newCtx scopeRes

  let (ARoot stmts) = ast
  let stmts = hgStmts ctx stmts

  HRoot stmts
