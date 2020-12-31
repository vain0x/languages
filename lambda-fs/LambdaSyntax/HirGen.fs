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
  | AUniversalTy name ->
      let text, symbol, pos = getName ctx name
      HVarTy((text, symbol), pos)

  | AArrowTy (l, r) ->
      let l = hgTy ctx l
      let r = hgTy ctx r
      HArrowTy(l, r, Pos.zero)

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
  let ty = hgTy ctx ty
  let varTys = collectVarTys ty
  varTys, ty

let private hgLetExpr (ctx: Ctx) letExpr =
  match letExpr with
  | ALetExpr (name, init, None) ->
      let text, symbol, pos = getName ctx name
      let init = hgExpr ctx init
      HLetExpr((text, symbol), init, pos)

  | ALetExpr (name, init, Some next) ->
      // Flatten chain of let-in.
      let stmts, last =
        let items = ResizeArray()
        items.Add(name, init)

        let rec go expr =
          match expr with
          | ALetExpr (name, init, Some next) ->
              items.Add(name, init)
              go next

          | _ -> expr

        let next = go next
        items.ToArray(), next

      let stmts =
        stmts
        |> Array.map
             (fun (name, init) ->
               let text, symbol, pos = getName ctx name
               let init = hgExpr ctx init
               HLetExpr((text, symbol), init, pos))

      let last = hgExpr ctx last
      HBlockExpr(stmts, last, Pos.zero)

  | _ -> failwith "NEVER"

let private hgExpr (ctx: Ctx) expr =
  match expr with
  | ANameExpr name ->
      let text, symbol, pos = getName ctx name
      HNameExpr((text, symbol), pos)

  | ALambdaExpr (name, body) ->
      let text, symbol, pos = getName ctx name
      let body = hgExpr ctx body
      HLambdaExpr((text, symbol), body, pos)

  | ALetExpr _ -> hgLetExpr ctx expr

  | ATypeAssertExpr (arg, ty) ->
      let arg = hgExpr ctx arg
      let ty = hgTyScheme ctx ty
      HTypeAssertExpr(arg, ty, Pos.zero)

  | ATypeErrorExpr arg ->
      let arg = hgExpr ctx arg
      HTypeErrorExpr(arg, Pos.zero)

  | AAppExpr (l, r) ->
      let l = hgExpr ctx l
      let r = hgExpr ctx r
      HAppExpr(l, r, Pos.zero)

  | ABlockExpr (stmts, last) ->
      let stmts = stmts |> hgStmts ctx
      let last = hgExpr ctx last
      HBlockExpr(stmts, last, Pos.zero)

let private hgStmts ctx stmts = stmts |> Array.map (hgExpr ctx)

let hirGen (ast: ARoot) (scopeRes: ScopeResult): HRoot =
  let ctx = newCtx scopeRes

  let (ARoot stmts) = ast
  let stmts = hgStmts ctx stmts

  HRoot stmts
