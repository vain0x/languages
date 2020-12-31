/// Scope resolution.
module rec LambdaSyntax.ScopeRes

open LambdaDomain.Location
open LambdaSyntax.Syntax
open LambdaSyntax.Token

let private textOfName name =
  let (AName (_, text, _)) = name
  text

let private posOfName name =
  let (AName (_, _, range)) = name
  range.Start

// -----------------------------------------------
// Context
// -----------------------------------------------

/// Scope context.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Sx =
  { ValueEnv: Map<string, int> list
    TyEnv: Map<string, int> list
    Symbols: ResizeArray<ResizeArray<Pos>>
    Unresolved: ResizeArray<Pos> }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ScopeResult =
  { Symbols: Pos [] []
    Unresolved: Pos [] }

let private newCtx (): Sx =
  { ValueEnv = [ Map.empty ]
    TyEnv = [ Map.empty ]
    Symbols = ResizeArray([ ResizeArray() ])
    Unresolved = ResizeArray() }

let private findValue name (ctx: Sx) =
  ctx.ValueEnv
  |> List.tryPick (fun map -> map |> Map.tryFind name)

let private findTy name (ctx: Sx) =
  ctx.TyEnv
  |> List.tryPick (fun map -> map |> Map.tryFind name)

let private freshSymbol (ctx: Sx) =
  let id = ctx.Symbols.Count
  ctx.Symbols.Add(ResizeArray())
  id, ctx

let private importValue alias symbol (ctx: Sx) =
  let valueEnv =
    match ctx.ValueEnv with
    | map :: env -> (map |> Map.add alias symbol) :: env
    | [] -> failwith "NEVER"

  { ctx with ValueEnv = valueEnv }

let private importTy alias symbol (ctx: Sx) =
  let tyEnv =
    match ctx.TyEnv with
    | map :: env -> (map |> Map.add alias symbol) :: env
    | [] -> failwith "NEVER"

  { ctx with TyEnv = tyEnv }

let private doWithScope (f: Sx -> Sx) (ctx: Sx) =
  let valueEnv, tyEnv = ctx.ValueEnv, ctx.TyEnv

  let ctx =
    let innerCtx =
      { ctx with
          ValueEnv = Map.empty :: valueEnv
          TyEnv = Map.empty :: tyEnv }

    f innerCtx

  { ctx with
      ValueEnv = valueEnv
      TyEnv = tyEnv }

let private markAsUnresolved pos (ctx: Sx) =
  ctx.Unresolved.Add(pos)
  ctx

let private markAsDef pos (ctx: Sx) =
  let symbol, ctx = freshSymbol ctx
  ctx.Symbols.[symbol].Add(pos)
  symbol, ctx

let private markAsUse pos symbol (ctx: Sx) =
  ctx.Symbols.[symbol].Add(pos)
  ctx

let private defineAsValue name ctx =
  let symbol, ctx = ctx |> markAsDef (posOfName name)
  ctx |> importValue (textOfName name) symbol

// -----------------------------------------------
// Control
// -----------------------------------------------

let rec private scopeTy ty (ctx: Sx) =
  match ty with
  | AUniversalTy name ->
      match ctx |> findTy (textOfName name) with
      | Some symbol -> ctx |> markAsUse (posOfName name) symbol

      | None ->
          let symbol, ctx = ctx |> markAsDef (posOfName name)

          ctx |> importTy (textOfName name) symbol

  | AArrowTy (s, t, _) -> ctx |> scopeTy s |> scopeTy t

let private scopeTyRoot ty ctx = ctx |> doWithScope (scopeTy ty)

let rec private scopeExpr expr (ctx: Sx) =
  match expr with
  | ANameExpr name ->
      match ctx |> findValue (textOfName name) with
      | Some symbol -> ctx |> markAsUse (posOfName name) symbol
      | None -> ctx |> markAsUnresolved (posOfName name)

  | ALambdaExpr (name, body, _) ->
      ctx
      |> doWithScope (fun ctx -> ctx |> defineAsValue name |> scopeExpr body)

  | ALetExpr (name, init, Some next, _) ->
      ctx
      |> doWithScope (scopeExpr init)
      |> doWithScope (fun ctx -> ctx |> defineAsValue name |> scopeExpr next)

  | ALetExpr (name, init, None, _) ->
      ctx
      |> doWithScope (scopeExpr init)
      |> defineAsValue name

  | ATypeAssertExpr (arg, ty, _) -> ctx |> scopeExpr arg |> scopeTyRoot ty

  | ATypeErrorExpr (arg, _) -> ctx |> scopeExpr arg

  | AAppExpr (l, r) -> ctx |> scopeExpr l |> scopeExpr r

  | ABlockExpr (stmts, last, _) ->
      ctx
      |> doWithScope (fun ctx -> ctx |> scopeExprs stmts |> scopeExpr last)

let private scopeExprs items ctx =
  items
  |> Array.fold (fun ctx stmt -> scopeExpr stmt ctx) ctx

let scopeRes (ast: ARoot): ScopeResult =
  let (ARoot items) = ast

  let ctx = newCtx () |> scopeExprs items

  { Symbols = [| for posArray in ctx.Symbols -> posArray.ToArray() |]
    Unresolved = ctx.Unresolved.ToArray() }
