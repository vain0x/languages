module rec LambdaSyntax.Parse

open System.Collections.Generic
open LambdaDomain.Location
open LambdaSyntax.Token
open LambdaSyntax.Syntax

let private some (value, state) = Some value, state

// -----------------------------------------------
// Context
// -----------------------------------------------

/// Parser context.
[<NoEquality; NoComparison>]
type private Px =
  { Tokens: TokenData []
    Index: int
    Eof: TokenData }

let private ofTokens (tokens: TokenData []): Px =
  let endPos =
    if tokens.Length > 0 then
      let _, _, range = tokens.[tokens.Length - 1]
      range.End
    else
      Pos.zero

  let eofToken =
    EofToken, "", Range.ofPair (endPos, endPos)

  { Tokens = tokens
    Index = 0
    Eof = eofToken }

let private error msg (px: Px) =
  let _, _, range = get 0 px
  failwithf "Parse error: %s near (%O)" msg range

let private atEof (px: Px) = px.Index = px.Tokens.Length

let private get (offset: int) (px: Px): TokenData =
  assert (offset >= 0)

  if px.Index + offset < px.Tokens.Length then
    px.Tokens.[px.Index]
  else
    px.Eof

let private at (offset: int) (px: Px): Token =
  assert (offset >= 0)

  if px.Index + offset < px.Tokens.Length then
    let token, _, _ = px.Tokens.[px.Index]
    token
  else
    EofToken

let private bump (px: Px): Px =
  assert (px.Index < px.Tokens.Length)
  { px with Index = px.Index + 1 }

// -----------------------------------------------
// Misc
// -----------------------------------------------

let private expectPun text px =
  match get 0 px with
  | PunToken, punText, _ when punText = text -> bump px
  | _ -> error ("Expected '" + text + "'") px

let private parseName px =
  match get 0 px with
  | (IdentToken, _, _) as token -> AName token, bump px
  | (GreekToken, _, _) as token -> AName token, bump px
  | _ -> error "Expected Name" px

let private parseAscription px =
  match get 0 px with
  | PunToken, ":", _ -> px |> bump |> parseTy
  | _ -> error "Expected ': type'" px

// -----------------------------------------------
// Types
// -----------------------------------------------

let private parseAtomicTy px =
  match get 0 px with
  // | IdentToken, _, _ -> px |> withNode NameTySN bump |> some

  | GreekToken, _, _ ->
      let name, px = parseName px
      AUniversalTy name, px

  | PunToken, "(", _ ->
      let ty, px = px |> bump |> parseTy
      let px = expectPun ")" px
      ty, px

  | _ -> error "Expected type" px

let private parseArrowTy px =
  let l, px = parseAtomicTy px

  match get 0 px with
  | PunToken, "->", _ ->
      let r, px = px |> bump |> parseArrowTy
      AArrowTy(l, r), px

  | _ -> l, px

let private parseTy px = parseArrowTy px

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private atAtomicExpr px =
  match get 0 px with
  | IdentToken, _, _
  | PunToken, "(", _
  | PunToken, "\\", _ -> true

  | _ -> false

let private parseAtomicExpr px =
  match get 0 px with
  | IdentToken, _, _ ->
      let name, px = px |> parseName
      ANameExpr name, px

  | PunToken, "(", _ ->
      let body, px = px |> bump |> parseBlockExpr
      let px = expectPun ")" px
      body, px

  | PunToken, "\\", _ ->
      let name, px = px |> bump |> parseName
      let body, px = px |> expectPun "." |> parseStmt
      ALambdaExpr(name, body), px

  | _ -> error "Expected an expression" px

let private parseAppExpr px =
  let rec go (l, px) =
    if atAtomicExpr px then
      let r, px = px |> parseAtomicExpr
      go (AAppExpr(l, r), px)
    else
      l, px

  px |> parseAtomicExpr |> go

let private atExpr px = atAtomicExpr px

let private parseExpr px = parseAppExpr px

let private atStmt px =
  match get 0 px with
  | KeywordToken, "let", _
  | KeywordToken, "type_assert", _
  | KeywordToken, "type_error", _ -> true
  | _ -> atExpr px

let private parseStmt px =
  match get 0 px with
  | KeywordToken, "let", _ ->
      let name, px = px |> bump |> parseName
      let init, px = px |> expectPun "=" |> parseStmt

      let next, px =
        match get 0 px with
        | KeywordToken, "in", _ -> px |> bump |> parseStmt |> some
        | _ -> None, px

      ALetExpr(name, init, next), px

  | KeywordToken, "type_assert", _ ->
      let arg, px = px |> bump |> parseExpr

      let ty, px = px |> parseAscription
      ATypeAssertExpr(arg, ty), px

  | KeywordToken, "type_error", _ ->
      let arg, px = px |> bump |> parseExpr
      ATypeErrorExpr arg, px

  | _ -> parseExpr px

let private parseBlockExpr px =
  let first, px = parseStmt px

  let items = ResizeArray()

  let rec go last px =
    match get 0 px with
    | PunToken, ";", _ ->
        let px = px |> bump

        if atStmt px then
          let item, px = px |> parseStmt
          items.Add(last)
          go item px
        else
          last, px

    | _ -> last, px

  let last, px = go first px

  if items.Count = 0 then
    last, px
  else
    ABlockExpr(items.ToArray(), last), px

// -----------------------------------------------
// Root
// -----------------------------------------------

let private parseRoot (px: Px) =
  let items = ResizeArray()

  let rec go px =
    if atEof px then
      px
    else if atStmt px |> not then
      error "Expected stmt or EOF" px
    else
      let item, px = parseStmt px
      items.Add(item)

      match get 0 px with
      | PunToken, ";", _ -> px |> bump |> go
      | _ -> go px

  let px = go px
  assert (atEof px)

  items.ToArray()

let parseTokens (tokens: TokenData []): ARoot =
  let px = ofTokens tokens
  let items = parseRoot px
  ARoot items
