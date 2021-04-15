module rec MiloneSyntaxV2.SyntaxTokenize

open MiloneSyntaxV2.Source
open MiloneSyntaxV2.Syntax

module C = MiloneStd.StdChar
module S = MiloneStd.StdString
module M = MiloneStd.StdMap

// -----------------------------------------------
// Cursor
// -----------------------------------------------

/// String with position.
[<RequireQualifiedAccess; Struct>]
type Cursor =
  { String: string
    Index: int

    /// Row index (0-indexed).
    Row: int

    /// Column index (0-indexed).
    Column: int }

module Cursor =
  let create (s: string): Cursor =
    { String = s
      Index = 0
      Row = 0
      Column = 0 }

  let atEof (cursor: Cursor) =
    assert (uint cursor.Index <= uint cursor.String.Length)
    cursor.Index >= cursor.String.Length

  let nth (offset: int) (cursor: Cursor): char =
    if uint (cursor.Index + offset) < uint cursor.String.Length then
      cursor.String.[cursor.Index + offset]
    else
      '\x00'

  let advanceTo (endIndex: int) (cursor: Cursor): Cursor =
    let s = cursor.String
    let endIndex = endIndex |> max 0 |> min s.Length

    let rec go i row column: Cursor =
      if i >= endIndex then
        { String = s
          Index = i
          Row = row
          Column = column }
      else if s.[i] = '\n' then
        go (i + 1) (row + 1) 0
      else
        go (i + 1) row (column + 1)

    go cursor.Index cursor.Row cursor.Column

  let advanceBy (len: int) (cursor: Cursor): Cursor = advanceTo (cursor.Index + len) cursor

  let toPos (cursor: Cursor): Pos =
    { Row = cursor.Row
      Column = cursor.Column }

  let spanBy (len: int) (cursor: Cursor) =
    let startIndex = cursor.Index
    let endIndex = startIndex + len

    let start = toPos cursor
    let cursor = advanceTo endIndex cursor
    let range = Range.ofPair (start, toPos cursor)

    startIndex, endIndex, range, cursor

// -----------------------------------------------
// Token
// -----------------------------------------------

let private isTrivia token =
  match token with
  | BadToken
  | BlankToken
  | NewLinesToken
  | CommentToken -> true

  | _ -> false

let private identKind (host: TokenizeHost) (s: string) =
  match host.KeywordMap |> M.tryFind s with
  | None -> IdentToken
  | Some _ -> KeywordToken

// -----------------------------------------------
// Char types
// -----------------------------------------------

let private at (i: int) (s: string) = if i < s.Length then s.[i] else '\x00'

let private isNewLine c = c = '\r' || c = '\n'

let private notNewLine c = c <> '\r' && c <> '\n'

let private isPunctuation (c: char) =
  match c with
  | '_'
  | '('
  | ')'
  | '['
  | ']'
  | '{'
  | '}' -> false

  | _ -> C.isPunctuation c

let private isIdent (c: char) =
  C.isAlphanumeric c || c = '_' || uint c > uint 127

let private isBad (c: char) =
  match c with
  | ' '
  | '\t'
  | '\r'
  | '\n'
  | '('
  | ')'
  | '['
  | ']'
  | '{'
  | '}' -> false

  | _ -> true

// -----------------------------------------------
// Scan
// -----------------------------------------------

let private countWhile (skip: int) pred (i: int) (s: string) =
  let rec go i =
    let c = at i s

    if c <> '\x00' && pred c then
      go (i + 1)
    else
      i

  go (i + skip) - i


let private scanCharLit (i: int) (s: string) =
  let rec go i =
    match at i s with
    | '\'' -> true, i + 1

    | '\x00'
    | '\r'
    | '\n' -> false, i

    | '\\' ->
        if at (i + 1) s <> '\x00' then
          go (i + 2)
        else
          false, i + 1

    | _ -> go (i + 1)

  let ok, r =
    assert (at i s = '\'')
    go (i + 1)

  let kind = if ok then CharToken else BadToken
  let len = r - i
  kind, len

let private scanStringLit (i: int) (s: string) =
  let rec go i =
    match at i s with
    | '"' -> true, i + 1

    | '\x00'
    | '\r'
    | '\n' -> false, i

    | '\\' ->
        if at (i + 1) s <> '\x00' then
          go (i + 2)
        else
          false, i + 1

    | _ -> go (i + 1)

  let ok, r =
    assert (at i s = '"')
    go (i + 1)

  let kind = (if ok then StringToken else BadToken)
  let len = r - i
  kind, len

let private scanIdent host skip (i: int) (s: string) =
  let len = countWhile skip isIdent i s
  assert (len >= 1)

  identKind host s.[i..i + len - 1], len

// -----------------------------------------------
// Interface
// -----------------------------------------------

let tokenizeNext host (i: int) (s: string): Token option * int =
  let get offset = at (i + offset) s

  let ok result =
    let token, len = result
    Some token, len

  match get 0 with
  | '\x00' -> None, 0

  | ' '
  | '\t' -> Some BlankToken, countWhile 1 C.isBlank i s

  | '\r'
  | '\n' -> Some NewLinesToken, countWhile 1 isNewLine i s

  | '('
  | ')'
  | '['
  | ']'
  | '{'
  | '}'
  | ','
  | ';' -> Some PunToken, 1

  | '/' when get 1 = '/' -> Some CommentToken, countWhile 2 notNewLine i s

  | '\'' -> scanCharLit i s |> ok
  | '"' -> scanStringLit i s |> ok

  | '_' -> scanIdent host 1 i s |> ok

  | c when C.isPunctuation c -> Some PunToken, countWhile 1 isPunctuation i s
  | c when C.isDigit c -> Some IntToken, countWhile 1 C.isDigit i s
  | c when isIdent c -> scanIdent host 1 i s |> ok

  | _ -> Some BadToken, countWhile 1 isBad i s

let tokenizeString (host: TokenizeHost) (s: string): TokenizeResult =
  let rec go tokens errors cursor =
    if Cursor.atEof cursor then
      // Insert sentinels.
      let eof =
        let pos = Cursor.toPos cursor
        TokenData(EofToken, "", pos)

      List.rev (eof :: eof :: tokens), List.rev errors
    else
      let i = cursor.Index

      match tokenizeNext host i s with
      | None, _ ->
          let _, endIndex, range, c = Cursor.spanBy (s.Length - i) cursor
          let error = SyntaxError(s.[i..endIndex - 1], range)
          assert (Cursor.atEof c)
          go tokens (error :: errors) c

      | Some kind, len ->
          let _, endIndex, range, c = Cursor.spanBy len cursor

          match kind with
          | BadToken ->
              let error = SyntaxError(s.[i..endIndex - 1], range)
              go tokens (error :: errors) c

          | _ when isTrivia kind ->
              // eprintfn "trivia: %A at %s" s.[i..endIndex - 1] (Range.toString range)
              go tokens errors c

          | _ ->
              let token =
                TokenData(kind, s.[i..endIndex - 1], range.Start)

              go (token :: tokens) errors c

  let tokens, errors = go [] [] (Cursor.create s)
  { Tokens = tokens; Errors = errors }
