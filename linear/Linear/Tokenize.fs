module Linear.Tokenize

open System
open Linear.Location
open Linear.Token

let private isTrivia token =
  match token with
  | BadToken
  | SpacesToken
  | CommentToken -> true

  | _ -> false

let private isExprLast (t: TokenData) =
  match t with
  | IntToken, _, _
  | IdentToken, _, _
  | KeywordToken, "end", _
  | PunToken, ")", _
  | PunToken, "}", _ -> true

  | _ -> false

let private at (i: int) (s: string) = if i < s.Length then s.[i] else '\x00'

let private notNewline c = c <> '\r' && c <> '\n'
let private notQuote c = c <> '\n' && c <> '"'

let private isLower (c: char) = 'a' <= c && c <= 'z'
let private isUpper (c: char) = 'A' <= c && c <= 'Z'
let private isDigit (c: char) = '0' <= c && c <= '9'

let private isIdent (c: char) =
  isLower (c)
  || isUpper (c)
  || isDigit (c)
  || c = '_'
  || byte c > 0x7fuy

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
// Keywords
// -----------------------------------------------

let private identKind (t: string) =
  match t with
  | "else"
  | "end"
  | "expect_error"
  | "expect"
  | "if"
  | "let"
  | "of"
  | "then"
  | "type" -> KeywordToken

  | _ -> IdentToken

// -----------------------------------------------
// Scan
// -----------------------------------------------

let private countWhile skip pred (i: int) (s: string) =
  let rec go i =
    let c = at i s

    if c <> '\x00' && pred c then
      go (i + 1)
    else
      i

  go (i + skip) - i

let private scanIdent skip (i: int) (s: string) =
  let len = countWhile skip isIdent i s
  assert (len >= 1)

  identKind s.[i .. i + len - 1], len

let tokenizeNext (i: int) (s: string) : Token option * int =
  let get offset = at (i + offset) s

  let ok result =
    let token, len = result
    Some token, len

  match get 0 with
  | '\x00' -> None, 0

  | ' '
  | '\t'
  | '\r'
  | '\n' -> Some SpacesToken, countWhile 1 Char.IsWhiteSpace i s

  | '('
  | ')'
  | '['
  | ']'
  | '{'
  | '}'
  | '<'
  | '>'
  | ':'
  | ','
  | '='
  | '+'
  | ';'
  | '*' -> Some PunToken, 1

  | '"' -> Some StringToken, countWhile 1 notQuote i s + 1

  | '-' when get 1 = '>' -> Some PunToken, 2

  | '/' when get 1 = '/' -> Some CommentToken, countWhile 2 notNewline i s

  // | '\'' when isIdent (get 1) && get 2 <> '\'' -> Some GreekToken, countWhile 1 isIdent i s

  | c when isDigit c -> Some IntToken, countWhile 1 isDigit i s
  | c when isIdent c -> scanIdent 1 i s |> ok

  | _ -> Some BadToken, countWhile 1 isBad i s

type private Paren =
  | Paren
  | Brace

let tokenizeString (s: string) : TokenData array * (string * Range) array =
  let tokens = ResizeArray<TokenData>()
  let errors = ResizeArray()
  let mutable cursor = Cursor.create s

  while cursor |> Cursor.atEof |> not do
    let i = cursor.Index

    match tokenizeNext i s with
    | None, _ ->
      let _, endIndex, range, c = Cursor.spanBy (s.Length - i) cursor
      errors.Add(s.[i .. endIndex - 1], range)
      cursor <- c
      assert (Cursor.atEof cursor)

    | Some kind, len ->
      let _, endIndex, range, c = Cursor.spanBy len cursor
      cursor <- c

      let text = s.[i .. endIndex - 1]

      if kind = BadToken then
        errors.Add(text, range)
      else if kind |> isTrivia |> not then
        let autoSemi =
          if tokens.Count >= 1 then
            let prev = tokens.[tokens.Count - 1]
            let _, _, r = prev

            if r.End.Row <> range.Start.Row && isExprLast prev then
              Some r.End
            else
              None
          else
            None

        match autoSemi with
        | Some pos -> tokens.Add(PunToken, ";", Range.ofPos pos)
        | _ -> ()

        tokens.Add(kind, text, range)

  tokens.ToArray(), errors.ToArray()
