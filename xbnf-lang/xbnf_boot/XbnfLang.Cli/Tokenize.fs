module XbnfLang.Tokenize

open XbnfLang.Helpers
open XbnfLang.Types

// -----------------------------------------------
// Character helpers
// -----------------------------------------------

let charNull: char = char 0

let charSub (x: char) (y: char) =
  int x - int y

let charIsControl (c: char) =
  let n = int c
  0 <= n && n < 32 || n = 127

let charIsCommentFirst (c: char) =
  c = '#'
  || c = '/'

let charIsSpace (c: char): bool =
  c = ' ' || c = '\t' || c = '\r' || c = '\n'

let charIsDigit (c: char): bool =
  '0' <= c && c <= '9'

let charIsAlpha (c: char): bool =
  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

let charIsIdentFirst (c: char) =
  c = '_' || charIsAlpha c

let charIsIdent (c: char): bool =
  c = '_' || charIsDigit c || charIsAlpha c

let charIsPun (c: char): bool =
  c = '('
  || c = ')'
  || c = ','
  || c = '='
  || c = '|'
  || c = '+'
  || c = ';'
  || c = '/'
  || c = '*'
  || c = '?'
  || c = '@'

let charIsAnyFirst (c: char) =
  charIsCommentFirst c
  || charIsSpace c
  || charIsPun c
  || c = '\''
  || c = '"'
  || charIsIdentFirst c

let charIsOther (c: char) =
  charIsAnyFirst c |> not

// -----------------------------------------------
// String helpers
// -----------------------------------------------

let strIsLoud (text: string) =
  let loud = text.ToUpperInvariant()
  text = loud && loud <> text.ToLowerInvariant()

let strSlice (start: int) (endIndex: int) (s: string): string =
  assert (start <= endIndex && endIndex <= s.Length)
  if start >= endIndex then
    ""
  else
    s.[start..endIndex - 1]

/// `s.[i..].StartsWith(prefix)`
let strIsFollowedBy (i: int) (prefix: string) (s: string): bool =
  /// `s.[si..].StartsWith(prefix.[pi..])`
  let rec go pi si =
    pi = prefix.Length || (
      si < s.Length
      && prefix.[pi] = s.[si]
      && go (pi + 1) (si + 1)
    )
  i + prefix.Length <= s.Length && go 0 i

// -----------------------------------------------
// Scan functions
// -----------------------------------------------

let lookEof (text: string) (i: int) =
  i >= text.Length

let lookOther (text: string) (i: int) =
  text.[i] |> charIsOther

let scanOther (text: string) (i: int) =
  assert (lookOther text i)
  let rec go i =
    if i < text.Length && text.[i] |> charIsOther then
      go (i + 1)
    else
      i
  go i

let lookSpace (text: string) (i: int) =
  text.[i] |> charIsSpace

let scanSpace (text: string) (i: int) =
  assert (lookSpace text i)
  let rec go i =
    if i < text.Length && text.[i] |> charIsSpace then
      go (i + 1)
    else
      i
  go i

let lookComment (text: string) (i: int) =
  text |> strIsFollowedBy i "#"
  || text |> strIsFollowedBy i "//"
  || text |> strIsFollowedBy i "--"

let scanComment (text: string) (i: int) =
  assert (lookComment text i)
  let rec go i =
    if i = text.Length then
      i
    else if text.[i] = '\n' then
      i + 1
    else
      go (i + 1)
  go i

let lookPun (text: string) (i: int) =
  text.[i] |> charIsPun

let scanPun (text: string) (i: int) =
  assert (lookPun text i)
  i + 1

let lookIdent (text: string) (i: int) =
  text.[i] |> charIsIdent
  && text.[i] |> charIsDigit |> not

let scanIdent (text: string) (i: int) =
  assert (lookIdent text i)
  let rec go i =
    if i < text.Length && text.[i] |> charIsIdent then
      go (i + 1)
    else
      i
  go i

let lookIntLit (text: string) (i: int) =
  text.[i] |> charIsDigit

let scanIntLit (text: string) (i: int) =
  assert (lookIntLit text i)
  let rec go i =
    if i < text.Length && text.[i] |> charIsDigit then
      go (i + 1)
    else
      i
  go i

let lookCharLit (text: string) (i: int) =
  text.[i] = '\''

let scanCharLit (text: string) (i: int) =
  assert (lookCharLit text i)

  // FIXME: escape sequence
  let rec go i =
    if i < text.Length && text.[i] = '\'' then
      // Success.
      i + 1
    else if i < text.Length && text.[i] <> '\n' then
      // Go ahead.
      go (i + 1)
    else
      // Missed the closing quote.
      assert (i = text.Length || text.[i] = '\n')
      i

  go (i + 1)

let lookStrLit (text: string) (i: int) =
  text.[i] = '"'

let scanStrLit (text: string) (i: int) =
  assert (lookStrLit text i)

  // FIXME: escape sequence
  let rec go i =
    if i < text.Length && text.[i] = '"' then
      // Success.
      i + 1
    else if i < text.Length && text.[i] <> '\n' then
      // Go ahead.
      go (i + 1)
    else
      // Missed the closing quote.
      assert (i = text.Length || text.[i] = '\n')
      i

  go (i + 1)

// -----------------------------------------------
// Tokenize rules
// -----------------------------------------------

let tokenize (text: string) =
  let rec go (i: int) acc =
    if lookEof text i then
      acc |> cons (EofToken, i, i)

    else if lookSpace text i then
      let r = scanSpace text i
      acc |> go r

    else if lookComment text i then
      let r = scanComment text i
      acc |> go r

    else if lookPun text i then
      let r = scanPun text i
      let content = text |> strSlice i r
      acc |> cons (PunToken content, i, r) |> go r

    else if lookIdent text i then
      let r = scanIdent text i
      let ident = text |> strSlice i r
      let kind =
        if strIsLoud ident then
          LoudToken ident
        else
          SnakeToken ident
      acc |> cons (kind, i, r) |> go r

    else if lookCharLit text i then
      let r = scanCharLit text i
      let tokenText = text |> strSlice i r
      acc |> cons (StrToken tokenText, i, r) |> go r

    else if lookStrLit text i then
      let r = scanStrLit text i
      let tokenText = text |> strSlice i r
      acc |> cons (StrToken tokenText, i, r) |> go r

    else
      assert (lookOther text i)
      let r = scanOther text i
      acc |> go r

  [] |> go 0 |> List.rev
