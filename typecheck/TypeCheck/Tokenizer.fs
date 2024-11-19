module TypeCheck.Tokenizer

open TypeCheck.Helpers
open TypeCheck.Syntax

let private isSpace (c: char) : bool =
  match c with
  | ' '
  | '\t'
  | '\r'
  | '\n' -> true

  | _ -> false

let private isDigit (c: char) : bool = '0' <= c && c <= '9'

let private isIdent (c: char) : bool =
  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || isDigit c || c = '_'

let private KeywordSet =
  Set.ofList [ "else"; "false"; "fun"; "if"; "in"; "let"; "rec"; "then"; "true" ]

exception BadTokenException of index: int

let nextToken (i: int) (s: string) : Token * int =
  assert (i < s.Length)

  let badToken i = raise (BadTokenException i)

  match nth i s with
  | ' '
  | '\t'
  | '\r'
  | '\n' ->
    let len = 1 + (s |> prefixLen (i + 1) isSpace)
    TriviaToken, len

  | '('
  | ')'
  | '{'
  | '}' -> ParenToken s.[i..i], 1

  | '\\'
  | ':'
  | '.'
  | '='
  | ';' -> PunToken s.[i..i], 1

  | '-' ->
    match nth (i + 1) s with
    | '>' -> PunToken "->", 2
    | _ -> badToken 1

  | '/' ->
    match nth (i + 1) s with
    | '/' ->
      // Comment
      let len = 1 + (s |> prefixLen (i + 1) (fun c -> c <> '\r' && c <> '\n'))
      TriviaToken, len

    | _ -> badToken 1

  | ch when isDigit ch ->
    let len = 1 + (s |> prefixLen (i + 1) isDigit)
    IntToken(int s.[i .. i + len - 1]), len

  | ch when isIdent ch ->
    let len = 1 + (s |> prefixLen (i + 1) isIdent)
    let t = s.[i .. i + len - 1]

    let token =
      if KeywordSet.Contains(t) then
        KeywordToken t
      else
        IdentToken t

    token, len

  | _ -> badToken 1

let tokenize (s: string) : (Token * Range) array =
  let tokens = ResizeArray()
  let mutable index = 0
  let mutable pos = Pos.Zero

  while index < s.Length do
    try
      let (token, len) = nextToken index s

      let startPos = pos
      let endPos = startPos + (Pos.scan index len s)
      pos <- endPos
      index <- index + len

      match token with
      | TriviaToken -> ()
      | _ -> tokens.Add(token, Range.Create(startPos, endPos))
    with BadTokenException _ ->
      failwithf "Tokenize error at %O: Unexpected char %A" pos s.[index]

  tokens.Add(EofToken, Range.Create(pos, pos))

  assert (index = s.Length)
  assert (pos.Index = index)
  tokens.ToArray()

let dumpTokens (tokens: (Token * Range) array) : unit =
  printfn "tokens (%d)" tokens.Length

  for (token, pos) in tokens do
    printfn "%A %O" pos token
