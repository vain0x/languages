module rec XbnfLang.Parse

open XbnfLang.Helpers
open XbnfLang.Types

let parseAtomTerm tokens =
  match tokens with
  | (StrToken content, l, r) :: tokens -> (TokenTerm content, (noAnnotation, (l, r))), tokens

  | (LoudToken ident, l, r) :: tokens -> (TokenTerm ident, (noAnnotation, (l, r))), tokens

  | (SnakeToken ident, l, r) :: tokens -> (SymbolTerm ident, (noAnnotation, (l, r))), tokens

  | (PunToken "@", l, _) :: (SnakeToken ident, _, r) :: (PunToken "(", _, _) :: tokens ->
    let body, tokens = parseTerm tokens

    let tokens =
      match tokens with
      | (PunToken ")", _, _) :: tokens -> tokens

      | _ ->
        // FIXME: missing closing paren
        tokens

    let annotation = ident, (l, r)
    termAddAnnotation annotation body, tokens

  | (PunToken "(", _, _) :: tokens ->
    let body, tokens = parseTerm tokens

    let tokens =
      match tokens with
      | (PunToken ")", _, _) :: tokens -> tokens

      | _ ->
        // FIXME: missing closing paren
        tokens

    body, tokens

  | _ -> failwithf "Expected Term at %A" tokens

let parseSuffixTerm tokens =
  let rec go first tokens =
    match tokens with
    | (PunToken "?", l, r) :: tokens -> go (OptTerm first, (noAnnotation, (l, r))) tokens

    | (PunToken "*", l, r) :: tokens -> go (ManyTerm first, (noAnnotation, (l, r))) tokens

    | (PunToken "+", l, r) :: tokens -> go (Many1Term first, (noAnnotation, (l, r))) tokens

    | (PunToken ",", l, _) :: (PunToken "*", _, r) :: tokens -> go (SepTerm(first, ","), (noAnnotation, (l, r))) tokens

    | (PunToken ",", l, _) :: (PunToken "+", _, r) :: tokens -> go (Sep1Term(first, ","), (noAnnotation, (l, r))) tokens

    | (PunToken ";", l, _) :: (PunToken "*", _, r) :: tokens -> go (SepTerm(first, ";"), (noAnnotation, (l, r))) tokens

    | (PunToken ";", l, _) :: (PunToken "+", _, r) :: tokens -> go (Sep1Term(first, ";"), (noAnnotation, (l, r))) tokens

    | _ -> first, tokens

  let first, tokens = parseAtomTerm tokens
  go first tokens

let parseConcatTerm tokens =
  let rec go first tokens =
    match tokens with
    | _ :: (PunToken "=", _, _) :: _ -> first, tokens

    | ((StrToken _
       | LoudToken _
       | SnakeToken _
       | PunToken "("),
       _,
       _) :: _ ->
      let second, tokens = parseSuffixTerm tokens
      go (ConcatTerm(first, second), (noAnnotation, noLocation)) tokens

    | _ -> first, tokens

  let first, tokens = parseSuffixTerm tokens
  go first tokens

let parseOrTerm tokens =
  let rec go first tokens =
    match tokens with
    | (PunToken "|", l, r) :: tokens ->
      let second, tokens = parseConcatTerm tokens
      go (OrTerm(first, second), (noAnnotation, (l, r))) tokens

    | _ -> first, tokens

  let first, tokens = parseConcatTerm tokens
  go first tokens

let parseTerm tokens : TermData * TokenData list = parseOrTerm tokens

let parseStmt tokens =
  match tokens with
  | (SnakeToken first, l, r) :: (PunToken "=", _, _) :: tokens ->
    let second, tokens = parseTerm tokens
    // FIXME: コメントをパースする
    RuleStmtTerm(first, second, [], (l, r)), tokens

  | _ -> failwithf "Expected stmt at %A" tokens

let parseSemi tokens =
  let rec go stmts tokens =
    match tokens with
    | []
    | [ (EofToken, _, _) ] -> stmts

    | _ ->
      let stmt, tokens = parseStmt tokens
      go (stmts |> cons stmt) tokens

  go [] tokens |> List.rev

let parse tokens = parseSemi tokens
