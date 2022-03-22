module rec Linear.Parse

open Linear.Ast
open Linear.Location
open Linear.Token

let private sTick = ref 0

let private checkInfiniteLoop =
  fun () ->
    sTick.Value <- sTick.Value + 1
    assert (sTick.contents < 1000000)

// -----------------------------------------------
// Context
// -----------------------------------------------

/// Parser context.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Px =
  { Tokens: TokenData array
    Index: int
    Eof: TokenData }

let private ofTokens (tokens: TokenData array) : Px =
  sTick.contents <- 0

  let endPos =
    if tokens.Length >= 1 then
      let _, _, range = tokens.[tokens.Length - 1]
      range.End
    else
      Pos.zero

  let eofToken: TokenData = EofToken, "", Range.ofPos endPos

  { Tokens = tokens
    Index = 0
    Eof = eofToken }

let private doGet (offset: int) (px: Px) : TokenData =
  assert (offset = 0 || offset = 1)

  if px.Index + offset < px.Tokens.Length then
    px.Tokens.[px.Index + offset]
  else
    px.Eof

let private get0 (px: Px) : TokenData = doGet 0 px
let private get1 (px: Px) : TokenData = doGet 1 px

let private doPeek (offset: int) (px: Px) : Token =
  assert (offset = 0 || offset = 1)
  checkInfiniteLoop ()

  if px.Index + offset < px.Tokens.Length then
    let token, _, _ = px.Tokens.[px.Index + offset]
    token
  else
    EofToken

let private peek0 (px: Px) : Token = doPeek 0 px
let private peek1 (px: Px) : Token = doPeek 1 px

let private bump (px: Px) : Px =
  assert (px.Index < px.Tokens.Length)
  // eprintfn "trace: bump %d %A" px.Index (px.Tokens.[px.Index])
  { px with Index = px.Index + 1 }

let private atEof (px: Px) : bool =
  assert (px.Index <= px.Tokens.Length)
  checkInfiniteLoop ()
  px.Index = px.Tokens.Length

let private lastPos (px: Px) : Pos =
  if px.Index >= 1 then
    let _, _, range = px.Tokens.[px.Index - 1]
    range.End
  else
    Pos.zero

let private startRange px : Pos =
  let _, _, range = get0 px
  range.Start

let private endRange start (px: Px) : Range =
  if px.Index >= 1 then
    let _, _, range = px.Tokens.[px.Index - 1]

    if Pos.compare start range.End <= 0 then
      Range.ofPair (start, range.End)
    else
      Range.ofPos start
  else
    Range.ofPos start

let private fail message (px: Px) : 'A =
  let _, _, range = get0 px
  failwithf "Parse error: %s near (%O)" message range

// -----------------------------------------------
// Misc
// -----------------------------------------------

let private eatPun (pun: string) px =
  match get0 px with
  | PunToken, text, _ when text = pun -> true, bump px
  | _ -> false, px

let private expectPun (pun: string) px =
  match get0 px with
  | PunToken, text, _ when text = pun -> bump px
  | _ -> fail ("Expected '" + pun + "'") px

let private expectKeyword (keyword: string) px =
  match get0 px with
  | KeywordToken, text, _ when text = keyword -> bump px
  | _ -> fail ("Expected '" + keyword + "'") px

let private parseName px =
  match get0 px with
  | (IdentToken, _, _) as token -> AName token, bump px
  // | (GreekToken, _, _) as token -> AName token, bump px
  | _ -> fail "Expected Name" px

let private skipSemi px =
  match get0 px with
  | PunToken, ";", _ -> skipSemi (bump px)
  | _ -> px

// -----------------------------------------------
// Types
// -----------------------------------------------

let private parseAtomicTy px : ATy * Px =
  match get0 px with
  | IdentToken, _, _ ->
    let name, px = parseName px

    match get0 px with
    | PunToken, "<", _ ->
      let tyArg, px = parseTy (bump px)
      let px = expectPun ">" px
      ALinearTy tyArg, px

    | _ -> ANameTy name, px

  | PunToken, "(", _ ->
    let ty, px = parseTy (bump px)
    let px = expectPun ")" px
    ty, px

  | _ -> fail "Expected type" px

let private parsePairTy px : ATy * Px =
  let l, px = parseAtomicTy px

  // Non-associative. `T * U * V` is error.
  match get0 px with
  | PunToken, "*", _ ->
    let r, px = parseAtomicTy (bump px)
    APairTy(l, r), px

  | _ -> l, px

let private parseArrowTy px : ATy * Px =
  let l, px = parsePairTy px

  match get0 px with
  | PunToken, "->", _ ->
    let r, px = parseArrowTy (bump px)
    AFunTy(l, r), px

  | _ -> l, px

let private parseTy px : ATy * Px = parseArrowTy px

// -----------------------------------------------
// Patterns
// -----------------------------------------------

let private atFirstOfPat px =
  match get0 px with
  | IdentToken, _, _
  | PunToken, "(", _ -> true

  | _ -> false

let private parseAtomicPat px : APat * Px =
  match get0 px with
  | IdentToken, "_", range -> AWildcardPat range.Start, bump px

  | IdentToken, _, _ ->
    let name, px = parseName px

    if atFirstOfPat px then
      let payload, px = parseAtomicPat px
      AWrapPat(name, payload), px
    else
      ANamePat name, px

  | PunToken, "(", _ ->
    let px = bump px

    match get0 px with
    | PunToken, ")", _ -> AUnitPat(lastPos px), px

    | _ ->
      let bodyPat, px = parsePat px
      let px = expectPun ")" px
      bodyPat, px

  | _ -> fail "Expected a pattern" px

let private parsePairPat px : APat * Px =
  let l, px = parseAtomicPat px

  match get0 px with
  | PunToken, ",", range ->
    let r, px = parseAtomicPat (bump px)
    APairPat(l, r, range.Start), px

  | _ -> l, px

let private parsePat px : APat * Px = parsePairPat px

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private atFirstOfAtomicExpr px =
  match get0 px with
  | IntToken, _, _
  | IdentToken, _, _
  | PunToken, "(", _ -> true

  | _ -> false

let private atFirstOfExpr px =
  match get0 px with
  | KeywordToken, "if", _
  | KeywordToken, "let", _ -> true

  | _ -> atFirstOfAtomicExpr px

let private parseAtomicExpr px : AExpr * Px =
  match get0 px with
  | IntToken, text, range -> AIntExpr(int text, range), bump px

  | IdentToken, _, _ ->
    let name, px = px |> parseName
    ANameExpr name, px

  | PunToken, "(", r1 ->
    let px = bump px

    match get0 px with
    | PunToken, ")", r2 -> AUnitExpr(Range.join r1 r2), bump px

    | _ ->
      let bodyExpr, px = parseExpr px
      let px = expectPun ")" px
      bodyExpr, px

  | _ -> fail "Expected an expression" px

let private parseAppExpr px =
  let rec go l px =
    if atFirstOfAtomicExpr px then
      let pos = lastPos px
      let r, px = parseAtomicExpr px
      go (AAppExpr(l, r, pos)) px
    else
      l, px

  let l, px = parseAtomicExpr px
  go l px

let private parseBinaryExpr px =
  let l, px = parseAppExpr px

  let binaryOpt =
    match get0 px with
    | PunToken, "+", _ -> Some Binary.Add
    | PunToken, "=", _ -> Some Binary.Equal
    | _ -> None

  match binaryOpt with
  | Some binary ->
    let r, px = parseAppExpr (bump px)
    ABinaryExpr(binary, l, r), px

  | _ -> l, px

let private parsePairExpr px =
  let l, px = parseBinaryExpr px

  match get0 px with
  | PunToken, ",", _ ->
    let r, px = parseBinaryExpr (bump px)
    ABinaryExpr(Binary.Pair, l, r), px

  | _ -> l, px

let private parseIfExpr px =
  let start = startRange px
  let px = bump px
  let cond, px = parseExpr px
  let px = expectKeyword "then" px
  let thenClause, px = parseClause px
  let px = expectKeyword "else" px
  let elseClause, px = parseClause px
  let px = expectKeyword "end" px
  let range = endRange start px
  AIfExpr(cond, thenClause, elseClause, range), px

let private parseLetExpr px =
  let start = startRange px
  let px = bump px
  let pat, px = parsePat px
  let px = expectPun "=" px
  let init, px = parseBlockOrExpr px
  let range = endRange start px
  ALetExpr(pat, init, range), px

let private parseExpr px : AExpr * Px =
  match get0 px with
  | KeywordToken, "if", _ -> parseIfExpr px
  | KeywordToken, "let", _ -> parseLetExpr px
  | _ -> parsePairExpr px

// -----------------------------------------------
// Clauses and Blocks
// -----------------------------------------------

let private atEndOfClause px =
  match get0 px with
  | EofToken, _, _
  | KeywordToken, "else", _
  | KeywordToken, "end", _
  | PunToken, "}", _ -> true

  | _ -> false

let private parseClause px : AExpr * Px =
  // Although the grammar is:
  //    `(;* expr)* ;* last? ;*`,
  // the implementation parses:
  //    `;* first? (;* expr)* ;*`.

  let rec go acc last px =
    let px = skipSemi px

    if atEndOfClause px then
      acc, last, px
    else
      let expr, px = parseExpr px
      go (last :: acc) expr px

  let px = skipSemi px

  if atFirstOfExpr px then
    let start = startRange px
    let expr, px = parseExpr px
    let acc, last, px = go [] expr px
    let range = endRange start px
    let px = skipSemi px

    if List.isEmpty acc then
      last, px
    else
      ABlockExpr(List.rev acc, last, range), px
  else
    AUnitExpr(Range.ofPos (lastPos px)), px

/// `{ clause }`
let private parseBlockExpr px : AExpr * Px =
  let px = expectPun "{" px
  let bodyExpr, px = parseClause px
  let px = expectPun "}" px
  bodyExpr, px

/// `{ clause } | expr`
let private parseBlockOrExpr px : AExpr * Px =
  let brace, px = eatPun "{" px

  if brace then
    let bodyExpr, px = parseClause px
    let px = expectPun "}" px
    bodyExpr, px
  else
    parseExpr px

// -----------------------------------------------
// Declarations
// -----------------------------------------------

let private parseFunDecl px : ADecl * Px =
  let parseParam px =
    let px = expectPun "(" px
    let name, px = parseName px
    let px = expectPun ":" px
    let ty, px = parseTy px
    let px = expectPun ")" px
    (name, ty), px

  let rec parseParamList px =
    let rec go acc px =
      match get0 px with
      | PunToken, "(", _ ->
        let p, px = parseParam px
        go (p :: acc) px

      | _ -> List.rev acc, px

    match get0 px, get1 px with
    | (PunToken, "(", _), (PunToken, ")", _) -> [], bump (bump px)
    | _ -> go [] px

  let start = startRange px
  let name, px = parseName (bump px)
  let paramList, px = parseParamList px
  let px = expectPun ":" px
  let resultTy, px = parseTy px
  let px = expectPun "=" px
  let body, px = parseBlockExpr px
  let range = endRange start px
  AFunDecl(name, paramList, resultTy, body, range), px

let private parseTypeDecl px =
  let start = startRange px
  let name, px = parseName (bump px)
  let px = expectPun "=" px
  let variant, px = parseName px
  let px = expectKeyword "of" px
  let payloadTy, px = parseTy px
  let range = endRange start px
  ANewtypeDecl(name, variant, payloadTy, range), px

let private parseExpectDecl px : ADecl * Px =
  let start = startRange px
  let px = bump px

  let (_, desc, _), px =
    if peek0 px <> StringToken then
      fail "Expected string" px

    get0 px, bump px

  let body, px = parseBlockExpr px
  let range = endRange start px
  AExpectDecl(desc, body, range), px

let private parseExpectErrorDecl px : ADecl * Px =
  let start = startRange px
  let px = bump px

  let (_, desc, _), px =
    if peek0 px <> StringToken then
      fail "Expected string" px

    get0 px, bump px

  let body, px = parseBlockExpr px
  let range = endRange start px
  AExpectErrorDecl(desc, body, range), px

let private parseDecl px : ADecl * Px =
  match get0 px with
  | KeywordToken, "let", _ -> parseFunDecl px
  | KeywordToken, "type", _ -> parseTypeDecl px
  | KeywordToken, "expect", _ -> parseExpectDecl px
  | KeywordToken, "expect_error", _ -> parseExpectErrorDecl px
  | _ -> fail "Expected a declaration" px

let private parseRoot (px: Px) : ADecl list =
  let rec go acc px =
    let px = skipSemi px

    if atEof px then
      List.rev acc
    else
      let decl, px = parseDecl px
      go (decl :: acc) px

  go [] px

// -----------------------------------------------
// Interface
// -----------------------------------------------

let parseTokens (tokens: TokenData array) : ARoot =
  let px = ofTokens tokens
  let decls = parseRoot px
  ARoot decls
