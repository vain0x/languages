module rec MiloneSyntaxV2.SyntaxParse

open MiloneSyntaxV2.Source
open MiloneSyntaxV2.Syntax

module C = MiloneStd.StdChar
module S = MiloneStd.StdString
module M = MiloneStd.StdMap

let private todo () = failwith "todo"

let private some (value, state) = Some value, state

let private map f (value, state) = f value, state

let private nameToPath name: APath = (name, []), []

// -----------------------------------------------
// Bp
// -----------------------------------------------

// Binding power.
[<NoEquality; NoComparison>]
type private Bp =
  | LogicalOrBp
  | LogicalAndBp
  | CompareBp
  | BitOrXorBp
  | BitAndBp
  | BitShiftBp
  | AddBp
  | MulBp

module private Bp =
  let minValue = LogicalOrBp

  let next bp =
    match bp with
    | LogicalOrBp -> Some LogicalAndBp
    | LogicalAndBp -> Some CompareBp
    | CompareBp -> Some BitOrXorBp
    | BitOrXorBp -> Some BitAndBp
    | BitAndBp -> Some BitShiftBp
    | BitShiftBp -> Some AddBp
    | AddBp -> Some MulBp
    | MulBp -> None

// -----------------------------------------------
// Context
// -----------------------------------------------

/// Parser context.
type private Px =
  { Tokens: TokenData list
    Errors: SyntaxError list }

let private ofTokens (tokens: TokenData list): Px = { Tokens = tokens; Errors = [] }

let private atEof (px: Px) =
  match px.Tokens with
  | TokenData (EofToken, _, _) :: _ -> true
  | _ -> false

let private peek1 (px: Px): Token * string =
  match px.Tokens with
  | (TokenData (token, text, _)) :: _ -> token, text

  | [] -> failwith "NEVER" // Sentinel should exist.

let private peek2 (px: Px): Token * string * Token * string =
  match px.Tokens with
  | TokenData (token1, text1, _) :: TokenData (token2, text2, _) :: _ -> token1, text1, token2, text2

  | []
  | [ _ ] -> failwith "NEVER" // Sentinel should exist.

let private currentPos (px: Px): Pos =
  match px.Tokens with
  | [] -> failwith "NEVER"
  | TokenData (_, _, pos) :: _ -> pos

let private error msg (px: Px) =
  let _, text, pos =
    match px.Tokens with
    | TokenData (token, text, pos) :: _ -> token, text, pos
    | [] -> failwith "NEVER" // Sentinel should exist.

  eprintfn "near: %A" (px.Tokens |> List.truncate 5)

  failwithf "Parse error: %s before %s `%s`" msg (Pos.toString pos) text

let private bump (px: Px): Pos * Px =
  match px.Tokens with
  | []
  | TokenData (EofToken, _, _) :: _ -> failwith "NEVER"

  | TokenData (_, _, pos) :: tokens -> pos, { px with Tokens = tokens }

let private skip (px: Px): Px = bump px |> snd

// -----------------------------------------------
// Parser combinator
// -----------------------------------------------

let private repeat p px =
  let rec go acc px =
    let valueOpt, px = p px

    match valueOpt with
    | Some value -> go (value :: acc) px
    | None -> List.rev acc, px

  go [] px

let private sepByComma parseItem px =
  let rec go acc px =
    let valueOpt, px = parseItem px

    match valueOpt with
    | None -> List.rev acc, px

    | Some value ->
        let px = eatPun "," px
        go (value :: acc) px

  go [] px

// -----------------------------------------------
// Misc
// -----------------------------------------------

let private eatPun text px =
  match peek1 px with
  | PunToken, punText when punText = text -> skip px
  | _ -> px

let private expectPun text px =
  match peek1 px with
  | PunToken, punText when punText = text -> skip px
  | _ -> error ("Expected '" + text + "'") px

let private andThenExpectPun text (value, px) =
  let px = expectPun text px
  value, px

let private unlessPun text p px =
  match peek1 px with
  | PunToken, punText when punText = text -> None, px
  | _ -> px |> p |> some

let private eatComma commaSeen (value, px) =
  match peek1 px with
  | PunToken, "," -> (value, true), skip px
  | _ -> (value, commaSeen), px

/// Tries to skip a comma or a semicolon.
let private eatSep px =
  match peek1 px with
  | PunToken, ","
  | PunToken, ";" -> skip px

  | _ -> px

let private parseName px =
  match peek1 px with
  | IdentToken, text ->
      let pos, px = bump px
      AName(text, pos), px

  | _ -> error "Expected name" px

let private parseModulePath px: AModulePath * Px =
  let head, px = parseName px

  let tail, px =
    px
    |> repeat
         (fun px ->
           match peek2 px with
           | PunToken, "::", IdentToken, _ -> px |> skip |> parseName |> some
           | _ -> None, px)

  (head, tail), px

let private parseGenericParams px: AName list * Px =
  px
  |> sepByComma
       (fun px ->
         match peek1 px with
         | IdentToken, _ -> px |> parseName |> some
         | _ -> None, px)

let private parseGenericParamList px: AGenericParamList * Px =
  let go px =
    px
    |> skip
    |> parseGenericParams
    |> andThenExpectPun "]"

  match peek2 px with
  | PunToken, "::", PunToken, "[" -> px |> skip |> go
  | PunToken, "[", _, _ -> px |> go
  | _ -> [], px

let private parseGenericArgs px: ATy list * Px =
  px |> sepByComma (unlessPun "]" parseTy)

let private parseGenericArgList px: AGenericArgList * Px =
  let go px =
    px
    |> skip
    |> parseGenericArgs
    |> andThenExpectPun "]"

  match peek2 px with
  | PunToken, "::", PunToken, "[" -> px |> skip |> go
  | PunToken, "[", _, _ -> px |> go
  | _ -> [], px

let private eatAscription px: ATy option * Px =
  match peek1 px with
  | PunToken, ":" -> px |> skip |> parseTy |> some
  | _ -> None, px

let private parseAscription px: ATy * Px =
  let tyOpt, px = eatAscription px

  match tyOpt with
  | Some ty -> ty, px
  | None -> error "Expected ': type'" px

let private parseResult px: AResult option * Px =
  match peek1 px with
  | PunToken, "->" ->
      let pos, px = bump px
      let ty, px = parseTy px
      Some(ty, pos), px

  | _ -> None, px

let private parseParams right px: AParam list * Px =
  let rec go acc px =
    match peek2 px with
    | PunToken, r, _, _ when r = right -> List.rev acc, px

    | IdentToken, _, PunToken, ":" ->
        let name, px = parseName px
        let ty, px = px |> skip |> parseTy
        let px = eatPun "," px
        go ((Some name, ty) :: acc) px

    | _ when atTy px ->
        let ty, px = parseTy px
        go ((None, ty) :: acc) px

    | _ -> error "Expected parameter" px

  go [] px

let private parseParamList px: AParamList * Px =
  match peek1 px with
  | PunToken, "(" ->
      let px = skip px
      let paramList, px = parseParams ")" px
      let px = px |> expectPun ")"
      paramList, px

  | _ -> [], px

let private parseArgs px: AArg list * Px =
  let rec go (acc: AArg list) px =
    match peek2 px with
    | PunToken, ")", _, _ -> List.rev acc, px

    | IdentToken, _, PunToken, ":" ->
        let name, px = parseName px
        let arg, px = px |> skip |> parseExpr
        let px = eatPun "," px
        go ((Some name, arg) :: acc) px

    | _ when atExpr px ->
        let arg, px = parseExpr px
        let px = eatPun "," px

        go ((None, arg) :: acc) px

    | _ -> acc, px

  let args, px = go [] px
  let px = expectPun ")" px
  args, px

/// keyword name paramList?
let private parseStmtHeader px: Pos * AName * AGenericParamList * Px =
  let pos, px = bump px
  let name, px = parseName px
  let genericParamList, px = parseGenericParamList px
  pos, name, genericParamList, px

// -----------------------------------------------
// Types
// -----------------------------------------------

let private atTy px =
  match peek1 px with
  | IdentToken, _
  | KeywordToken, "fn"
  | KeywordToken, "unit"
  | PunToken, "(" -> true

  | _ -> false

let private parseParenOrTupleTy px: ATy * Px =
  let pos, px = bump px

  let tys, px =
    px
    |> sepByComma (unlessPun ")" parseTy)
    |> andThenExpectPun ")"

  match tys with
  | [ ty ] -> ty, px
  | _ -> ATupleTy(tys, pos), px

let private parseAtomicTy px: ATy * Px =
  match peek1 px with
  | IdentToken, "_" -> px |> parseName |> map AInferTy

  | IdentToken, _ ->
      let path, px = parseModulePath px
      let genericArgList, px = parseGenericArgList px
      APathTy(path, genericArgList), px

  | KeywordToken, "unit" -> px |> bump |> map AUnitTy

  | PunToken, "(" -> parseParenOrTupleTy px

  | _ -> error "Expected type" px

let private parseFnTy px: AFnTy * Px =
  let pos, px = bump px

  let paramList, px = parseParamList px
  let result, px = parseResult px

  { ParamList = paramList
    Result = result
    Pos = pos },
  px

let private parseTy px =
  match peek1 px with
  | KeywordToken, "fn" -> parseFnTy px |> map AFnTy
  | _ -> parseAtomicTy px

// -----------------------------------------------
// Patterns
// -----------------------------------------------

let private parseParenOrTuplePat px: APat * Px =
  let pos, px = bump px

  let pats, px =
    px
    |> sepByComma (unlessPun ")" parsePat)
    |> andThenExpectPun ")"

  let pat =
    match pats with
    | [] -> ALitPat(AUnitLit, pos)
    | [ pat ] -> pat
    | _ -> ANodePat(ATuplePk, pats, pos)

  pat, px

let private parseListPat px: APat * Px =
  let pItems px =
    let rec go acc px =
      match peek1 px with
      | PunToken, "]" -> (List.rev acc, false), px

      | PunToken, ".." ->
          let pos, px = bump px

          let tail, px =
            match peek1 px with
            | PunToken, "]" -> AWildcardPat(AName("", pos)), px
            | _ -> parsePat px

          (List.rev (tail :: acc), true), px

      | _ ->
          let param, px = parsePat px
          let px = eatPun "," px
          go (param :: acc) px

    go [] px

  let pos, px = bump px
  let (pats, hasTail), px = pItems px
  let px = expectPun "]" px

  ANodePat(AListPk hasTail, pats, pos), px

let private parseVarOrPathPat px: APat * Px =
  let pParams px =
    let rec go acc px =
      match peek2 px with
      | PunToken, ")", _, _ -> List.rev acc, px

      | IdentToken, _, PunToken, ":" ->
          let name, px = parseName px
          let px = skip px
          let param, px = parsePat px
          go ((Some name, param) :: acc) px

      | _ ->
          let param, px = parsePat px
          let px = eatPun "," px
          go ((None, param) :: acc) px

    go [] px

  let pParamList px =
    match peek1 px with
    | PunToken, "(" ->
        let pos, px = bump px
        let paramList, px = pParams px
        let px = expectPun ")" px
        Some(paramList, pos), px

    | _ -> None, px

  let (head, tail), px = parseModulePath px
  let genericArgList, px = parseGenericArgList px
  let paramList, px = pParamList px

  match paramList with
  | None ->
      if List.isEmpty tail then
        AVarPat head, px
      else
        APathPat((head, tail), genericArgList), px

  | Some (paramList, pos) ->
      let callPat: ACallPat =
        { Callee = (head, tail), genericArgList
          ParamList = paramList
          Pos = pos }

      ACallPat callPat, px

let private parseAtomicPat px: APat * Px =
  let onLit lit px =
    let pos, px = bump px
    ALitPat(lit, pos), px

  match peek1 px with
  | IntToken, text -> onLit (AIntLit text) px
  | FloatToken, text -> onLit (AFloatLit text) px
  | CharToken, text -> onLit (ACharLit text) px
  | StringToken, text -> onLit (AStringLit text) px
  | KeywordToken, "false" -> onLit (ABoolLit false) px
  | KeywordToken, "true" -> onLit (ABoolLit true) px
  | KeywordToken, "unit" -> onLit AUnitLit px

  | IdentToken, "_" -> parseName px |> map AWildcardPat
  | IdentToken, _ -> parseVarOrPathPat px

  | PunToken, "(" -> parseParenOrTuplePat px
  | PunToken, "[" -> parseListPat px

  | _ -> error "Expected pattern" px

let private parseAsPat px: APat * Px =
  let next px = parseAtomicPat px

  match peek2 px with
  | IdentToken, _, PunToken, "@" ->
      let name, px = parseName px
      let pos, px = bump px
      let body, px = next px
      AAsPat { Name = name; Body = body; Pos = pos }, px

  | _ -> next px

let private parseOrPat px: APat * Px =
  let rec go l px =
    match peek1 px with
    | PunToken, "|" ->
        let pos, px = bump px
        let r, px = parseAsPat px
        go (AOrPat(l, r, pos)) px

    | _ -> l, px

  let l, px = parseAsPat px
  go l px

let private parsePat px: APat * Px = parseOrPat px

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private parseParenOrTupleExpr px: AExpr * Px =
  let pos, px = bump px

  let exprList, px =
    px
    |> sepByComma (unlessPun ")" parseExpr)
    |> andThenExpectPun ")"

  match exprList with
  | [] ->
      let pos, px = bump px
      ALitExpr(AUnitLit, pos), px

  | [ expr ] -> expr, px

  | _ -> ANodeExpr(ATupleEk, exprList, pos), px

let private parseListExpr px: AExpr * Px =
  let go px =
    let rec go acc px =
      match peek1 px with
      | PunToken, "]" -> List.rev acc, px

      | PunToken, ".." ->
          let pos, px = bump px
          let tail, px = parseExpr px
          let arg = ANodeExpr(ASpreadEk, [ tail ], pos)
          let px = eatPun "," px
          go (arg :: acc) px

      | _ ->
          let arg, px = parseExpr px
          let px = eatPun "," px
          go (arg :: acc) px

    go [] px

  let pos, px = bump px
  let args, px = go px
  let px = expectPun "]" px

  ANodeExpr(AListEk, args, pos), px

let private parseBlock px: ABlockExpr * Px =
  match peek1 px with
  | PunToken, "{" ->
      let start, px = bump px
      let stmts, px = parseStmts px
      let range = Range.ofPair (start, currentPos px)
      let px = expectPun "}" px

      { Stmts = stmts; Range = range }, px

  | _ -> error "Expected block" px

let private atAtomicExpr px =
  match peek1 px with
  | IntToken, _
  | FloatToken, _
  | CharToken, _
  | StringToken, _
  | IdentToken, _
  | KeywordToken, "false"
  | KeywordToken, "true"
  | KeywordToken, "unit"
  | PunToken, "("
  | PunToken, "[" -> true

  | _ -> false

let private parseAtomicExpr px =
  let onLit lit px =
    let pos, px = bump px
    ALitExpr(lit, pos), px

  match peek1 px with
  | IntToken, text -> onLit (AIntLit text) px
  | FloatToken, text -> onLit (AFloatLit text) px
  | CharToken, text -> onLit (ACharLit text) px
  | StringToken, text -> onLit (AStringLit text) px

  | IdentToken, _ ->
      let path, px = parseModulePath px

      let genericArgList, px =
        match peek1 px with
        | PunToken, "::" -> parseGenericArgList px
        | _ -> [], px

      APathExpr(path, genericArgList), px

  | KeywordToken, "false" -> onLit (ABoolLit false) px
  | KeywordToken, "true" -> onLit (ABoolLit true) px
  | KeywordToken, "unit" -> onLit AUnitLit px

  | PunToken, "(" -> parseParenOrTupleExpr px
  | PunToken, "[" -> parseListExpr px

  | _ ->
      assert (atExpr px
              || (eprintfn "atExpr px = false"
                  true))

      error "Expected an expression" px

let private parseSuffixExpr px =
  let rec go (l, px) =
    match peek1 px with
    | PunToken, "(" ->
        let pos, px = bump px
        let argList, px = parseArgs px

        let r =
          ACallExpr
            { Callee = l
              ArgList = argList
              Pos = pos }

        go (r, px)

    | PunToken, "[" ->
        let pos, px = bump px
        let arg, px = parseExpr px |> andThenExpectPun "]"
        let r = ANodeExpr(AIndexEk, [ l; arg ], pos)
        go (r, px)

    | _ -> l, px

  px |> parseAtomicExpr |> go

let private parsePrefixExpr px =
  match peek1 px with
  | PunToken, "-" ->
      let pos, px = bump px
      let arg, px = parseSuffixExpr px
      ANodeExpr(APrefixEk AMinusEp, [ arg ], pos), px

  | _ -> parseSuffixExpr px

let private parseBinaryExpr bp px =
  let go px =
    match Bp.next bp with
    | Some nextBp -> parseBinaryExpr nextBp px
    | None -> parsePrefixExpr px

  let l, px = go px

  let goRhs binary px =
    let pos, px = bump px
    let r, px = go px
    ANodeExpr(ABinaryEk binary, [ l; r ], pos), px

  match bp, peek1 px with
  | CompareBp, (PunToken, "==") -> goRhs AEqualEb px
  | CompareBp, (PunToken, "<>") -> goRhs ANotEqualEb px
  | CompareBp, (PunToken, "<") -> goRhs ALessEb px
  | CompareBp, (PunToken, "<=") -> goRhs ALessEqualEb px
  | CompareBp, (PunToken, ">") -> goRhs AGreaterEb px
  | CompareBp, (PunToken, ">=") -> goRhs AGreaterEqualEb px

  | BitAndBp, (PunToken, "&") -> goRhs ABitAndEb px
  | BitOrXorBp, (PunToken, "|") -> goRhs ABitOrEb px
  | BitOrXorBp, (PunToken, "^") -> goRhs ABitXorEb px
  | BitShiftBp, (PunToken, "<<") -> goRhs ALeftShiftEb px
  | BitShiftBp, (PunToken, ">>") -> goRhs ARightShiftEb px

  | AddBp, (PunToken, "+") -> goRhs AAddEb px
  | AddBp, (PunToken, "-") -> goRhs ASubEb px
  | MulBp, (PunToken, "*") -> goRhs AMulEb px
  | MulBp, (PunToken, "/") -> goRhs ADivEb px
  | MulBp, (PunToken, "%") -> goRhs AModuloEb px

  | LogicalAndBp, (PunToken, "&&") -> goRhs ALogicalAndEb px
  | LogicalOrBp, (PunToken, "||") -> goRhs ALogicalOrEb px

  | _ -> l, px

let private atExpr px =
  match peek1 px with
  | KeywordToken, "fn"
  | KeywordToken, "if"
  | KeywordToken, "match"
  | PunToken, "{"
  | PunToken, "|"
  | PunToken, "||" -> true

  | _ -> atAtomicExpr px

let private parseClosureExpr hasParamList px: AClosureExpr * Px =
  let start, px = bump px

  let paramList, px =
    if hasParamList then
      px |> parseParams "|" |> andThenExpectPun "|"
    else
      [], px

  let result, px = parseResult px

  let body, px =
    match result with
    | Some _ -> parseBlock px |> map ABlockExpr
    | _ -> parseExpr px

  let range = Range.ofPair (start, currentPos px)

  { ParamList = paramList
    Result = result
    Body = body
    Range = range },
  px

let private parseIfExpr px: AExpr * Px =
  let pos, px = bump px
  let cond, px = parseExpr px
  let body, px = parseBlock px |> map ABlockExpr

  let alt, px =
    match peek2 px with
    | KeywordToken, "else", KeywordToken, "if" -> px |> skip |> parseIfExpr |> some

    | KeywordToken, "else", _, _ -> px |> skip |> parseBlock |> map ABlockExpr |> some

    | _ -> None, px

  let args =
    match alt with
    | Some alt -> [ cond; body; alt ]
    | None -> [ cond; body ]

  ANodeExpr(AIfEk, args, pos), px

let private parseMatchClauses px: AMatchClause list * Px =
  let parseClause px: AMatchClause * Px =
    let start = currentPos px
    let pat, px = parsePat px

    let guard, px =
      match peek1 px with
      | KeywordToken, "if" -> px |> skip |> parseExpr |> some

      | _ -> None, px

    let body, px =
      match peek1 px with
      | PunToken, "{" -> parseBlock px |> map ABlockExpr
      | PunToken, "=>" -> px |> skip |> parseExpr
      | _ -> error "Expected '=> expr' or '{}'." px

    let range = Range.ofPair (start, currentPos px)
    let px = eatSep px

    { Pat = pat
      Guard = guard
      Body = body
      Range = range },
    px

  px |> repeat (unlessPun "}" parseClause)

let private parseMatchExpr px: AMatchExpr * Px =
  let pos, px = bump px
  let cond, px = parseExpr px

  let px = expectPun "{" px
  let clauses, px = parseMatchClauses px
  let px = expectPun "}" px

  { Cond = cond
    Clauses = clauses
    Pos = pos },
  px

let private parseExpr px =
  match peek1 px with
  | KeywordToken, "if" -> parseIfExpr px
  | KeywordToken, "match" -> parseMatchExpr px |> map AMatchExpr
  | PunToken, "{" -> parseBlock px |> map ABlockExpr
  | PunToken, "|" -> parseClosureExpr true px |> map AClosureExpr
  | PunToken, "||" -> parseClosureExpr false px |> map AClosureExpr
  | _ -> parseBinaryExpr Bp.minValue px

// -----------------------------------------------
// Statements
// -----------------------------------------------

let private atStmt px =
  match peek1 px with
  | KeywordToken, "fn"
  | KeywordToken, "let"
  | KeywordToken, "enum"
  | KeywordToken, "struct"
  | KeywordToken, "type"
  | KeywordToken, "mod"
  | KeywordToken, "use"
  | PunToken, ";" -> true

  | _ -> atExpr px

let private parseFnStmt px: AFnStmt * Px =
  let start, name, genericParamList, px = parseStmtHeader px
  let paramList, px = parseParamList px
  let result, px = parseResult px
  let block, px = parseBlock px

  { Name = name
    GenericParamList = genericParamList
    ParamList = paramList
    Result = result
    Body = ABlockExpr block
    Range = Range.ofPair (start, block.Range.End) },
  px

let private parseLetStmt px: ALetStmt * Px =
  let pos, px = bump px
  let pat, px = parsePat px
  let ty, px = eatAscription px
  let init, px = px |> expectPun "=" |> parseExpr
  let px = expectPun ";" px

  { Pat = pat
    Ty = ty
    Init = init
    Pos = pos },
  px

let private parsEnumStmt px: AEnumStmt * Px =
  let pVariant px: AVariantDecl * Px =
    let name, px = parseName px
    let fields, px = parseParamList px
    let px = eatSep px
    { Name = name; Fields = fields }, px

  let start, name, genericParamList, px = parseStmtHeader px
  let px = expectPun "{" px

  let variants, px = px |> repeat (unlessPun "}" pVariant)

  let range = Range.ofPair (start, currentPos px)
  let px = expectPun "}" px

  { Name = name
    GenericParamList = genericParamList
    Variants = variants
    Range = range },
  px

let private parseStructStmt px: AStructStmt * Px =
  let start, name, genericParamList, px = parseStmtHeader px
  let paramList, px = parseParamList px
  let range = Range.ofPair (start, currentPos px)
  let px = expectPun ";" px

  { Name = name
    GenericParamList = genericParamList
    Fields = paramList
    Range = range },
  px

let private parseTypeStmt px: ATypeStmt * Px =
  let start, name, genericParamList, px = parseStmtHeader px

  let body, px =
    match peek1 px with
    | PunToken, "=" -> px |> skip |> parseTy |> some
    | _ -> None, px

  let range = Range.ofPair (start, currentPos px)
  let px = eatPun ";" px

  { Name = name
    GenericParamList = genericParamList
    Body = body
    Range = range },
  px

let private parseModStmt px: AModStmt * Px =
  let start, px = bump px
  let name, px = parseName px

  let px = expectPun "{" px

  let body, px = px |> repeat (unlessPun "}" parseStmt)

  let range = Range.ofPair (start, currentPos px)
  let px = expectPun "}" px

  { Name = name; Body = body; Range = range }, px

let private parseUseStmt px: AUseStmt * Px =
  let rec parseUseTree px: AUseTree * Px =
    let parseUseName px: AUseTree * Px =
      let name, px = parseName px

      let aliasOpt, px =
        match peek1 px with
        | KeywordToken, "as" -> px |> skip |> parseName |> some
        | _ -> None, px

      AUseName(name, aliasOpt), px

    let parseUsePath px: AUseTree * Px =
      let head, px = parseName px
      let pos, px = bump px
      let tail, px = parseUseTree px
      AUsePath(head, tail, pos), px

    let parseUseGroup px: AUseTree * Px =
      let pos, px = bump px

      let children, px =
        sepByComma (unlessPun "}" parseUseTree) px

      let px = expectPun "}" px
      AUseGroup(children, pos), px

    match peek2 px with
    | PunToken, "{", _, _ -> parseUseGroup px
    | IdentToken, _, PunToken, "::" -> parseUsePath px
    | IdentToken, _, _, _ -> parseUseName px
    | _ -> error "Expected use tree" px

  let pos, px = bump px
  let body, px = parseUseTree px
  let px = eatPun ";" px

  { Body = body; Pos = pos }, px

let private parseStmt px =
  match peek1 px with
  | KeywordToken, "fn" -> px |> parseFnStmt |> map AFnStmt
  | KeywordToken, "let" -> px |> parseLetStmt |> map ALetStmt

  | KeywordToken, "enum" -> px |> parsEnumStmt |> map AEnumStmt
  | KeywordToken, "struct" -> px |> parseStructStmt |> map AStructStmt
  | KeywordToken, "type" -> px |> parseTypeStmt |> map ATypeStmt
  | KeywordToken, "mod" -> px |> parseModStmt |> map AModStmt
  | KeywordToken, "use" -> px |> parseUseStmt |> map AUseStmt

  | PunToken, ";" -> parseStmt (skip px)

  | _ ->
      let expr, px = parseExpr px
      let px = eatPun ";" px
      AExprStmt expr, px

// -----------------------------------------------
// Root
// -----------------------------------------------

let private parseStmts (px: Px) =
  let rec go acc px =
    if atStmt px then
      let stmt, px = parseStmt px
      go (stmt :: acc) px
    else
      List.rev acc, px

  go [] px

let private parseRoot (px: Px) =
  let rec go acc px =
    if atEof px then
      List.rev acc, px
    else if atStmt px then
      let stmt, px = parseStmt px
      go (stmt :: acc) px
    else
      error "Expected stmt or EOF" px

  go [] px

let parseTokens (tokens: TokenData list): ARoot =
  let px = ofTokens tokens

  let start = currentPos px
  let stmts, px = parseRoot px
  let range = Range.ofPair (start, currentPos px)
  { Stmts = stmts; Range = range }
