module rec TypeCheck.Parser

open TypeCheck.Helpers
open TypeCheck.Syntax

let private Trace = false

exception ParseException of message: string * token: Token * range: Range

/// Parser context.
type Px(tokens: (Token * Range) array) =
  let mutable index = 0

  member _.Nth(n) : Token =
    if index + n < tokens.Length then
      tokens[index + n] |> fst
    else
      EofToken

  member this.Next: Token = this.Nth(0)

  member _.NextRange: Range =
    if index < tokens.Length then
      tokens[index] |> snd
    else
      tokens[tokens.Length - 1] |> snd

  member _.Shift() : Token =
    assert (index + 1 <= tokens.Length)

    let token, tokenPos = tokens[index]
    index <- index + 1

    if Trace then
      eprintfn "shift %A (%A)" token tokenPos

    token

let private parseError (message: string) (px: Px) =
  raise (ParseException(message, px.Next, px.NextRange))

let private eat (token: Token) (px: Px) : bool =
  if px.Next = token then
    px.Shift() |> ignore
    true
  else
    false

let expectIdent (px: Px) : string =
  match px.Next with
  | IdentToken name ->
    px.Shift() |> ignore
    name

  | _ -> parseError "Expected identifier" px

let expectToken (token: Token) (px: Px) : unit =
  if px.Next = token then
    px.Shift() |> ignore
  else
    parseError (sprintf "Expected %A" token) px

let expectTerm (px: Px) : Term =
  match px |> parseTerm with
  | Some term -> term
  | None -> parseError ("Expected term") px

let parseRecordArgs (px: Px) : (string * Term) list =
  let rec go1 acc =
    match px.Next with
    | EofToken
    | ParenToken "}" -> acc

    | IdentToken name ->
      px.Shift() |> ignore

      px |> expectToken (PunToken "=")

      let arg = px |> expectTerm

      let acc = (name, arg) :: acc
      go2 acc

    | _ -> parseError "Expected field name" px

  and go2 acc =
    match px.Next with
    | EofToken
    | ParenToken "}" -> acc
    | _ ->
      px |> expectToken (PunToken ";")
      go1 acc

  go1 [] |> List.rev

let parseAtomicTerm (px: Px) : Term option =
  match px.Next with
  | KeywordToken "true" ->
    px.Shift() |> ignore
    BoolLitTerm true |> Some

  | KeywordToken "false" ->
    px.Shift() |> ignore
    BoolLitTerm false |> Some

  | IntToken value ->
    px.Shift() |> ignore
    IntLitTerm value |> Some

  | IdentToken name ->
    px.Shift() |> ignore
    VarTerm name |> Some

  | ParenToken "(" ->
    px.Shift() |> ignore
    let body = px |> expectTerm
    px |> expectToken (ParenToken ")")

    Some body

  | ParenToken "{" ->
    px.Shift() |> ignore
    let fields = px |> parseRecordArgs
    px |> expectToken (ParenToken "}")

    RecordTerm fields |> Some

  | _ -> None

let parseSelectTerm (px: Px) : Term option =
  let rec go (lhs: Term) =
    match px.Next with
    | PunToken "." ->
      px.Shift() |> ignore
      let name = px |> expectIdent
      SelectTerm(lhs, name) |> go

    | _ -> lhs

  match parseAtomicTerm px with
  | Some lhs -> go lhs |> Some
  | None -> None

let parseAppTerm (px: Px) : Term option =
  let rec go (lhs: Term) =
    match px |> parseSelectTerm with
    | None -> lhs

    | Some rhs ->
      let term = AppTerm(lhs, rhs)
      go term

  match px |> parseSelectTerm with
  | Some lhs -> go lhs |> Some
  | None -> None

let parseTerm (px: Px) : Term option =
  match px.Next with
  | KeywordToken "if" ->
    px.Shift() |> ignore

    let cond = px |> expectTerm

    px |> expectToken (KeywordToken "then")
    let thenTerm = px |> expectTerm

    px |> expectToken (KeywordToken "else")
    let elseTerm = px |> expectTerm

    // IfTerm(cond, thenTerm, elseTerm) |> Some
    AppTerm(AppTerm(AppTerm(VarTerm "if", cond), thenTerm), elseTerm) |> Some

  | KeywordToken "fun" ->
    px.Shift() |> ignore

    let name = px |> expectIdent

    px |> expectToken (PunToken "->")
    let body = px |> expectTerm

    LambdaTerm(name, body) |> Some

  | KeywordToken "let" ->
    px.Shift() |> ignore

    let isRec = if px |> eat (KeywordToken "rec") then Rec else NotRec

    let name = px |> expectIdent

    px |> expectToken (PunToken "=")
    let rhs = px |> expectTerm

    px |> expectToken (KeywordToken "in")
    let body = px |> expectTerm

    LetTerm(isRec, name, rhs, body) |> Some

  | _ -> px |> parseAppTerm

let parseDecl (px: Px) : Decl option =
  match px.Next with
  | KeywordToken "let" ->
    px.Shift() |> ignore

    let isRec = if px |> eat (KeywordToken "rec") then Rec else NotRec

    let name = px |> expectIdent
    px |> expectToken (PunToken "=")

    let rhs = px |> expectTerm
    px |> eat (PunToken ";") |> ignore

    { IsRec = isRec
      Name = name
      Rhs = rhs }
    |> Some

  | _ -> None

let parseDecls (px: Px) : Decl list =
  let rec go acc =
    match px.Next with
    | EofToken -> acc
    | _ ->
      match px |> parseDecl with
      | Some def -> go (def :: acc)
      | None -> parseError "Expected definition" px

  go [] |> List.rev

let parseTokens (tokens: (Token * Range) array) : Ast =
  let px = Px(tokens)
  let decls = parseDecls px

  if px.Next <> EofToken then
    parseError "Expected EOF" px

  { Decls = decls }
