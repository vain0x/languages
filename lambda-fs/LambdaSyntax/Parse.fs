module rec LambdaSyntax.Parse

open System.Collections.Generic
open LambdaDomain.Location
open LambdaSyntax.Token
open LambdaSyntax.Syntax

let private some result =
  let value, px = result
  Some value, px

let private none px = None, px

// -----------------------------------------------
// InternalParseEvent
// -----------------------------------------------

// See <https://github.com/rust-analyzer/rust-analyzer/blob/50a02eb3593591a02677e1b56f24d7ff0459b9d0/crates/parser/src/event.rs>

[<Struct; NoEquality; NoComparison>]
type StartNode = StartNode of int

[<Struct; NoEquality; NoComparison>]
type FinishNode = FinishNode of int

[<RequireQualifiedAccess; Struct; NoEquality; NoComparison>]
type InternalParseEvent =
  /// forwardParent = ValueSome -1 is tombstone.
  | Start of kind: SyntaxKind * forwardParent: int option
  | Finish of startEventIndex: int
  | Token of tokenIndex: int

let private tombstone =
  InternalParseEvent.Start(RootSN, Some(-1))

let private isTombstone e =
  match e with
  | InternalParseEvent.Start (_, Some (-1)) -> true
  | _ -> false

let private finishEvents tokenCount (src: InternalParseEvent []): ParseEvent [] =
  let output = ResizeArray()

  let parents = ResizeArray()
  let stack = Stack()
  let mutable tokenIndex = 0

  for ei in 0 .. src.Length - 1 do
    match src.[ei] with
    | InternalParseEvent.Start (_, Some (-1)) -> ()

    | InternalParseEvent.Start (kind, forwardParentOpt) ->
        assert (parents.Count = 0)
        parents.Add(ei, kind)

        // Process forward parents first.
        let mutable opt = forwardParentOpt

        while opt |> Option.isSome do
          let p = Option.get opt

          match src.[p] with
          | InternalParseEvent.Start (kind, forwardParentOpt) ->
              parents.Add(p, kind)
              opt <- forwardParentOpt
              src.[p] <- tombstone

          | _ -> opt <- None

        for pi = parents.Count - 1 downto 0 do
          let ei, kind = parents.[pi]
          stack.Push(ei)
          output.Add(StartEvent kind)

        parents.Clear()

    | InternalParseEvent.Finish startIndex ->
        assert (stack.Count >= 1)
        let ei = stack.Pop()
        assert (ei = startIndex)

        output.Add(FinishEvent)

    | InternalParseEvent.Token ti ->
        assert (tokenIndex = ti)
        tokenIndex <- tokenIndex + 1

        output.Add(TokenEvent)

  assert (stack.Count = 0)
  assert (tokenIndex = tokenCount)
  output.ToArray()

// -----------------------------------------------
// Context
// -----------------------------------------------

/// Parser context.
[<NoEquality; NoComparison>]
type private Px =
  { Tokens: TokenData []
    Index: int
    Eof: TokenData
    Events: ResizeArray<InternalParseEvent>
    Errors: ResizeArray<ParseError> }

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
    Eof = eofToken
    Events = ResizeArray()
    Errors = ResizeArray() }

let private addError msg (px: Px) =
  let _, _, range = get 0 px
  px.Errors.Add(ParseError(msg, range))
  px

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
  if px.Index < px.Tokens.Length then
    px.Events.Add(InternalParseEvent.Token 1)
    { px with Index = px.Index + 1 }
  else
    px

let private startNode (px: Px): StartNode * Px =
  let i = px.Events.Count
  px.Events.Add(tombstone)
  StartNode i, px

let private finishNode (StartNode startIndex) kind (px: Px): FinishNode * Px =
  assert (isTombstone px.Events.[startIndex])
  px.Events.[startIndex] <- InternalParseEvent.Start(kind, None)

  px.Events.Add(InternalParseEvent.Finish startIndex)
  FinishNode startIndex, px

let private precede (FinishNode startIndex) (px: Px): StartNode * Px =
  let newStartIndex = px.Events.Count
  px.Events.Add(InternalParseEvent.Start(RootSN, Some startIndex))
  StartNode newStartIndex, px

let private withNode kind p px =
  let n, px = startNode px
  px |> p |> finishNode n kind

// -----------------------------------------------
// Misc
// -----------------------------------------------

let private expectPun text px =
  match get 0 px with
  | PunToken, punText, _ when punText = text -> bump px
  | _ -> px |> addError ("Expected '" + text + "'")

let private parseName px =
  match at 0 px with
  | IdentToken -> px |> withNode NameSN bump |> snd
  | _ -> px

let private parseAscription px =
  match get 0 px with
  | PunToken, ":", _ -> px |> bump |> parseTy
  | _ -> px

// -----------------------------------------------
// Types
// -----------------------------------------------

let private parseAtomicTy px =
  match get 0 px with
  // | IdentToken, _, _ -> px |> withNode NameTySN bump |> some

  | GreekToken, _, _ -> px |> withNode UnivTySN bump |> some

  | PunToken, "(", _ ->
      px
      |> withNode ParenTySN (fun px -> px |> bump |> parseTy |> expectPun ")")
      |> some

  | _ -> px |> addError "Expected type" |> none

let private parseArrowTy px =
  let l, px = parseAtomicTy px

  match l with
  | None -> px
  | Some l ->
      match get 0 px with
      | PunToken, "->", _ ->
          let n, px = px |> precede l
          let px = px |> bump |> parseArrowTy
          px |> finishNode n ArrowTySN |> snd

      | _ -> px

let private parseTy px = parseArrowTy px

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private parseAtomicExpr px =
  match get 0 px with
  | IdentToken, _, _ -> px |> withNode NameExprSN bump |> some

  | PunToken, "(", _ ->
      px
      |> withNode ParenExprSN (fun px -> px |> bump |> parseExpr |> expectPun ")")
      |> some

  | PunToken, "\\", _ ->
      px
      |> withNode
           LambdaExprSN
           (fun px ->
             px
             |> bump
             |> parseName
             |> expectPun "."
             |> parseExpr)
      |> some

  | _ -> px |> addError "Expected an expression" |> none

let private parseAppExpr px =
  let rec go (l, px) =
    let n, px = px |> precede l
    let r, px = px |> parseAtomicExpr

    match r with
    | None -> Some l, px
    | Some _ -> px |> finishNode n AppExprSN |> go

  let l, px = parseAtomicExpr px

  match l with
  | None -> None, px
  | Some l -> go (l, px)

let private parseBinding px =
  match get 0 px with
  | KeywordToken, "let", _ ->
      px
      |> withNode
           LetExprSN
           (fun px ->
             let px =
               px
               |> bump
               |> parseName
               |> expectPun "="
               |> parseExpr

             match get 0 px with
             | KeywordToken, "in", _ -> px |> bump |> parseExpr
             | _ -> px)
      |> some

  | KeywordToken, "type_assert", _ ->
      px
      |> withNode TypeAssertSN (fun px -> px |> bump |> parseExpr |> parseAscription)
      |> some

  | KeywordToken, "type_error", _ ->
      px
      |> withNode TypeErrorSN (fun px -> px |> bump |> parseExpr)
      |> some

  | _ -> parseAppExpr px

let private parseExpr px =
  let l, px = parseBinding px

  match l, get 0 px with
  | Some l, (PunToken, ";", _) ->
      let n, px = px |> precede l

      let rec go px =
        let l, px = px |> parseBinding

        match l, get 0 px with
        | Some _, (PunToken, ";", _) -> px |> bump |> go
        | _ -> px

      px
      |> bump
      |> go
      |> finishNode n BlockExprSN
      |> snd

  | _ -> px

// -----------------------------------------------
// Root
// -----------------------------------------------

let private parseRoot (px: Px) =
  let rec go px =
    if atEof px |> not then
      parseExpr px |> go

  go px

let parseTokens (tokens: TokenData []): ParseResult =
  let tokenCount = tokens.Length
  let px = ofTokens tokens

  { Events = px.Events.ToArray() |> finishEvents tokenCount
    Errors = px.Errors.ToArray() }
