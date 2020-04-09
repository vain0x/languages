module rec PhancieLang.SyntaxParse

open PhancieLang.Helpers
open PhancieLang.Syntax
open PhancieLang.SyntaxParseContext

type P = ParseContext

let parseBoolLiteralTerm (p: P) =
  assert (p.Next = FalseToken || p.Next = TrueToken)

  p.StartNode()
  (p.Eat(FalseToken) || p.Eat(TrueToken)) |> is true
  p.EndNode(BoolLiteralNode)

let parseIntLiteralTerm (p: P) =
  assert (p.Next = IntToken)

  p.StartNode()
  p.Bump()
  p.EndNode(IntLiteralNode)

let parseStrLiteralTerm (p: P) =
  assert (p.Next = StrStartToken)

  p.StartNode()
  p.Bump()

  while p.Eat(StrVerbatimToken) do
    ()

  p.Eat(StrEndToken) |> is true

  p.EndNode(StrLiteralNode)

let parseNameTerm (p: P) =
  assert (p.Next = IdentToken)

  p.StartNode()
  p.Bump()
  p.EndNode(NameNode)

let parseIfTerm (p: P) =
  assert (p.Next = IfToken)

  p.StartNode()
  p.Bump()

  // cond
  parseHeadTerm p

  // body
  if p.Next = LeftBraceToken then
    parseBlockTerm p
  else
    p.AddError(ExpectedError "ブロック")

  // alt
  if p.Next = ElseToken then
    p.StartNode()
    p.Bump()

    match p.Next with
    | IfToken ->
      parseIfTerm p

    | LeftBraceToken ->
      parseBlockTerm p

    | _ ->
      p.AddError(ExpectedError "ブロック")

    p.EndNode(ElseNode)

  p.EndNode(IfNode)

let parseBreakTerm (p: P) =
  assert (p.Next = BreakToken)

  p.StartNode()
  p.Bump()
  p.EndNode(BreakNode)

let parseContinueTerm (p: P) =
  assert (p.Next = ContinueToken)

  p.StartNode()
  p.Bump()
  p.EndNode(ContinueNode)

let parseLoopTerm (p: P) =
  assert (p.Next = LoopToken)

  p.StartNode()
  p.Bump()

  if p.Next = LeftBraceToken then
    parseBlockTerm p
  else
    p.AddError(ExpectedError "ブロック")
  p.EndNode(LoopNode)

let parseWhileTerm (p: P) =
  assert (p.Next = WhileToken)

  p.StartNode()
  p.Bump()

  // cond
  parseHeadTerm p

  // body
  if p.Next = LeftBraceToken then
    parseBlockTerm p
  else
    p.AddError(ExpectedError "ブロック")

  p.EndNode(WhileNode)

let parseGroupTerm (p: P) =
  p.StartNode()

  p.Eat(LeftParenToken) |> is true

  parseTerm p

  if p.Eat(RightParenToken) |> not then
    p.AddError(ExpectedError "右カッコ")

  p.EndNode(GroupNode)

let parseBlockTerm (p: P) =
  p.StartNode()

  p.Eat(LeftBraceToken) |> is true

  parseSemi p

  if p.Eat(RightBraceToken) |> not then
    p.AddError(ExpectedError "右カッコ")

  p.EndNode(BlockNode)

let parseAtomTerm (p: P) =
  match p.Next with
  | FalseToken
  | TrueToken ->
    parseBoolLiteralTerm p

  | IntToken ->
    parseIntLiteralTerm p

  | StrStartToken ->
    parseStrLiteralTerm p

  | IdentToken ->
    parseNameTerm p

  | LeftParenToken ->
    parseGroupTerm p

  | LeftBraceToken ->
    parseBlockTerm p

  | IfToken ->
    parseIfTerm p

  | BreakToken ->
    parseBreakTerm p

  | ContinueToken ->
    parseContinueTerm p

  | LoopToken ->
    parseLoopTerm p

  | WhileToken ->
    parseWhileTerm p

  | _ ->
    p.Next |> tokenIsAtomTermFirst |> is false

let parseCallTerm (p: P) =
  parseAtomTerm p

  while p.Next = LeftParenToken do
    p.StartNodeWithPrevious()
    p.Eat(LeftParenToken) |> is true

    while p.Next |> tokenIsParamFirst
      || p.Next = CommaToken do
      parseArg p
      p.Eat(CommaToken) |> ignore

    if p.Eat(RightParenToken) |> not then
      p.AddError(ExpectedError "右カッコ")

    p.EndNode(CallNode)

let parseAddTerm (p: P) =
  parseCallTerm p

  while p.Next = PlusToken do
    p.StartNodeWithPrevious()
    p.Eat(PlusToken) |> is true
    parseCallTerm p
    p.EndNode(BinNode)

let parseEqTerm (p: P) =
  parseAddTerm p

  while p.Next = EqualEqualToken do
    p.StartNodeWithPrevious()
    p.Eat(EqualEqualToken) |> is true
    parseAddTerm p
    p.EndNode(BinNode)

let parseHeadTerm (p: P) =
  parseEqTerm p

let parseTerm (p: P) =
  parseHeadTerm p

  if p.Next = EqualToken then
    p.StartNodeWithPrevious()
    p.Eat(EqualToken) |> is true
    parseTerm p
    p.EndNode(BinNode)

let parseArg (p: P) =
  p.StartNode()

  (p.Eat(InToken) || p.Eat(MoveToken) || p.Eat(RefToken)) |> ignore
  parseTerm p

  p.EndNode(ArgNode)

let parseParam (p: P) =
  p.StartNode()

  (p.Eat(InToken) || p.Eat(MutToken) || p.Eat(RefToken)) |> ignore
  parseCallTerm p

  if p.Eat(ColonToken) then
    parseTy p

  p.EndNode(ParamNode)

let parseResult (p: P) =
  p.StartNode()

  parseTy p

  p.EndNode(ResultNode)

let parseTy (p: P) =
  p.StartNode()

  parseCallTerm p

  p.EndNode(TyNode)

// `fn name(param*)`
let parseFnHead (p: P) =
  if p.Eat(FnToken) |> not then
    p.AddError(ExpectedError "fn")

  if p.Next = IdentToken then
    parseNameTerm p
  else
    p.AddError(ExpectedError "関数名")

  // 引数リスト
  if p.Eat(LeftParenToken) |> not then
    p.AddError(ExpectedError "左カッコ")

  while p.Next |> tokenIsParamFirst
    || p.Next = CommentToken do
    parseParam p
    p.Eat(CommaToken) |> ignore

  if p.Eat(RightParenToken) |> not then
    p.AddError(ExpectedError "右カッコ")

let parseStmt (p: P) =
  match p.Next with
  | LetToken ->
    p.StartNode()
    p.Eat(LetToken) |> is true

    if p.Next |> tokenIsParamFirst then
      parseParam p
    else
      p.AddError(ExpectedError "変数")

    if p.Eat(EqualToken) |> not then
      p.AddError(ExpectedError "=")

    if p.Next |> tokenIsArgFirst then
      parseArg p
    else
      p.AddError(ExpectedError "式")

    p.Eat(SemiToken) |> ignore
    p.EndNode(LetNode)

  | ExternToken ->
    p.StartNode()

    p.Eat(ExternToken) |> is true
    parseFnHead p

    if p.Eat(SlimArrowToken) then
      parseResult p

    p.Eat(SemiToken) |> ignore

    p.EndNode(ExternFnNode)

  | FnToken ->
    p.StartNode()
    parseFnHead p

    // 本体
    if p.Next = LeftBraceToken then
      parseBlockTerm p
    else
      p.AddError(ExpectedError "ブロック")

    if p.Eat(SlimArrowToken) then
      parseResult p

    p.EndNode(FnNode)

  | StructToken ->
    p.StartNode()
    p.Bump()

    if p.Next = IdentToken then
      parseNameTerm p
    else
      p.AddError(ExpectedError "名前")

    if p.Eat(LeftBraceToken) |> not then
      p.AddError(ExpectedError "ブロック")
    else
      while p.Next = IdentToken
        || p.Next = CommaToken do
        parseParam p
        p.Eat(CommaToken) |> ignore

      if p.Eat(RightBraceToken) |> not then
        p.AddError(ExpectedError "}")

    p.EndNode(StructNode)

  | _ ->
    p.Next |> tokenIsTermFirst |> is true

    p.StartNode()
    parseTerm p
    p.Eat(SemiToken) |> ignore
    p.EndNode(ExprNode)

let parseSemi (p: P) =
  p.StartNode()

  while p.Next |> tokenIsStmtFirst do
    parseStmt p

  p.EndNode(SemiNode)

let parseRoot (p: P) =
  p.StartNode()

  while not p.AtEof do
    if p.Next |> tokenIsStmtFirst then
      parseStmt p
    else
      p.AddError(ExpectedError "文")

      while not (p.AtEof || p.Next |> tokenIsStmtKeyword) do
        p.Bump()

  p.EndNode(SemiNode)

  if p.Eat(EofToken) |> not then
    p.AddError(ExpectedError "anything but EOF")

let parse (sourceCode: string) =
  let tokens = SyntaxTokenize.tokenize sourceCode
  let p = ParseContext(tokens)
  parseRoot p
  p.Finish()

let nodeToSnapshot (node: NodeData) =
  let w = System.Text.StringBuilder()

  let writeIndent depth =
    for _ in 0..(depth - 1) do
      w.Append("  ") |> ignore

  let writeToken depth (token: TokenData) =
    writeIndent depth
    w.AppendFormat("T({0}) `{1}`\n", token.Token, token.Text) |> ignore

  let rec writeNode depth (node: NodeData) =
    writeIndent depth
    w.AppendFormat("N({0}) [\n", node.Node) |> ignore

    for child in node.Children do
      match child with
      | TokenElement token ->
        writeToken (depth + 1) token

      | NodeElement child ->
        writeNode (depth + 1) child

      | ErrorElement error ->
        writeIndent (depth + 1)
        w.AppendFormat("E({0})\n", error) |> ignore

    writeIndent depth
    w.Append("]\n") |> ignore

  writeNode 0 node
  w.ToString()
