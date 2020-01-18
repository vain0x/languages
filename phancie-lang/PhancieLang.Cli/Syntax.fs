module rec PhancieLang.Syntax

open PhancieLang.Helpers

type TextLength = int

[<Struct>]
type SyntaxError =
  | ExpectedError
    of string

  | UnexpectedCharsError

type Token =
  | EofToken
  | EolToken
  | SpaceToken
  | CommentToken
  | OtherToken

  | IntToken

  | StrStartToken
  | StrVerbatimToken
  | StrEndToken

  // キーワード類
  | IdentToken
  | BreakToken
  | ContinueToken
  | ElseToken
  | ExternToken
  | FalseToken
  | FnToken
  | IfToken
  | InToken
  | LetToken
  | LoopToken
  | MoveToken
  | MutToken
  | RefToken
  | ThenToken
  | TrueToken
  | WhileToken

  // 記号類
  | ColonToken
  | CommaToken
  | EqualEqualToken
  | EqualToken
  | LeftBraceToken
  | LeftParenToken
  | PlusToken
  | RightBraceToken
  | RightParenToken
  | SemiToken
  /// `->`
  | SlimArrowToken

[<Struct>]
type TokenData =
  {
    Token: Token
    Text: string
  }

type TriviaData =
  TokenData

[<Struct>]
type TokenFat =
  {
    Token: TokenData
    Leading: ResizeArray<TriviaData>
    Trailing: ResizeArray<TriviaData>
  }

type Node =
  // 項
  | BoolLiteralNode
  | IntLiteralNode
  | StrLiteralNode
  | NameNode
  | GroupNode
  | BlockNode
  | BreakNode
  | ContinueNode
  | LoopNode
  | WhileNode
  | CallNode
  | IfNode
  | ThenNode
  | ElseNode
  | BinNode

  // その他
  | ParamNode
  | ArgNode
  | ResultNode

  // 型
  | TyNode

  // 文
  | ExprNode
  | LetNode
  | ExternFnNode
  | FnNode
  | SemiNode

[<Struct>]
type NodeData =
  {
    Node: Node
    Children: ResizeArray<Element>
  }

and [<Struct>] Element =
  | TokenElement
    of token:TokenData

  | NodeElement
    of node:NodeData

  | ErrorElement
    of error:SyntaxError

type ABin =
  | AEqBin
  | AAddBin
  | AAssignBin

[<Struct>]
type AName =
  | AName
    of string option * NodeData

[<Struct>]
type ATy =
  | ATy
    of string option * NodeData

[<Struct>]
type AArg =
  | AArg
    of PassBy * ATerm option * NodeData

[<Struct>]
type AParam =
  | AParam
    of Mode * AName option * ATy option * NodeData

[<Struct>]
type AResult =
  | AResult
    of ATy option * NodeData

type ATerm =
  | ABoolLiteral
    of bool * NodeData

  | AIntLiteral
    of text:string option * NodeData

  | AStrLiteral
    of StrSegment list * NodeData

  | ANameTerm
    of AName

  | AGroupTerm
    of ATerm option * NodeData

  | ABlockTerm
    of AStmt list * NodeData

  | ABreakTerm
    of NodeData

  | AContinueTerm
    of NodeData

  | ALoopTerm
    of ATerm option * NodeData

  | ACallTerm
    of ATerm option * AArg list * NodeData

  | ABinTerm
    of ABin option * ATerm option * ATerm option * NodeData

  | AIfTerm
    of cond:ATerm option * body:ATerm option * alt:ATerm option * NodeData

  | AWhileTerm
    of cond:ATerm option * body:ATerm option * NodeData

type AStmt =
  | ATermStmt
    of ATerm option * NodeData

  | ALetStmt
    of AParam option * AArg option * NodeData

  | AExternFnStmt
    of AName option * AParam list * AResult option * NodeData

  | AFnStmt
    of AName option * AParam list * AResult option * ATerm option * NodeData

  | ASemiStmt
    of AStmt list * NodeData

let keywords =
  [
    BreakToken, "break"
    ContinueToken, "continue"
    ElseToken, "else"
    ExternToken, "extern"
    FalseToken, "false"
    FnToken, "fn"
    IfToken, "if"
    InToken, "in"
    LetToken, "let"
    LoopToken, "loop"
    MoveToken, "move"
    MutToken, "mut"
    RefToken, "ref"
    ThenToken, "then"
    TrueToken, "true"
    WhileToken, "while"
  ]

let punctuations =
  [
    ColonToken, ":"
    CommaToken, ","
    EqualEqualToken, "=="
    EqualToken, "="
    LeftBraceToken, "{"
    LeftParenToken, "("
    PlusToken, "+"
    RightBraceToken, "}"
    RightParenToken, ")"
    SemiToken, ";"
    SlimArrowToken, "->"
  ]

let tokenIsTrivia token =
  match token with
  | EolToken
  | SpaceToken
  | CommentToken
  | OtherToken ->
    true

  | _ ->
    false

let tokenIsLeadingTrivia token =
  tokenIsTrivia token

let tokenIsTrailingTrivia token =
  tokenIsTrivia token && token <> EolToken

let tokenIsStmtKeyword token =
  match token with
  | ExternToken
  | LetToken
  | FnToken ->
    true

  | _ ->
    false

let tokenIsAtomTermFirst token =
  match token with
  | FalseToken
  | TrueToken
  | IntToken
  | StrStartToken
  | IdentToken
  | BreakToken
  | ContinueToken
  | LoopToken
  | LeftParenToken
  | LeftBraceToken ->
    true

  | _ ->
    false

let tokenIsTermFirst token =
  tokenIsAtomTermFirst token

let tokenIsArgFirst token =
  match token with
  | InToken
  | MoveToken
  | RefToken ->
    true

  | _ ->
    tokenIsTermFirst token

let tokenIsParamFirst token =
  match token with
  | InToken
  | MutToken
  | RefToken ->
    true

  | _ ->
    tokenIsTermFirst token

/// パイプラインのセグメントの先頭になるトークンか？
let tokenIsSegmentFirst token =
  match token with
  | ThenToken
  | WhileToken ->
    true

  | _ ->
    false

let tokenIsStmtFirst token =
  tokenIsStmtKeyword token
  || tokenIsTermFirst token

let tokenToText (token: TokenData) =
  token.Text

let tokenAsBin (token: Token) =
  match token with
  | EqualEqualToken ->
    Some AEqBin

  | PlusToken ->
    Some AAddBin

  | EqualToken ->
    Some AAssignBin

  | _ ->
    None

let tokenAsMode (token: Token) =
  match token with
  | InToken ->
    Some InMode

  | MutToken ->
    Some MutMode

  | RefToken ->
    Some RefMode

  | _ ->
    None

let tokenAsPassBy (token: Token) =
  match token with
  | InToken ->
    Some ByIn

  | MoveToken ->
    Some ByMove

  | RefToken ->
    Some ByRef

  | _ ->
    None

let nodeIsTerm (node: Node) =
  match node with
  | BoolLiteralNode
  | IntLiteralNode
  | StrLiteralNode
  | NameNode
  | GroupNode
  | BlockNode
  | BreakNode
  | ContinueNode
  | LoopNode
  | CallNode
  | BinNode
  | IfNode
  | WhileNode ->
    true

  | _ ->
    false

let nodeIsStmt (node: Node) =
  match node with
  | ExprNode
  | LetNode
  | ExternFnNode
  | FnNode
  | SemiNode ->
    true

  | _ ->
    false

let nodeToFirstToken pred (node: NodeData) =
  node.Children |> Seq.tryPick (fun element ->
    match element with
    | TokenElement t when pred t.Token ->
      Some t

    | _ ->
      None
  )

let nodeToFilterToken pred (node: NodeData) =
  node.Children |> Seq.choose (fun element ->
    match element with
    | TokenElement t when pred t.Token ->
      Some t

    | _ ->
      None
  )
  |> Seq.toList

let nodeToFirstNode pred (node: NodeData) =
  node.Children |> Seq.tryPick (fun element ->
    match element with
    | NodeElement n when pred n.Node ->
      Some n

    | _ ->
      None
  )

let nodeToFilterNode pred (node: NodeData) =
  node.Children |> Seq.choose (fun element ->
    match element with
    | NodeElement n when pred n.Node ->
      Some n

    | _ ->
      None
  )
  |> Seq.toList

let nodeAddTokenFat (token: TokenFat) (node: NodeData) =
  for trivia in token.Leading do
    node.Children.Add(TokenElement trivia)

  node.Children.Add(TokenElement token.Token)

  for trivia in token.Trailing do
    node.Children.Add(TokenElement trivia)
