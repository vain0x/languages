module LambdaSyntax.Syntax

open LambdaDomain.Location
open LambdaSyntax.Token

/// Kind of syntax node.
type SyntaxKind =
  | RootSN
  | NameSN
  | TypeAssertSN
  | TypeErrorSN
  | NameTySN
  | UnivTySN
  | ParenTySN
  | ArrowTySN
  | NameExprSN
  | ParenExprSN
  | LambdaExprSN
  | LetExprSN
  | AppExprSN
  | ArgSN
  | BlockExprSN

[<RequireQualifiedAccess; Struct; NoEquality; NoComparison>]
type SyntaxToken =
  { Kind: Token
    Text: string
    Range: Range }

[<NoEquality; NoComparison>]
type SyntaxElement =
  | SyntaxToken of SyntaxToken
  | SyntaxNode of SyntaxKind * SyntaxElement []

[<Struct; NoEquality; NoComparison>]
type ParseEvent =
  | StartEvent of SyntaxKind
  | FinishEvent
  | TokenEvent

[<NoEquality; NoComparison>]
type ParseError = ParseError of string * Range

[<NoEquality; NoComparison>]
type ParseResult =
  { Events: ParseEvent []
    Errors: ParseError [] }
