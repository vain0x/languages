module LambdaSyntax.Syntax

open LambdaDomain.Location
open LambdaSyntax.Token

[<NoEquality; NoComparison>]
type ParseError = ParseError of string * Range

[<Struct; NoEquality; NoComparison>]
type AName = AName of TokenData

/// Term of type.
[<NoEquality; NoComparison>]
type ATy =
  | AUniversalTy of AName
  | AArrowTy of ATy * ATy * Pos

[<NoEquality; NoComparison>]
type AExpr =
  | ANameExpr of AName
  | ALambdaExpr of AName * AExpr * Pos
  | ALetExpr of AName * init: AExpr * inClause: AExpr option * Pos
  | ATypeAssertExpr of AExpr * ATy * Pos
  | ATypeErrorExpr of AExpr * Pos
  | AAppExpr of AExpr * AExpr
  | ABlockExpr of AExpr [] * AExpr * Pos

[<NoEquality; NoComparison>]
type ARoot = ARoot of AExpr []
