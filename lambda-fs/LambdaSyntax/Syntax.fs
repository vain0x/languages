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
  | AArrowTy of ATy * ATy

[<NoEquality; NoComparison>]
type AExpr =
  | ANameExpr of AName
  | ALambdaExpr of AName * AExpr
  | ALetExpr of AName * init: AExpr * inClause: AExpr option
  | ATypeAssertExpr of AExpr * ATy
  | ATypeErrorExpr of AExpr
  | AAppExpr of AExpr * AExpr
  | ABlockExpr of AExpr [] * AExpr

[<NoEquality; NoComparison>]
type ARoot = ARoot of AExpr []
