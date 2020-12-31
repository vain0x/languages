/// High-level intermediate representation.
module rec LambdaDomain.Hir

open LambdaDomain.Location

type HTyId = string * int

// hint, id
type HVar = string * int

[<NoEquality; NoComparison>]
type HTy =
  | HVarTy of HTyId * Pos
  | HArrowTy of HTy * HTy * Pos

type HTyScheme = HTyId [] * HTy

[<NoEquality; NoComparison>]
type HExpr =
  | HNameExpr of HVar * Pos
  | HLambdaExpr of HVar * HExpr * Pos
  | HAppExpr of HExpr * HExpr * Pos
  | HLetExpr of HVar * init: HExpr * Pos
  | HTypeAssertExpr of HExpr * HTyScheme * Pos
  | HTypeErrorExpr of HExpr * Pos
  | HBlockExpr of HExpr [] * HExpr * Pos

[<NoEquality; NoComparison>]
type HRoot = HRoot of HExpr []
