/// High-level intermediate representation.
module rec LambdaDomain.Hir

open LambdaDomain.Location

// hint, id
type HVarTy = string * int * Pos

// hint, id
type HVar = string * int * Pos

[<NoEquality; NoComparison>]
type HTy =
  | HVarTy of HVarTy
  | HArrowTy of HTy * HTy * Pos

type HTyScheme = HVarTy [] * HTy

[<NoEquality; NoComparison>]
type HExpr =
  | HNameExpr of HVar
  | HLambdaExpr of HVar * HExpr * Pos
  | HAppExpr of HExpr * HExpr
  | HLetExpr of HVar * init: HExpr * Pos
  | HTypeAssertExpr of HExpr * HTyScheme * Pos
  | HTypeErrorExpr of HExpr * Pos
  | HBlockExpr of HExpr [] * HExpr * Pos

[<NoEquality; NoComparison>]
type HRoot = HRoot of HExpr []
