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
  | HUnitExpr of Pos
  | HNameExpr of HVar * Pos
  | HLambdaExpr of HVar * HExpr * Pos
  | HAppExpr of HExpr * HExpr * Pos
  | HBlockExpr of HStmt [] * HExpr * Pos

[<NoEquality; NoComparison>]
type HStmt =
  | HExprStmt of HExpr * Pos
  | HLetStmt of HVar * init: HExpr * Pos
  | HTypeAssertStmt of HExpr * HTyScheme * Pos
  | HTypeErrorStmt of HExpr * Pos

[<NoEquality; NoComparison>]
type HRoot = HRoot of HStmt []
