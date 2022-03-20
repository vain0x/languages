/// Abstract syntax tree.
module Linear.Ast

open Linear.Location
open Linear.Token

[<Struct; NoEquality; NoComparison>]
type AName = AName of TokenData

[<NoEquality; NoComparison>]
type ATy =
  | ANameTy of AName
  | APairTy of ATy * ATy
  | AFunTy of ATy * ATy
  | ALinearTy of ATy

[<NoEquality; NoComparison>]
type APat =
  | ANamePat of AName
  | AWildcardPat of Pos
  | AUnitPat of Pos
  | APairPat of APat * APat
  | AWrapPat of AName * APat

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Binary =
  | Add
  | Equal
  | Pair

[<NoEquality; NoComparison>]
type AExpr =
  | AIntExpr of int * Pos
  | AUnitExpr of Pos
  | ANameExpr of AName
  | AAppExpr of AExpr * AExpr
  | ABinaryExpr of Binary * AExpr * AExpr
  | AIfExpr of cond: AExpr * thenClause: AExpr * elseClause: AExpr
  | ALetExpr of APat * init: AExpr
  | ABlockExpr of AExpr list * last: AExpr

[<NoEquality; NoComparison>]
type ADecl =
  | AFunDecl of AName * paramList: (AName * ATy) list * resultTy: ATy * body: AExpr * Range
  | AExpectDecl of desc: string * AExpr * Range
  | AExpectErrorDecl of desc: string * AExpr * Range

[<NoEquality; NoComparison>]
type ARoot = ARoot of ADecl list
