module Linear.TypedIR

open Linear.Location

type Id = int
type TIdent = string

[<NoEquality; NoComparison>]
type TName = TName of TIdent * Id * Pos

type TVarName = TName
type TFunName = TName
type TVariantName = TName

[<NoEquality; NoComparison>]
type TTy =
  | TIntTy
  | TUnitTy
  | TBoolTy
  | TUnionTy of TName
  | TPairTy of TTy * TTy
  | TFunTy of TTy * TTy
  | TLinearTy of TTy

[<NoEquality; NoComparison>]
type TPat =
  | TVarPat of TVarName * TTy
  | TWildcardPat of TTy * Pos
  | TUnitPat of Pos
  | TPairPat of TPat * TPat
  | TWrapPat of TVariantName * TPat

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TBinary =
  | Add
  | Equal
  | Pair

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TPrim =
  | Assert
  | UUAcquire
  | UUDispose

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TUnary =
  | Assert
  | UUAcquire
  | UUDispose

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TSymbolKind =
  | Var
  | Fun
  | Variant

[<NoEquality; NoComparison>]
type TExpr =
  | TIntExpr of int * Pos
  | TBoolExpr of bool * Pos
  | TUnitExpr of Pos
  | TSymbolExpr of TSymbolKind * Id * TTy * Pos
  | TAppExpr of TExpr * TExpr
  | TUnaryExpr of TUnary * TExpr
  | TBinaryExpr of TBinary * TExpr * TExpr
  | TIfExpr of cond: TExpr * thenClause: TExpr * elseClause: TExpr
  | TLetExpr of pat: TPat * init: TExpr
  | TBlockExpr of TExpr list * last: TExpr

[<NoEquality; NoComparison>]
type TDecl =
  | TExpectDecl of desc: string * TExpr * Range
  | TExpectErrorDecl of desc: string * TExpr * Range

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TFunDef =
  { Name: TFunName
    ParamList: (TVarName * TTy) list
    ResultTy: TTy
    Body: TExpr
    Range: Range }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TNewtypeDef =
  { Name: TName
    Variant: TVariantName
    PayloadTy: TTy
    Range: Range }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TModule =
  { Decls: TDecl list
    Funs: Map<Id, TFunDef>
    Newtypes: Map<Id, TNewtypeDef> }
