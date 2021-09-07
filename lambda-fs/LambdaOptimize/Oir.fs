module rec LambdaOptimize.Oir

type OVar = OVar of string * int

type OLit =
  | OBoolLit of bool
  | OStringLit of string

type ONodeKind =
  | FunCall of OVar
  | ListCreate
  | GenericAdd
  | GenericEqual
  | GenericCompare

type OBaseTy = OBaseTy of string * int

type OTy = OBaseTy of OBaseTy

type OExpr =
  | OLitExpr of OLit
  | OVarExpr of OVar
  | ONodeExpr of ONodeKind * tyArgs: OTy list * valueArgs: OExpr list
  | OBlockExpr of OStmt list * last: OExpr

type OStmt = OLetStmt of OVar * init: OExpr
