module rec PhancieLang.Ast

open PhancieLang.Helpers
open PhancieLang.Syntax

type ABin =
  | AEqBin
  | AAddBin
  | AAssignBin

[<ReferenceEquality>]
[<NoComparison>]
type ASymbol =
  | ATySymbol
    of tyName:string

  | ALocalSymbol
    of localName:string * localTy:ATy option

  | AFnSymbol
    of fnName:string * fnParams:AParam list * fnResult:AResult

[<ReferenceEquality>]
[<NoComparison>]
type ALoop =
  {
    LoopOpt: ATerm option
    BreakOpt: ATerm option
    ContinueOpt: ATerm option
  }

[<Struct>]
type AName =
  | AName
    of string option * ASymbol option ref * NodeData

[<Struct>]
type ATy =
  | ATy
    of AName option * NodeData

[<Struct>]
type AParam =
  | AParam
    of Mode * AName option * ATy option * NodeData

[<Struct>]
type AResult =
  | AResult
    of ATy option * NodeData

[<Struct>]
type AArg =
  | AArg
    of PassBy * ATerm option * NodeData

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
    of ALoop option ref * NodeData

  | AContinueTerm
    of ALoop option ref * NodeData

  | ALoopTerm
    of ATerm option * ALoop option ref * NodeData

  | ACallTerm
    of ATerm option * AArg list * NodeData

  | ABinTerm
    of ABin option * ATerm option * ATerm option * NodeData

  | AIfTerm
    of cond:ATerm option
      * body:ATerm option
      * alt:ATerm option
      * ty:ATy option ref
      * NodeData

  | AWhileTerm
    of cond:ATerm option
      * body:ATerm option
      * loop:ALoop option ref
      * NodeData

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

let aBinFromToken (token: Token) =
  match token with
  | EqualEqualToken ->
    Some AEqBin

  | PlusToken ->
    Some AAddBin

  | EqualToken ->
    Some AAssignBin

  | _ ->
    None
