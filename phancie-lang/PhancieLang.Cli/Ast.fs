module rec PhancieLang.Ast

open PhancieLang.Helpers
open PhancieLang.Syntax

type ABin =
  | AEqBin
  | AAddBin
  | AAssignBin

[<Struct>]
type AName =
  | AName
    of string option * NodeData

[<Struct>]
type ATy =
  | ATy
    of string option * NodeData

[<Struct>]
type AArg =
  | AArg
    of PassBy * ATerm option * NodeData

[<Struct>]
type AParam =
  | AParam
    of Mode * AName option * ATy option * NodeData

[<Struct>]
type AResult =
  | AResult
    of ATy option * NodeData

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
    of NodeData

  | AContinueTerm
    of NodeData

  | ALoopTerm
    of ATerm option * NodeData

  | ACallTerm
    of ATerm option * AArg list * NodeData

  | ABinTerm
    of ABin option * ATerm option * ATerm option * NodeData

  | AIfTerm
    of cond:ATerm option * body:ATerm option * alt:ATerm option * NodeData

  | AWhileTerm
    of cond:ATerm option * body:ATerm option * NodeData

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
