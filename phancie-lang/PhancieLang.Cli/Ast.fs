module rec PhancieLang.Ast

open PhancieLang.Helpers
open PhancieLang.Syntax

type AError =
  | AError
    of string * NodeData

  | APassByMismatchError
    of Mode * PassBy * NodeData

  | ATyMismatchError
    of actual:ATyInfo * expected:ATyInfo

type ABin =
  | AEqBin
  | AAddBin
  | AAssignBin

[<Struct>]
type AParamTy =
  | AParamTy
    of Mode * ATyInfo

[<Struct>]
type AArgTy =
  | AArgTy
    of PassBy * ATyInfo

type ATyInfo =
  | ANameTy
    of string * ASymbol option ref * NodeData

  | ABoolTy
    of NodeData

  | AIntTy
    of NodeData

  | AStringTy
    of NodeData

  | AFnTy
    of AParamTy list * ATyInfo * NodeData

[<ReferenceEquality>]
[<NoComparison>]
type ASymbol =
  | ATySymbol
    of tyName:string * ty:ATyInfo

  | ALocalSymbol
    of localName:string * localMode:Mode * localTy:ATyInfo

  | AFnSymbol
    of AFn

[<ReferenceEquality>]
[<NoComparison>]
type ALoop =
  {
    LoopOpt: ATerm option
    FnOpt: AFn option
  }

[<Struct>]
type AFnKind =
  | AExternFnKind

  | AFnKind
    of ATerm option

[<ReferenceEquality>]
[<NoComparison>]
type AFn =
  {
    Name: string
    Kind: AFnKind
    Params: AParam list
    ResultOpt: AResult option
    ParamTys: AParamTy list
    ResultTy: ATyInfo
    Node: NodeData
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
      * ty:ATyInfo option ref
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
    of AName option * AParam list * AResult option * AFn option ref * NodeData

  | AFnStmt
    of AName option * AParam list * AResult option * ATerm option * AFn option ref * NodeData

  | ASemiStmt
    of AStmt list * NodeData

// -----------------------------------------------
// 二項演算子
// -----------------------------------------------

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

let aBinToSig bin firstTy secondTy syn =
  match bin with
  | AEqBin ->
    AParamTy (InMode, firstTy),
    AParamTy (InMode, firstTy),
    ABoolTy syn

  | AAddBin ->
    AParamTy (ValMode, firstTy),
    AParamTy (ValMode, firstTy),
    firstTy

  | AAssignBin ->
    AParamTy (MutMode, firstTy),
    AParamTy (MutMode, firstTy),
    firstTy

// -----------------------------------------------
// 型情報
// -----------------------------------------------

let aNeverTy syn =
  ANameTy ("__never", ref None, syn)

let aErrorTy syn =
  ANameTy ("__error", ref None, syn)

let aUnitTy syn =
  AIntTy syn

let aTyToSyn ty =
  match ty with
  | ANameTy (_, _, syn)
  | ABoolTy syn
  | AIntTy syn
  | AStringTy syn
  | AFnTy (_, _, syn) ->
    syn

let aTyToParamTys ty =
  match ty with
  | ANameTy (_, symbolSlot, _) ->
    match !symbolSlot with
    | Some symbol ->
      symbol |> aSymbolToTy |> aTyToParamTys

    | None ->
      []

  | AFnTy (paramTys, _, _) ->
    paramTys

  | _ ->
    []

let aTyToResultTy ty =
  match ty with
  | ANameTy (_, symbolSlot, _) ->
    !symbolSlot |> Option.bind (aSymbolToTy >> aTyToResultTy)

  | AFnTy (_, resultTy, _) ->
    Some resultTy

  | _ ->
    None

// -----------------------------------------------
// シンボル
// -----------------------------------------------

let aSymbolToTy symbol =
  match symbol with
  | ATySymbol (_, ty) ->
    ty

  | ALocalSymbol (_, _, ty) ->
    ty

  | AFnSymbol fn ->
    AFnTy (fn.ParamTys, fn.ResultTy, fn.Node)
