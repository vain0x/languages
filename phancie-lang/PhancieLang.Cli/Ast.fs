module rec PhancieLang.Ast

open PhancieLang.Helpers
open PhancieLang.Syntax

type AError =
  | AError
    of string * SyntaxNode

  | APassByMismatchError
    of Mode * PassBy * SyntaxNode

  | ATyMismatchError
    of actual:ATyInfo * expected:ATyInfo

type ABin =
  | AEqBin
  | AAddBin
  | AAssignBin

[<Struct>]
type AParamTy =
  | AParamTy
    of Mode * ATyInfo * SyntaxNode

[<Struct>]
type AArgTy =
  | AArgTy
    of PassBy * ATyInfo * SyntaxNode

type ATyInfo =
  | ANameTy
    of string * ASymbol option ref * SyntaxNode

  | ABoolTy
    of SyntaxNode

  | AIntTy
    of SyntaxNode

  | AStringTy
    of SyntaxNode

  | AFnTy
    of AParamTy list * ATyInfo * SyntaxNode

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
    Syn: SyntaxNode
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
    Node: SyntaxNode
  }

[<Struct>]
type AName =
  | AName
    of string option * ASymbol option ref * SyntaxNode

[<Struct>]
type ATy =
  | ATy
    of AName option * SyntaxNode

[<Struct>]
type AParam =
  | AParam
    of Mode * AName option * ATy option * SyntaxNode

[<Struct>]
type AResult =
  | AResult
    of ATy option * SyntaxNode

[<Struct>]
type AArg =
  | AArg
    of PassBy * ATerm option * SyntaxNode

type ATerm =
  | ABoolLiteral
    of bool * SyntaxNode

  | AIntLiteral
    of text:string option * SyntaxNode

  | AStrLiteral
    of StrSegment list * SyntaxNode

  | ANameTerm
    of AName

  | AGroupTerm
    of ATerm option * SyntaxNode

  | ABlockTerm
    of AStmt list * SyntaxNode

  | ABreakTerm
    of ALoop option ref * SyntaxNode

  | AContinueTerm
    of ALoop option ref * SyntaxNode

  | ALoopTerm
    of ATerm option * ALoop option ref * SyntaxNode

  | ACallTerm
    of ATerm option * AArg list * SyntaxNode

  | ABinTerm
    of ABin option * ATerm option * ATerm option * (AArgTy * AArgTy * ATyInfo) option ref * SyntaxNode

  | AIfTerm
    of cond:ATerm option
      * body:ATerm option
      * alt:ATerm option
      * ty:ATyInfo option ref
      * SyntaxNode

  | AWhileTerm
    of cond:ATerm option
      * body:ATerm option
      * loop:ALoop option ref
      * SyntaxNode

type AStmt =
  | ATermStmt
    of ATerm option * SyntaxNode

  | ALetStmt
    of AParam option * AArg option * SyntaxNode

  | AExternFnStmt
    of AName option * AParam list * AResult option * AFn option ref * SyntaxNode

  | AFnStmt
    of AName option * AParam list * AResult option * ATerm option * AFn option ref * SyntaxNode

  | AStructStmt
    of AName option * AParam list * SyntaxNode

  | ASemiStmt
    of AStmt list * SyntaxNode

// -----------------------------------------------
// エラー
// -----------------------------------------------

let aErrorToString e =
  let near (first, second) =
    sprintf "    %s\n    %s" first second

  match e with
  | AError (msg, syn) ->
    sprintf "ERROR: %s\n%s"
      msg (syn |> synToFeatureText |> near)

  | APassByMismatchError (mode, passBy, syn) ->
    sprintf "ERROR: Can't pass %A argument to a param with %A\n%s"
      passBy mode (syn |> synToFeatureText |> near)

  | ATyMismatchError (actual, expected) ->
    let expectedTy = expected |> aTyToString
    let expectedNear = expected |> aTyToSyn |> synToFeatureText |> near
    let actualTy = actual |> aTyToString
    let actualNear = actual |> aTyToSyn |> synToFeatureText |> near
    sprintf "ERROR: Type mismatch.\nExpected: %s\nActual: %s\nNear1:\n%s\nNear2:\n%s"
      expectedTy actualTy expectedNear actualNear

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

let aBinToSig bin firstTy _secondTy syn =
  match bin with
  | AEqBin ->
    AParamTy (InMode, firstTy, syn),
    AParamTy (InMode, firstTy, syn),
    ABoolTy syn

  | AAddBin ->
    AParamTy (ValMode, firstTy, syn),
    AParamTy (ValMode, firstTy, syn),
    firstTy

  | AAssignBin ->
    AParamTy (MutMode, firstTy, syn),
    AParamTy (MutMode, firstTy, syn),
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

let aTyToString ty =
  match ty with
  | ANameTy (name, _, _) ->
    name

  | ABoolTy _ ->
    "bool"

  | AIntTy _ ->
    "int"

  | AStringTy _ ->
    "string"

  | AFnTy (paramTys, resultTy, _) ->
    // FIXME: params and result
    "fn"

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
