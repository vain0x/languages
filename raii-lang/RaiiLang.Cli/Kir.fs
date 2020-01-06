module rec RaiiLang.Kir

open RaiiLang.Helpers
open RaiiLang.Syntax

type KTy =
  | KNeverTy
  | KUnitTy
  | KBoolTy
  | KIntTy
  | KStrTy
  | KFunTy
    of KParam list * KResult

  | KInferTy
    of string * KTy option ref

type KFixKind =
  | KLabelFix
  | KFnFix

[<Struct>]
type KPrim =
  | KBoolLiteralPrim
    of boolValue:bool

  | KIntLiteralPrim
    of intText:string

  | KStrLiteralPrim
    of strSegments:StrSegment list

  | KEqPrim

  | KAddPrim

  | KAssignPrim

  | KFnPrim
    of fnName:string

  | KExternFnPrim
    of externFnName:string

[<Struct>]
type KArg =
  | KArg
    of PassBy * argNode:KNode

[<Struct>]
type KParam =
  | KParam
    of Mode * paramName:string * paramTy:KTy

[<Struct>]
type KResult =
  | KResult
    of resultTy:KTy

[<Struct>]
type KLabel =
  | KLabel
    of labelName:string

  | KReturnLabel

  | KExitLabel

type KNode =
  | KNoop

  | KName
    of name:string

  | KPrim
    of prim:KPrim
      * args:KArg list
      * result:KParam
      * next:KNode

  | KJump
    of KLabel
      * args:KArg list

  | KIf
    of cond:KNode
      * body:KNode
      * alt:KNode

  | KFix
    of funName:string
      * kind:KFixKind
      * paramList:KParam list
      * result:KResult
      * funBody:KNode
      * next:KNode

let kPrimFromBin bin =
  match bin with
  | AEqBin ->
    KEqPrim

  | AAddBin ->
    KAddPrim

  | AAssignBin ->
    KAssignPrim

let kPrimIsLiteral prim =
  match prim with
  | KBoolLiteralPrim _
  | KIntLiteralPrim _
  | KStrLiteralPrim _ ->
    true

  | _ ->
    false

let kPrimToSig prim =
  match prim with
  | KBoolLiteralPrim _ ->
    [], KBoolTy

  | KIntLiteralPrim _ ->
    [], KIntTy

  | KStrLiteralPrim _ ->
    [], KStrTy

  | KEqPrim ->
    [ByIn; ByIn], KBoolTy

  | KAddPrim ->
    [ByMove; ByMove], KIntTy

  | KAssignPrim ->
    [ByRef; ByMove], KUnitTy

  | KFnPrim _
  | KExternFnPrim _ ->
    failwithf "kPrimToSig では関数のシグネチャを取得できません: %A" prim

let kPrimToString prim =
  match prim with
  | KBoolLiteralPrim false ->
    "false"

  | KBoolLiteralPrim true ->
    "true"

  | KIntLiteralPrim intText ->
    intText

  | KStrLiteralPrim segments ->
    [] |> strUnescape segments |> List.rev |> String.concat ""

  | KEqPrim ->
    "prim_eq"

  | KAddPrim ->
    "prim_add"

  | KAssignPrim ->
    "prim_assign"

  | KFnPrim name ->
    name

  | KExternFnPrim name ->
    sprintf "extern_%s" name

let kTyDeref ty =
  match ty with
  | KInferTy (_, tyOpt) ->
    match !tyOpt with
    | Some ty ->
      ty |> kTyDeref

    | None ->
      ty

  | _ ->
    ty
