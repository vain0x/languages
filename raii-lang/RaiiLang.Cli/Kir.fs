module rec RaiiLang.Kir

open RaiiLang.Helpers
open RaiiLang.Syntax

type KIdent = string

/// 識別子の定義・使用の状況。
[<Struct>]
type KUsage =
  {
    /// 定義済みの識別子
    DefSet: Set<KIdent>

    /// 使用されている識別子
    UseSet: Set<KIdent>
  }

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

[<Struct>]
type KVar =
  | KVar
    of string * KTy

[<Struct>]
type KParam =
  | KParam
    of Mode * paramName:string * paramTy:KTy

[<Struct>]
type KArg =
  | KArg
    of PassBy * argNode:string * Mode option ref

[<Struct>]
type KResult =
  | KResult
    of resultTy:KTy

[<Struct>]
type KLabel =
  | KLabel
    of labelName:string
      * KParam list
      * body:KNode ref

[<Struct>]
type KFn =
  | KFn
    of fnName:string
      * KParam list
      * KResult
      * body:KNode ref

[<Struct>]
type KExternFn =
  | KExternFn
    of externFnName:string
      * KParam list
      * KResult

[<Struct>]
type KFix =
  | KLabelFix
    of label:KLabel

  | KFnFix
    of fn:KFn

[<Struct>]
type KCont =
  | KLabelCont
    of label:KLabel

  | KReturnCont
    of fn:KFn

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

  | KJumpPrim

  | KIfPrim

  | KFnPrim
    of fnName:string * fn:KFn option ref

  | KExternFnPrim
    of externFn:KExternFn

type KNode =
  | KNoop

  | KPrim
    of prim:KPrim
      * args:KArg list
      * conts:KCont list

  | KFix
    of fixes:KFix list
      * next:KNode

// -----------------------------------------------
// KUsage
// -----------------------------------------------

let kUsageEmpty (): KUsage =
  {
    DefSet = Set.empty
    UseSet = Set.empty
  }

let kUsageAddDef ident usage =
  { usage with
      DefSet = usage.DefSet |> Set.add ident
  }

let kUsageAddUse ident usage =
  { usage with
      UseSet = usage.UseSet |> Set.add ident
  }

let kUsageToCaptureMap (knownSet: HashSet<_>) (usage: KUsage) =
  Set.difference usage.UseSet usage.DefSet
  |> Set.filter (knownSet.Contains >> not)

// -----------------------------------------------
// KPrim
// -----------------------------------------------

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

  | KJumpPrim ->
    [], KNeverTy

  | KIfPrim ->
    [ByMove], KNeverTy

  | KFnPrim _
  | KExternFnPrim _ ->
    failwithf "unimpl: %A" prim

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

  | KJumpPrim ->
    "prim_jump"

  | KIfPrim ->
    "prim_if"

  | KFnPrim (funName, _) ->
    funName

  | KExternFnPrim (KExternFn (funName, _, _)) ->
    sprintf "extern_%s" funName

// -----------------------------------------------
// KTy
// -----------------------------------------------

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
