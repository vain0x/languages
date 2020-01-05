module rec RaiiLang.Kir

open RaiiLang.Helpers
open RaiiLang.Syntax

type KTy =
  | KIntTy
  | KFunTy
    of (Mode * KTy) list

type KFixKind =
  | KLabelFix
  | KFnFix

[<Struct>]
type KPrim =
  | KEqPrim
  | KAddPrim
  | KAssignPrim
  | KFnPrim
    of fnName:string
  | KExternFnPrim
    of externFnName:string

[<Struct>]
type KParam =
  | KParam
    of Mode * paramName:string

[<Struct>]
type KArg =
  | KArg
    of PassBy * argNode:KNode

[<Struct>]
type KLabel =
  | KLabel
    of labelName:string

  | KReturnLabel

  | KExitLabel

type KNode =
  | KBool
    of boolValue:bool

  | KInt
    of intText:string

  | KStr
    of strSegments:StrSegment list

  | KName
    of name:string

  | KPrim
    of prim:KPrim
      * args:KArg list
      * result:string
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

let kPrimToSig prim =
  match prim with
  | KEqPrim ->
    [ByIn; ByIn]

  | KAddPrim ->
    [ByMove; ByMove]

  | KAssignPrim ->
    [ByRef; ByMove]

  | KFnPrim _
  | KExternFnPrim _ ->
    failwithf "kPrimToSig では関数のシグネチャを取得できません: %A" prim

let kPrimToString prim =
  match prim with
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
