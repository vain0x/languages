module rec PhancieLang.Kir

open PhancieLang.Ast
open PhancieLang.Helpers
open PhancieLang.Syntax

type KIdent = string

/// 識別子の定義・使用の状況。
[<Struct>]
type KUsage =
  {
    /// 定義済みの識別子
    Defs: Map<KIdent, Mode>

    /// 使用されている識別子
    Uses: Map<KIdent, PassBy list>
  }

[<Struct>]
type KParamTy =
  | KParamTy
    of Mode * KTyData * SyntaxNode

[<Struct>]
type KArgTy =
  | KArgTy
    of PassBy * KTyData * SyntaxNode

type KTy =
  | KNeverTy

  | KUnitTy

  | KBoolTy

  | KIntTy

  | KStrTy

  | KFnTy
    of KParamTy list * KTyData

type KTyData =
  (struct (KTy * SyntaxNode))

[<Struct>]
type KParam =
  | KParam
    of Mode * KIdent * KTyData * SyntaxNode

[<Struct>]
type KArg =
  | KArg
    of PassBy * KTermData * SyntaxNode

[<Struct>]
type KLoop =
  | KLoop
    of breakLabel:KLabel
      * continueLabel:KLabel

[<Struct>]
type KLabel =
  | KLabel
    of labelName:string
      * KParam list
      * body:KNodeData option ref
      * SyntaxNode

[<Struct>]
type KFn =
  | KFn
    of fnName:string
      * KParam list
      * KTyData
      * body:KNodeData option ref
      * SyntaxNode

[<Struct>]
type KExternFn =
  | KExternFn
    of externFnName:string
      * KParam list
      * KTyData
      * SyntaxNode

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
  | KIdPrim

  | KEqPrim

  | KAddPrim

  | KAssignPrim

  | KIfPrim

  | KFnPrim
    of fn:KFn

  | KExternFnPrim
    of externFn:KExternFn

[<Struct>]
type KTerm =
  | KBoolLiteral
    of boolValue:bool

  | KIntLiteral
    of intText:string

  | KStrLiteral
    of strSegments:StrSegment list

  | KLocalTerm
    of KParam

type KTermData =
  (struct (KTerm * SyntaxNode))

type KNode =
  | KNoop

  | KPrim
    of prim:KPrim
      * args:KArg list
      * results:KParam list
      * nexts:KNodeData list

  | KJump
    of cont:KCont
      * args:KArg list

  | KFix
    of fixes:KFix list
      * next:KNodeData

type KNodeData =
  (struct (KNode * SyntaxNode))

// -----------------------------------------------
// KUsage
// -----------------------------------------------

let kUsageEmpty (): KUsage =
  {
    Defs = Map.empty
    Uses = Map.empty
  }

let kUsageAddDef ident mode usage =
  { usage with
      Defs = usage.Defs |> Map.add ident mode
  }

let kUsageAddUse ident passBy usage =
  { usage with
      Uses =
        match usage.Uses |> Map.tryFind ident with
        | Some passByList ->
          usage.Uses |> Map.add ident (passBy :: passByList)

        | None ->
          usage.Uses |> Map.add ident [passBy]
  }

let kUsageToFreeVars (knownSet: HashSet<_>) (usage: KUsage) =
  usage.Uses
  |> Seq.choose (fun (KeyValue (ident, passByList)) ->
    match usage.Defs |> Map.tryFind ident with
    | Some _ ->
      None

    | None ->
      if knownSet.Contains(ident) then
        None
      else
        Some (ident, passByList)
  )

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

let kPrimToString prim =
  match prim with
  | KIdPrim ->
    "prim_id"

  | KEqPrim ->
    "prim_eq"

  | KAddPrim ->
    "prim_add"

  | KAssignPrim ->
    "prim_assign"

  | KIfPrim ->
    "prim_if"

  | KFnPrim (KFn (fnName, _, _, _, _)) ->
    fnName

  | KExternFnPrim (KExternFn (fnName, _, _, _)) ->
    sprintf "extern_%s" fnName
