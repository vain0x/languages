module rec MiloneSyntaxV2.Hir

open MiloneSyntaxV2.Source
open MiloneSyntaxV2.Syntax

type VarId = VarId of int
type FunId = FunId of int
type JointId = JointId of int
type RegId = RegId of int
type RecordId = RecordId of int
type VariantId = VariantId of int

type HTy = HTy of int

type HTerm =
  | HLitTerm of ALit * Loc
  | HVarTerm of VarId * HTy list * Loc
  | HFunTerm of FunId * HTy list * Loc
  | HRegTerm of RegId * HTy list * Loc

type HPatKind =
  | HLitPk of ALit
  | HWildcardPk
  | HVariantPk of VariantId

type HPat =
  | HPat of HPatKind * inputs: HPat list * outputOpt: RegId option * Loc

type HExpr =
  | HTermExpr of HTerm
  | HNodeExpr of HExprKind * inputs: HTerm list * outputOpt: RegId option * Loc
  | HMatchExpr of HTerm * HMatchClause list * Loc

type HBlock =
  { Joints: HJointBinding list
    Last: HExpr }

type HJointBinding =
  { Joint: JointId
    Params: RegId list
    Body: HBlock }

type HMatchClause =
  { Pat: (HPat * RegId option) list
    Guard: RegId option
    Body: HBlock }

[<NoEquality; NoComparison>]
type HExprKind =
  // Primitive.
  | HMoveEk
  | HMinusEk
  | HAddEk
  | HSubEk
  | HMulEk
  | HDivEk
  | HModuloEk
  | HBitAndEk
  | HBitOrEk
  | HBitXorEk
  | HLeftShiftEk
  | HRightShiftEk
  | HEqualEk
  | HNotEqualEk
  | HLessEk
  | HLessEqualEk
  | HGreaterEk
  | HGreaterEqualEk
  | HLogicalOrEk
  | HLogicalAndEk
  | HIndexEk
  | HFieldEk of int
  | HCallFunEk of FunId * HTy list
  | HCallJointEk of JointId

  // Data constructor.
  | HListEk // spread?
  | HRecordEk of RecordId
  | HVariantEk of VariantId
  | HClosureEk of FunId

  // Effect.
  | HAbortEk
