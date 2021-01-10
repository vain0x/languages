module rec WasmLang.Types

type Depth = int

type IsReserved =
  | IsReserved
  | NotReserved

type IsMut =
  | IsMut
  | NotMut

type ResizableLimits =
  { Flags: uint
    Initial: uint
    Maximum: uint option }

type ValueType =
  | I32
  | I64
  | F32
  | F64

type BlockType = ValueType option

type ElemType = | AnyFunc

type FuncType =
  { Params: ValueType list
    Ret: ValueType option }

type GlobalType =
  { Content: ValueType
    Mutable: IsMut }

type TableType =
  { Element: ElemType
    Limits: ResizableLimits }

type MemoryType =
  { Limits: ResizableLimits }

type BrTarget =
  { Table: uint list
    Default: uint }

type FunctionSpaceIndex = uint
type TypeIndex = uint

type CallIndirect =
  { Index: TypeIndex
    Reserved: IsReserved }

type LocalIndex = uint
type GlobalIndex = uint

type MemoryImmediate =
  { Flags: uint
    Offset: uint }

type Op =
  | Unreachable
  | Nop
  | Block of BlockType
  | Loop  of BlockType
  | If    of BlockType
  | Else
  | End

  // TODO: use relative block index
  | Br of Depth
  | BrIf of Depth
  | BrTable of BrTarget
  | Return
  | Call of FunctionSpaceIndex
  | CallIndirect of CallIndirect
  | Drop
  | Select
  | GetLocal of LocalIndex
  | SetLocal of LocalIndex
  | TeeLocal of LocalIndex
  | GetGlobal of GlobalIndex
  | SetGlobal of GlobalIndex
  | I32Load of MemoryImmediate
  | I64Load of MemoryImmediate
  | F32Load of MemoryImmediate
  | F64Load of MemoryImmediate
  | I32Load8S of MemoryImmediate
  | I32Load8U of MemoryImmediate
  | I32Load16S of MemoryImmediate
  | I32Load16U of MemoryImmediate
  | I64Load8S of MemoryImmediate
  | I64Load8U of MemoryImmediate
  | I64Load16S of MemoryImmediate
  | I64Load16U of MemoryImmediate
  | I64load32S of MemoryImmediate
  | I64load32U of MemoryImmediate
  | I32Store of MemoryImmediate
  | I64Store of MemoryImmediate
  | F32Store of MemoryImmediate
  | F64Store of MemoryImmediate
  | I32Store8 of MemoryImmediate
  | I32Store16 of MemoryImmediate
  | I64Store8 of MemoryImmediate
  | I64Store16 of MemoryImmediate
  | I64Store32 of MemoryImmediate
  | CurrentMemory of IsReserved
  | GrowMemory of IsReserved
  | I32Const of int32
  | I64Const of int64
  | F32Const of float32
  | F64Const of float
  | I32Eqz
  | I32Eq
  | I32NE
  | I32LtS
  | I32LtU
  | I32GtS
  | I32GtU
  | I32LeS
  | I32LeU
  | I32GeS
  | I32GeU
  | I64Eqz
  | I64Eq
  | I64Ne
  | I64LtS
  | I64LtU
  | I64GtS
  | I64GtU
  | I64LeS
  | I64LeU
  | I64GeS
  | I64GeU
  | F32Eq
  | F32Ne
  | F32Lt
  | F32Gt
  | F32Le
  | F32Ge
  | F64Eq
  | F64Ne
  | F64Lt
  | F64Gt
  | F64Le
  | F64Ge
  | I32Clz
  | I32Ctz
  | I32Popcnt
  | I32Add
  | I32Sub
  | I32Mul
  | I32DivS
  | I32DivU
  | I32RemS
  | I32RemU
  | I32And
  | I32Or
  | I32Xor
  | I32Shl
  | I32ShrS
  | I32ShrU
  | I32Rotl
  | I32Rotr
  | I64Clz
  | I64Ctz
  | I64Popcnt
  | I64Add
  | I64Sub
  | I64Mul
  | I64DivS
  | I64DivU
  | I64RemS
  | I64RemU
  | I64And
  | I64Or
  | I64Xor
  | I64Shl
  | I64ShrS
  | I64ShrU
  | I64Rotl
  | I64Rotr
  | F32Abs
  | F32Neg
  | F32Ceil
  | F32Floor
  | F32Trunc
  | F32Nearest
  | F32Sqrt
  | F32Add
  | F32Sub
  | F32Mul
  | F32Div
  | F32Min
  | F32Max
  | F32Copysign
  | F64Abs
  | F64Neg
  | F64Ceil
  | F64Floor
  | F64Trunc
  | F64Nearest
  | F64Sqrt
  | F64Add
  | F64Sub
  | F64Mul
  | F64Div
  | F64Min
  | F64Max
  | F64Copysign
  | I32wrapI64
  | I32TruncSF32
  | I32TruncUF32
  | I32TruncSF64
  | I32TruncUF64
  | I64ExtendSI32
  | I64ExtendUI32
  | I64TruncSF32
  | I64TruncUF32
  | I64TruncSF64
  | I64TruncUF64
  | F32ConvertSI32
  | F32ConvertUI32
  | F32ConvertSI64
  | F32ConvertUI64
  | F32DemoteF64
  | F64ConvertSI32
  | F64ConvertUI32
  | F64ConvertSI64
  | F64ConvertUI64
  | F64PromoteF32
  | I32ReinterpretF32
  | I64ReinterpretF64
  | F32ReinterpretI32
  | F64ReinterpretI64

type Code = Op list
