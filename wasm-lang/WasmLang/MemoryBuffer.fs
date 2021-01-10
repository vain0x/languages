module rec WasmLang.MemoryBuffer

type MemoryBuffer = ResizeArray<byte>

let create (): MemoryBuffer = ResizeArray()

let push (value: byte) (array: ResizeArray<byte>): unit = array.Add(value)

let finish (array: ResizeArray<byte>): byte [] = array.ToArray()
