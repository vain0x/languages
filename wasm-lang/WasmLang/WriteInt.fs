module rec WasmLang.WriteInt

open WasmLang.MemoryBuffer

let writeUint8 (value: uint8) (dest: MemoryBuffer): int =
  MemoryBuffer.push value dest
  1

let writeUint16 (value: uint16) (dest: MemoryBuffer): int =
  MemoryBuffer.push (uint8 (value &&& uint16 0xff)) dest
  MemoryBuffer.push (uint8 ((value >>> 8) &&& uint16 0xff)) dest
  2

let writeUint32 (value: uint) (dest: MemoryBuffer): int =
  dest
  |> writeUint16 (uint16 (value &&& uint 0xffff))
  |> ignore

  dest
  |> writeUint16 (uint16 ((value >>> 16) &&& uint 0xffff))
  |> ignore

  4

let writeUint64 (value: uint64) (dest: MemoryBuffer): int =
  dest
  |> writeUint32 (uint (value &&& uint64 0xffffffff))
  |> ignore

  dest
  |> writeUint32 (uint ((value >>> 32) &&& uint64 0xffffffff))
  |> ignore

  8

let writeVarUint1 (value: uint8) (dest: MemoryBuffer): int = writeUint8 value dest

let writeVarInt7 (value: int8) (dest: MemoryBuffer): int =
  writeUint8 (uint8 value ^^^ uint8 0x80) dest

let writeVarUint32 (value: uint32) (dest: MemoryBuffer): int =
  let least7Bits (value: uint32) = uint8 (value &&& uint32 0x7f)

  let rec go (cur: uint8) len (value: uint32) dest =
    if value = uint32 0 then
      cur, len, dest
    else
      MemoryBuffer.push (cur ||| uint8 0x80) dest
      go (least7Bits value) (len + 1) (value >>> 7) dest

  let cur = least7Bits value
  let value = value >>> 7
  let cur, len, dest = go cur 0 value dest
  MemoryBuffer.push cur dest
  len + 1

let writeVarInt32 (value: int) (dest: MemoryBuffer): int =
  let least7Bits (value: int) = uint8 (value &&& 0x7f)

  let endValue = if value >= 0 then 0 else -1

  let rec go (cur: uint8) len (value: int) dest =
    if value = endValue then
      cur, len, dest
    else
      MemoryBuffer.push (cur ||| uint8 0x80) dest
      go (least7Bits value) (len + 1) (value >>> 7) dest

  let cur = least7Bits value
  let value = value >>> 7
  let cur, len, dest = go cur 1 value dest
  MemoryBuffer.push cur dest
  len

let writeVarInt64 (value: int64) (dest: MemoryBuffer): int =
  let least7Bits (value: int64) = uint8 (value &&& int64 0x7f)

  let endValue = if value >= int64 0 then int64 0 else int64 (-1)

  let rec go (cur: uint8) len (value: int64) dest =
    if value = endValue then
      cur, len, dest
    else
      MemoryBuffer.push (cur ||| uint8 0x80) dest
      go (least7Bits value) (len + 1) (value >>> 7) dest

  let cur = least7Bits value
  let value = value >>> 7
  let cur, len, dest = go cur 1 value dest
  MemoryBuffer.push cur dest
  len

let writeBytes (bytes: byte array) (dest: MemoryBuffer): int =
  dest.AddRange(bytes)
  dest.Count
