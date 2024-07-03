module TypeCheck.Helpers

open System.Collections.Generic

// -----------------------------------------------
// Pos
// -----------------------------------------------

/// 文字列上の位置 (UTF-16 基準)
[<Struct>]
type Pos =
  { Index: int
    Row: int
    Column: int }

  static member Zero = Pos.Create(0, 0, 0)

  static member Create(index: int, row: int, column: int) : Pos =
    { Index = index
      Row = row
      Column = column }

  static member (+)(left: Pos, right: Pos) =
    let index = left.Index + right.Index

    if right.Row = 0 then
      Pos.Create(index, left.Row, left.Column + right.Column)
    else
      Pos.Create(index, left.Row + right.Row, right.Column)

  override this.ToString() =
    sprintf "%d:%d" (this.Row + 1) (this.Column + 1)

module Pos =
  let scan (start: int) (len: int) (s: string) : Pos =
    let endIndex = start + len
    assert (endIndex <= s.Length)

    let rec go (row: int) (head: int) =
      let index = s.IndexOf("\n", head)

      if index < 0 || index >= endIndex then
        Pos.Create(len, row, endIndex - head)
      else
        go (row + 1) (index + 1)

    go 0 start

// -----------------------------------------------
// Range
// -----------------------------------------------

/// 文字列上の範囲
[<Struct>]
type Range =
  { Start: Pos
    End: Pos }

  static member Create(start: Pos, endPos: Pos) : Range = { Start = start; End = endPos }

  override this.ToString() =
    let py = this.Start.Row + 1
    let px = this.Start.Column + 1
    let qy = this.End.Row + 1
    let qx = this.End.Column + 1
    sprintf "%d.%d-%d:%d" py px qy qx

// -----------------------------------------------
// string
// -----------------------------------------------

/// 16進展開
let hex (n: int) = sprintf "%x" n

/// i 番目の文字
let nth (i: int) (s: string) : char = if i < s.Length then s.[i] else '\x00'

/// 部分文字列
let substr (l: int) (r: int) (s: string) = if l < r then s.[l .. r - 1] else ""

/// 条件を満たすプレフィックスの長さ
let prefixLen (i: int) (pred: char -> bool) (s: string) =
  let start = i
  let mutable i = i

  while i < s.Length && pred s.[i] do
    i <- i + 1

  i - start

/// true ならカッコで囲む
let parenIf (cond: bool) (s: string) = if cond then sprintf "(%s)" s else s
