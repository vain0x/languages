/// Source location information.
module LambdaDomain.Location

// -----------------------------------------------
// Pos
// -----------------------------------------------

/// Position in string.
[<RequireQualifiedAccess; Struct>]
type Pos =
  {
    /// Row index (0-indexed).
    Row: int

    /// Column index (0-indexed).
    Column: int }

  override this.ToString() =
    let row, column = this.Row, this.Column
    string (row + 1) + ":" + string (column + 1)

module Pos =
  let zero: Pos = { Row = 0; Column = 0 }

  let ofPair (row, column): Pos = { Row = row; Column = column }

  let toPair (pos: Pos) = pos.Row, pos.Column

  let toString (pos: Pos) = string pos

// -----------------------------------------------
// Range
// -----------------------------------------------

[<RequireQualifiedAccess>]
type Range =
  { Start: Pos
    End: Pos }

  override this.ToString() =
    let s, t = this.Start, this.End
    let sy, sx = Pos.toPair s
    let ty, tx = Pos.toPair t

    string (sy + 1)
    + "."
    + string (sx + 1)
    + "-"
    + string (ty + 1)
    + "."
    + string (tx + 1)

module Range =
  let ofPair (s, t): Range = { Start = s; End = t }

  let toPair (range: Range) = range.Start, range.End

  let toString (range: Range) = string range

// -----------------------------------------------
// Doc
// -----------------------------------------------

type Doc = string

[<RequireQualifiedAccess; Struct>]
type Loc =
  { Doc: Doc
    Pos: Pos }

  override this.ToString() =
    let doc, pos = this.Doc, this.Pos
    doc + ":" + Pos.toString pos

module Loc =
  let ofPair doc pos: Loc = { Doc = doc; Pos = pos }

  let toPair (loc: Loc) = loc.Doc, loc.Pos

  let toString (loc: Loc) = string loc

// -----------------------------------------------
// Cursor
// -----------------------------------------------

/// String with position.
[<RequireQualifiedAccess; Struct>]
type Cursor =
  { String: string

    /// Index in UTF-16 code units.
    Index: int

    /// Row index (0-indexed).
    Row: int

    /// Column index (0-indexed).
    Column: int }

module Cursor =
  let create (s: string): Cursor =
    { String = s
      Index = 0
      Row = 0
      Column = 0 }

  let atEof (cursor: Cursor) =
    assert (uint cursor.Index <= uint cursor.String.Length)
    cursor.Index >= cursor.String.Length

  let nth (offset: int) (cursor: Cursor): char =
    if uint (cursor.Index + offset) < uint cursor.String.Length then
      cursor.String.[cursor.Index + offset]
    else
      '\x00'

  let advanceTo (endIndex: int) (cursor: Cursor): Cursor =
    let s = cursor.String
    let endIndex = endIndex |> max 0 |> min s.Length

    let rec go i row column: Cursor =
      if i >= endIndex then
        { String = s
          Index = i
          Row = row
          Column = column }
      else if s.[i] = '\n' then
        go (i + 1) (row + 1) 0
      else
        go (i + 1) row (column + 1)

    go cursor.Index cursor.Row cursor.Column

  let advanceBy (len: int) (cursor: Cursor): Cursor = advanceTo (cursor.Index + len) cursor

  let toPos (cursor: Cursor): Pos =
    { Row = cursor.Row
      Column = cursor.Column }

  let spanBy (len: int) (cursor: Cursor) =
    let startIndex = cursor.Index
    let endIndex = startIndex + len

    let start = toPos cursor
    let cursor = advanceTo endIndex cursor
    let range = Range.ofPair (start, toPos cursor)

    startIndex, endIndex, range, cursor
