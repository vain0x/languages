module rec MiloneSyntaxV2.Source

// -----------------------------------------------
// Pos
// -----------------------------------------------

/// Position in string.
[<RequireQualifiedAccess; Struct>]
[<StructuredFormatDisplay("{AsDisplay}")>]
type Pos =
  {
    /// Row index (0-indexed).
    Row: int

    /// Column index (0-indexed).
    Column: int }

  member private this.AsDisplay = this.ToString()

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
[<StructuredFormatDisplay("{AsDisplay}")>]
type Range =
  { Start: Pos
    End: Pos }

  member private this.AsDisplay = this.ToString()

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

// -----------------------------------------------
// Loc
// -----------------------------------------------

[<RequireQualifiedAccess; Struct>]
type Loc =
  { Doc: Doc
    Range: Range }

  override this.ToString() =
    let doc, range = this.Doc, this.Range
    doc + ":" + Range.toString range

module Loc =
  let ofPair doc range: Loc = { Doc = doc; Range = range }

  let toPair (loc: Loc) = loc.Doc, loc.Range

  let toString (loc: Loc) = string loc
