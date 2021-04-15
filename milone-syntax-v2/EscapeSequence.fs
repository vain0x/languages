module MiloneSyntaxV2.EscapeSequence

module C = MiloneStd.StdChar
module S = MiloneStd.StdString

// -----------------------------------------------
// Int
// -----------------------------------------------

let private intToHexWithPadding (len: int) (value: int) =
  if value < 0 then
    failwith "intToHexWithPadding: unimplemented negative"
  else

    assert (len >= 0)

    let rec go acc len (n: int) =
      if n = 0 && len <= 0 then
        acc
      else
        let d = n % 16
        let s = "0123456789abcdef" |> S.slice d (d + 1)
        go (s + acc) (len - 1) (n / 16)

    if value = 0 && len = 0 then "0" else go "" len value

let private intFromHex (l: int) (r: int) (s: string) =
  assert (0 <= l && l < r && r <= s.Length)

  let hexDigitToInt (c: char) =
    if '0' <= c && c <= '9' then
      int c - int '0'
    else if 'A' <= c && c <= 'F' then
      int c - int 'A' + 10
    else if 'a' <= c && c <= 'f' then
      int c - int 'a' + 10
    else
      assert false
      0

  let rec go (acc: int) (i: int) =
    if i = r then
      acc
    else
      let d = hexDigitToInt s.[i]
      go (acc * 16 + d) (i + 1)

  go 0 l

// -----------------------------------------------
// Char
// -----------------------------------------------

let charNeedsEscaping (c: char) =
  C.isControl c || c = '\\' || c = '"' || c = '\''

let charEscape (c: char) =
  assert (c |> charNeedsEscaping)

  match c with
  | '\x00' ->
      // C-style.
      "\\0"

  | '\t' -> "\\t"

  | '\n' -> "\\n"

  | '\r' -> "\\r"

  | '\'' -> "\\\'"

  | '\"' -> "\\\""

  | '\\' -> "\\\\"

  | c ->
      let h = c |> int |> intToHexWithPadding 2
      "\\x" + h

// -----------------------------------------------
// String
// -----------------------------------------------

let private strConcat (xs: string list) = S.concat "" xs

let strNeedsEscaping (str: string) =
  let rec go i =
    i < str.Length
    && (charNeedsEscaping str.[i] || go (i + 1))

  go 0

let strEscape (str: string) =
  let rec go acc i =
    /// Finds the end index of the maximum non-escaping segment
    /// that starts at `l`.
    let rec raw i =
      if i = str.Length || charNeedsEscaping str.[i]
      then i
      else raw (i + 1)

    // Skip the non-escape segment that starts at `i`.
    let i, acc =
      let r = raw i
      r, (str |> S.slice i r) :: acc

    if i = str.Length then
      acc
    else
      let t = str.[i] |> charEscape
      go (t :: acc) (i + 1)

  if str |> strNeedsEscaping |> not then str else go [] 0 |> List.rev |> strConcat
