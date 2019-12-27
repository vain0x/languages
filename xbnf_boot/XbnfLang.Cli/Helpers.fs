module XbnfLang.Helpers

open XbnfLang.Types

let inline cons head tail = head :: tail

let inline swap<'T> (first: byref<'T>) (second: byref<'T>) =
  let t = first
  first <- second
  second <- t

let noLocation = 0, 0

/// x? ==> (x / ε)
let optNode item location =
  OrNode (item, EmptyNode location, location)

/// x* ==> (x+)?
let manyNode item location =
  optNode (Many1Node (item, location)) location

/// x,+ ==> x ("," x)*
let sep1Node item sep location =
  ConcatNode (item, manyNode (ConcatNode (sep, item, location)) location, location)

/// x,* ==> (x,+)?
let sepNode item sep location =
  optNode (sep1Node item sep location) location
