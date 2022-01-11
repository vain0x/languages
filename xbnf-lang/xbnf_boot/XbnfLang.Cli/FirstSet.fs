module XbnfLang.FirstSet

open System.Collections.Generic
open XbnfLang.Helpers
open XbnfLang.Nullability
open XbnfLang.Types

let nodeToFirstSet isNullable firstSet node =
  let rec go (node, _) =
    match node with
    | EmptyNode _ -> Set.empty
    | TokenNode name -> Set.singleton name
    | SymbolNode name -> firstSet name
    | Many1Node item -> go item

    | ConcatNode (first, second) ->
      if nodeIsNullable isNullable first then
        go first |> Set.union (go second)
      else
        go first

    | OrNode (first, second) -> go first |> Set.union (go second)

  go node

let ruleToFirstSet isNullable firstSet rule =
  match rule with
  | Rule (_, body, _, _) -> nodeToFirstSet isNullable firstSet body

let firstSet isNullable rules =
  let map = HashMap()

  let firstSet (name: string) =
    match map.TryGetValue(name) with
    | true, set -> set
    | false, _ -> Set.empty

  let mutable stuck = false

  while not stuck do
    stuck <- true

    for rule in rules do
      match rule with
      | Rule (name, body, _, _) ->
        let set = nodeToFirstSet isNullable firstSet body

        if set <> firstSet name then
          map.[name] <- set
          stuck <- false

  firstSet
