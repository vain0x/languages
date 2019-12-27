module XbnfLang.FirstSet

open System.Collections.Generic
open XbnfLang.Helpers
open XbnfLang.Nullability
open XbnfLang.Types

let nodeToFirstSet isNullable firstSet node =
  let rec go node =
    match node with
    | EmptyNode _ ->
      Set.empty

    | TokenNode _ ->
      Set.singleton node

    | SymbolNode (name, _) ->
      firstSet name

    | Many1Node (item, _) ->
      go item

    | ConcatNode (first, second, _) ->
      if nodeIsNullable isNullable first then
        go first |> Set.union (go second)
      else
        go first

    | OrNode (first, second, _) ->
      go first
      |> Set.union (go second)

  go node

let ruleToFirstSet isNullable firstSet rule =
  match rule with
  | Rule (_, body, _) ->
    nodeToFirstSet isNullable firstSet body

let firstSet isNullable rules =
  let map = HashMap()

  let firstSet (name: string) =
    match map.TryGetValue(name) with
    | true, set ->
      set

    | false, _ ->
      Set.empty

  let mutable stuck = false

  while not stuck do
    stuck <- true

    for rule in rules do
      match rule with
      | Rule (name, body, _) ->
        let set = nodeToFirstSet isNullable firstSet body
        if set <> firstSet name then
          map.[name] <- set
          stuck <- false

  firstSet
