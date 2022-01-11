module XbnfLang.Nullability

open System.Collections.Generic
open XbnfLang.Helpers
open XbnfLang.Types

let nodeIsNullable isNullable node =
  let rec go (node, _) =
    match node with
    | EmptyNode _ -> true

    | TokenNode _ -> false

    | SymbolNode name -> isNullable name

    | Many1Node item -> go item

    | ConcatNode (first, second) -> go first && go second

    | OrNode (first, second) -> go first || go second

  go node

let ruleIsNullable isNullable rule =
  match rule with
  | Rule (_, body, _, _) -> nodeIsNullable isNullable body

let isNullableFun (rules: Rule list) : IsNullableFun =
  let set = HashSet()
  let isNullable name = set.Contains(name)

  let mutable current = ResizeArray(rules)
  let mutable next = ResizeArray()
  let mutable stuck = false

  while not stuck do
    for rule in current do
      match rule with
      | Rule (name, body, _, _) ->
        if nodeIsNullable isNullable body then
          set.Add(name) |> ignore
        else
          next.Add(rule)

    stuck <- current.Count = next.Count

    swap &current &next
    next.Clear()

  isNullable
