module XbnfLang.Reduce

open XbnfLang.Helpers
open XbnfLang.Types

let rec reduceNode (node: Node) =
  match node with
  | StrNode ("", location) ->
    EmptyNode location

  | Many1Node (EmptyNode location, _) ->
    EmptyNode location

  | ConcatNode (EmptyNode _, EmptyNode _, location) ->
    EmptyNode location

  | ConcatNode (EmptyNode _, item, _)
  | ConcatNode (item, EmptyNode _, _) ->
    item

  | StrNode _
  | TokenNode _
  | SymbolNode _
  | EmptyNode _ ->
    node

  | Many1Node (item, location) ->
    Many1Node (reduceNode item, location)

  | ConcatNode (first, second, location) ->
    ConcatNode (reduceNode first, reduceNode second, location)

  | OrNode (first, second, location) ->
    OrNode (reduceNode first, reduceNode second, location)

let reduceRule rule =
  match rule with
  | Rule (ruleId, body, location) ->
    Rule (ruleId, reduceNode body, location)

let reduce rules =
  rules |> List.map reduceRule
