module rec XbnfLang.Sugar

open XbnfLang.Helpers
open XbnfLang.Types

let rec sugarNode node =
  match node with
  | StrNode (content, location) ->
    StrTerm (content, location)

  | TokenNode (name, location) ->
    TokenTerm (name, location)

  | SymbolNode (name, location) ->
    SymbolTerm (name, location)

  | EmptyNode location ->
    StrTerm ("", location)

  | Many1Node (item, location) ->
    let item = sugarNode item
    Many1Term (item, location)

  | ConcatNode (first, second, location) ->
    let first = sugarNode first
    let second = sugarNode second
    ConcatTerm (first, second, location)

  | OrNode (Many1Node (item, location), EmptyNode _, _) ->
    let item = sugarNode item
    ManyTerm (item, location)

  | OrNode (first, EmptyNode _, location) ->
    let first = sugarNode first
    OptTerm (first, location)

  | OrNode (first, second, location) ->
    let first = sugarNode first
    let second = sugarNode second
    OrTerm (first, second, location)

let sugarRule rule =
  match rule with
  | Rule (name, body, location) ->
    RuleStmtTerm (name, sugarNode body, location) |> Some

let sugar rules =
  rules |> List.choose sugarRule
