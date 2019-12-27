module XbnfLang.Lower

open XbnfLang.Helpers
open XbnfLang.Types

let rec lowerTerm (term: Term) =
  match term with
  | TokenTerm ("\"\"", location)
  | TokenTerm ("''", location) ->
    EmptyNode location

  | TokenTerm (name, location) ->
    TokenNode (name, location)

  | SymbolTerm (name, location) ->
    SymbolNode (name, location)

  | OptTerm (item, location) ->
    let item = lowerTerm item
    optNode item location

  | ManyTerm (item, location) ->
    let item = lowerTerm item
    manyNode item location

  | Many1Term (item, location) ->
    let item = lowerTerm item
    Many1Node (item, location)

  | SepTerm (item, sep, location) ->
    let item = lowerTerm item
    let sep = TokenNode (sprintf "\"%s\"" sep, location)
    sepNode item sep location

  | Sep1Term (item, sep, location) ->
    let item = lowerTerm item
    let sep = TokenNode (sprintf "\"%s\"" sep, location)
    sep1Node item sep location

  | ConcatTerm (first, second, location) ->
    let first = lowerTerm first
    let second = lowerTerm second
    ConcatNode (first, second, location)

  | OrTerm (first, second, location) ->
    let first = lowerTerm first
    let second = lowerTerm second
    OrNode (first, second, location)

let lowerStmt stmt =
  match stmt with
  | RuleStmtTerm (name, body, location) ->
    Rule (name, lowerTerm body, location)

let lower (stmts: StmtTerm list): Rule list =
  stmts |> List.map lowerStmt
