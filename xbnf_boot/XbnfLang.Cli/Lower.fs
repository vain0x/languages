module XbnfLang.Lower

open XbnfLang.Helpers
open XbnfLang.Types

let ruleToName rule =
  match rule with
  | Rule (name, _, _, _) ->
    name

let ruleAppend first second =
  match first, second with
  | Rule (name, body, comments, location),
    Rule (secondName, secondBody, secondComments, secondLocation) ->
    assert (name = secondName)
    let body = OrNode (body, secondBody), secondLocation
    let comments = List.append comments secondComments
    Rule (name, body, comments, location)

let ruleConcat rules =
  match rules with
  | [] ->
    failwith "NEVER"

  | rule :: rules ->
    rules |> List.fold ruleAppend rule

let rec lowerTerm (term: TermData) =
  match term with
  | (TokenTerm "\"\"", location)
  | (TokenTerm "''", location) ->
    EmptyNode, location

  | TokenTerm name, location ->
    TokenNode name, location

  | SymbolTerm name, location ->
    SymbolNode name, location

  | OptTerm item, location ->
    let item = lowerTerm item
    optNode item location

  | ManyTerm item, location ->
    let item = lowerTerm item
    manyNode item location

  | Many1Term item, location ->
    let item = lowerTerm item
    Many1Node item, location

  | SepTerm (item, sep), location ->
    let item = lowerTerm item
    let sep = TokenNode (sprintf "\"%s\"" sep), location
    sepNode item sep location

  | Sep1Term (item, sep), location ->
    let item = lowerTerm item
    let sep = TokenNode (sprintf "\"%s\"" sep), location
    sep1Node item sep location

  | ConcatTerm (first, second), location ->
    let first = lowerTerm first
    let second = lowerTerm second
    ConcatNode (first, second), location

  | OrTerm (first, second), location ->
    let first = lowerTerm first
    let second = lowerTerm second
    OrNode (first, second), location

let lowerStmt stmt =
  match stmt with
  | RuleStmtTerm (name, body, comments, location) ->
    Rule (name, lowerTerm body, comments, location)

let lower (stmts: StmtTerm list): Rule list =
  let rules = stmts |> List.map lowerStmt

  // シンボルのルールを1つにまとめる。
  rules
  |> List.groupBy ruleToName
  |> List.map (fun (_, rules) -> rules |> Seq.toList |> ruleConcat)
