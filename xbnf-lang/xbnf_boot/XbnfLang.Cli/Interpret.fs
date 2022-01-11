module rec XbnfLang.Interpret

open XbnfLang.FirstSet
open XbnfLang.FollowSet
open XbnfLang.Lower
open XbnfLang.Nullability
open XbnfLang.Parse
open XbnfLang.Tokenize
open XbnfLang.Types

/// Token of target language.
type private TokenData = string * int * int

// -----------------------------------------------
// Grammar
// -----------------------------------------------

[<RequireQualifiedAccess>]
type Grammar =
  private
    { IsNullable: string -> bool
      FirstSet: string -> Set<string>
      Rule: string -> Rule
      RootRule: string }

let parseGrammar (text: string) : Grammar =
  let rules = text |> tokenize |> parse |> lower
  let isNullable = isNullableFun rules
  let firstSet = firstSet isNullable rules
  let followSet = followSet isNullable firstSet rules

  let (Rule (root, _, _, _)) =
    rules
    |> List.tryLast
    |> Option.defaultWith (fun () -> failwith "Expected at least one rule.")

  let getRule name =
    rules
    |> List.tryFind (fun (Rule (n, _, _, _)) -> n = name)
    |> Option.defaultWith (fun () -> failwithf "Undefined rule '%s'" name)

  { IsNullable = isNullable
    FirstSet = firstSet
    Rule = getRule
    RootRule = root }

let private isNullable (g: Grammar) (node: NodeData) = nodeIsNullable g.IsNullable node

let private isFirstOf (g: Grammar) (node: NodeData) (token: string) =
  // Inefficient.
  let first =
    nodeToFirstSet g.IsNullable g.FirstSet node

  first |> Set.contains token

// -----------------------------------------------
// PNode
// -----------------------------------------------

/// Representation of parse tree.
[<RequireQualifiedAccess>]
type ParseNode =
  | Atom of TokenData
  | Paren of string * ParseNode list

/// Parse node with fragments embedded.
[<RequireQualifiedAccess>]
type private FNode =
  | Empty
  | Fragment of FNode list
  | Atom of TokenData
  | Paren of string * FNode

/// Flatten fragments.
let private flatten (element: FNode) : ParseNode list =
  let rec go element =
    match element with
    | FNode.Empty -> []
    | FNode.Atom token -> [ ParseNode.Atom token ]
    | FNode.Fragment elements -> List.collect flatten elements
    | FNode.Paren (name, children) -> [ ParseNode.Paren(name, flatten children) ]

  go element

let dumpNode node : string =
  match node with
  | ParseNode.Atom (t, _, _) -> t

  | ParseNode.Paren (name, children) ->
    "("
    + name
    + (children
       |> List.map (fun s -> " " + dumpNode s)
       |> String.concat "")
    + ")"

// -----------------------------------------------
// Parser
// -----------------------------------------------

let private lookahead (g: Grammar) (node: NodeData) tokens =
  match tokens with
  | [] -> isNullable g node
  | (t, _, _) :: _ -> isFirstOf g node t

let private parseOnNode (g: Grammar) (node: NodeData) (tokens: TokenData list) : FNode * TokenData list =
  let node, _ = node

  match node with
  | TokenNode name ->
    match tokens with
    | ((t, _, _) as token) :: tokens when t = name -> FNode.Atom token, tokens

    | _ ->
      failwithf
        "Parse error: Expected token '%s' near %A"
        name
        (tokens
         |> List.take (tokens |> List.length |> min 4))

  | SymbolNode name ->
    let rule = g.Rule name
    let name, children, tokens = parseOnRule g rule tokens
    FNode.Paren(name, children), tokens

  | EmptyNode -> FNode.Empty, tokens

  | Many1Node itemNode ->
    let rec go acc tokens =
      if lookahead g itemNode tokens then
        let item, tokens = parseOnNode g itemNode tokens
        go (item :: acc) tokens
      else
        acc, tokens

    let item, tokens = parseOnNode g itemNode tokens
    let acc, tokens = go [ item ] tokens
    FNode.Fragment(List.rev acc), tokens

  | ConcatNode (l, r) ->
    let c1, tokens = parseOnNode g l tokens
    let c2, tokens = parseOnNode g r tokens
    FNode.Fragment [ c1; c2 ], tokens

  | OrNode (l, r) ->
    let ok = lookahead g l tokens
    parseOnNode g (if ok then l else r) tokens

let private parseOnRule rules rule tokens : string * FNode * TokenData list =
  let (Rule (name, body, _, _)) = rule
  let children, tokens = parseOnNode rules body tokens
  name, children, tokens

let parseTokensWith (g: Grammar) (tokens: TokenData list) : ParseNode * TokenData list =
  let name, children, tokens =
    parseOnRule g (g.Rule g.RootRule) tokens

  ParseNode.Paren(name, flatten children), tokens
