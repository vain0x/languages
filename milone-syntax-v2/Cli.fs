module rec MiloneSyntaxV2.Cli

module M = MiloneStd.StdMap
module Pos = MiloneSyntaxV2.Source.Pos
module Range = MiloneSyntaxV2.Source.Range
module S = MiloneSyntaxV2.Syntax
module ST = MiloneSyntaxV2.SyntaxTokenize
module SP = MiloneSyntaxV2.SyntaxParse
module SS = MiloneSyntaxV2.SyntaxScope

let assertEq<'T when 'T: equality> actual expected =
  if System.Collections.Generic.EqualityComparer<'T>
       .Default.Equals(actual, expected)
     |> not then
    failwithf "Assertion violated: \n\nactual = %A\n\nexpected = %A\n" actual expected

let tests = ResizeArray<string * (unit -> unit)>()

let test (title: string) (action: unit -> unit): unit = tests.Add(title, action)

let cliTest (): int =
  test "arith" (fun () -> 2 + 3 |> assertEq 5)

  let mutable pass = 0
  let mutable failed = ResizeArray()

  for title, action in tests do
    try
      action ()
      pass <- pass + 1
    with err -> failed.Add(title, err)

  let status, code =
    if failed.Count <> 0 then "FAIL", 1
    else if pass = 0 then "EMPTY", 1
    else "OK", 0

  if failed.Count > 0 then
    for title, err in failed do
      eprintfn "FAIL %s: %A" title err

  let fail = failed.Count
  let total = pass + fail
  eprintfn "%s - pass %d / fail %d / total %d" status pass fail total
  code

let cliMain (input: string): int =
  if cliTest () <> 0 then
    exit 1

  let mutable ok = true

  let tokenizeResult =
    ST.tokenizeString (S.tokenizeHostNew ()) input

  if tokenizeResult.Errors |> List.isEmpty |> not then
    for S.SyntaxError (msg, range) in tokenizeResult.Errors do
      eprintfn "Bad token: %s at %s" msg (Range.toString range)
      ok <- false

  let nameMap =
    tokenizeResult.Tokens
    |> List.fold
         (fun map token ->
           match token with
           | S.TokenData (S.IdentToken, text, pos) -> map |> Map.add pos text
           | _ -> map)
         Map.empty

  let lookup pos =
    nameMap
    |> Map.tryFind pos
    |> Option.defaultValue ""

  let root = SP.parseTokens tokenizeResult.Tokens
  printfn "%s" (sprintf "%A" root)

  let scope = SS.resolveScopes root
  printfn "Node: %s" (sprintf "%A" scope.RootScope)

  let rec dumpSymbolTree indent parentPos =
    for (kind, name), pos in scope.SymbolTree
                             |> M.tryFind parentPos
                             |> Option.defaultValue (M.empty compare)
                             |> M.toList do
      printfn "%s%A %s@%s:" indent kind name (Pos.toString pos)
      dumpSymbolTree (indent + "  ") pos

  printfn "SymbolTree:"

  let indent = "  "

  for kind, pos in scope.ToplevelSymbols do
    match kind with
    | S.ValueNs -> printfn "%s value %s@%s" indent (lookup pos) (Pos.toString pos)
    | S.TyNs ->
        printfn "%s%A %s@%s:" indent kind (lookup pos) (Pos.toString pos)
        dumpSymbolTree (indent + "  ") pos

  for kind, name, node in scope.UnresolvedNames do
    eprintfn "Unresolved %A %A %A" kind name (M.toList node.LocalScope)

  // try
  //   printf "\n\n[EVAL] %s\n" (Eval.eval "MyProject" "MyModule" root)
  // with e ->
  //   ok <- false

  //   let stacktrace =
  //     e.StackTrace.Replace("\r\n", "\n").Split('\n')
  //     |> Array.truncate 3
  //     |> String.concat "\n"

  //   printf "EVAL FAILED: %s\n%s ..." e.Message stacktrace
  //   eprintfn "%A" e

  if ok then 0 else 1
