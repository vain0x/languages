module LambdaCli.Program

open System
open LambdaDomain.Location
open LambdaSyntax.Tokenize
open LambdaSyntax.Parse
open LambdaSyntax.ScopeRes
open LambdaSyntax.HirGen

let private dumpTokens (tokens: _ array, errors: _ array) =
  if errors |> Array.isEmpty |> not then
    printfn "ERRORS(%d)" errors.Length

    for text, range in errors do
      printfn "    ErrorToken(%O) %A" range text

    printfn ""

  printfn "TOKENS(%d)" tokens.Length

  for kind, text, range in tokens do
    printfn "    %A(%O) %A" kind range text

[<EntryPoint>]
let main _ =
  // stdin.ReadToEnd() |> tokenizeString |> dumpTokens
  let tokens, errors = stdin.ReadToEnd() |> tokenizeString

  if Array.isEmpty errors |> not then
    dumpTokens (tokens, errors)
    exit 1

  let ast = parseTokens tokens
  // printfn "%A" ast

  let scopeResult = scopeRes ast
  // printfn "%A" scopeResult

  let hir = hirGen ast scopeResult
  printfn "%A" hir
  0
