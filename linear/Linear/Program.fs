module Linear.Program

open System.IO
open Linear.Location

let private at (file: string) (range: Range) = sprintf "%s:%A" file range.Start

let private readTokenize (file: string) =
  let src = File.ReadAllText(file)
  let tokens, errors = Tokenize.tokenizeString src

  if errors.Length <> 0 then
    printfn "ERROR: Tokenize errors (%d)" errors.Length

    for message, range in errors do
      printfn "  %s %s" (at file range) message

    exit 1

  tokens

let private cmdParse (file: string) =
  printfn "file: %s" file
  let tokens = readTokenize file
  let ast = Parse.parseTokens tokens
  printfn "ast:\n%A" ast

let private cmdTypeCheck (file: string) =
  printfn "file: %s" file
  let tokens = readTokenize file
  let ast = Parse.parseTokens tokens
  let m = TypeCheck.typeCheck ast
  printfn "module:\n%A" m

let private cmdTypeFail (file: string) =
  printfn "file: %s" file
  let tokens = readTokenize file
  let ast = Parse.parseTokens tokens

  let result =
    try
      Ok(TypeCheck.typeCheck ast)
    with
    | ex -> Error ex

  match result with
  | Ok m ->
    printfn "module:\n%A" m
    printfn "Unexpectedly type-check succeeded."
    exit 1

  | Error ex -> printfn "Expectedly type-error occurred: %A" ex

let private cmdEval (file: string) =
  printfn "file: %s" file
  let tokens = readTokenize file
  let ast = Parse.parseTokens tokens
  let m = TypeCheck.typeCheck ast
  Eval.eval m

[<EntryPoint>]
let main _ =
  cmdParse "tests/auto_semi.lin"
  cmdEval "tests/linear_primitive.lin"
  cmdEval "tests/safe_wrapper.lin"
  cmdTypeFail "tests/issue_leaky_fun.lin"
  0
