module Linear.Program

open System.IO

[<EntryPoint>]
let main _ =
  let files =
    [ "tests/auto_semi.lin"
      "tests/linear_primitive.lin"
      // "tests/safe_wrapper.lin"
      ]

  for file in files do
    printfn "file: %s" file
    let src = File.ReadAllText(file)
    let tokens, errors = Tokenize.tokenizeString src

    if errors.Length <> 0 then
      for message, range in errors do
        eprintfn "ERROR: %A %s" range message

      exit 1

    // for kind, text, range in tokens do
    //   printfn "%A %A %A" range kind text

    let ast = Parse.parseTokens tokens
    printfn "%A" ast

  0
