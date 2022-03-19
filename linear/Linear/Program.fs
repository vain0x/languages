module Linear.Program

open System.IO

[<EntryPoint>]
let main _ =
  let src = File.ReadAllText("tests/linear_primitive.lin")
  let tokens, errors = Linear.Tokenize.tokenizeString src

  if errors.Length <> 0 then
    for message, range in errors do
      eprintfn "ERROR: %A %s" range message

  for kind, text, range in tokens do
    printfn "%A %A %A" range kind text

  0
