module Joy.Program

open System
open FSharp.Text.Lexing

[<EntryPoint>]
let main _ =
  let input = stdin.ReadLine()
  let lexbuf = LexBuffer<_>.FromString input

  let value =
    try
      Parser.root Lexer.read lexbuf
    with
    | _ -> reraise ()

  printfn "%d" value
  0
