module RaiiLang.Cli.Program

open System
open System.IO
open RaiiLang.SyntaxAst
open RaiiLang.SyntaxParse
open RaiiLang.Kir
open RaiiLang.KirGen

[<EntryPoint>]
let main _ =
  let sourceCode = File.ReadAllText("tests/loop/loop.raii")
  sourceCode |> parse |> astRoot |> kirGen |> ignore
  0
