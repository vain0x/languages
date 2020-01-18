module PhancieLang.Cli.Program

open System
open System.IO
open PhancieLang.SyntaxAst
open PhancieLang.SyntaxParse
open PhancieLang.Kir
open PhancieLang.KirGen

[<EntryPoint>]
let main _ =
  let sourceCode = File.ReadAllText("tests/loop/loop.phancie")
  sourceCode |> parse |> astRoot |> kirGen |> ignore
  0
