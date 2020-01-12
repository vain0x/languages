module RaiiLang.Cli.Program

open System
open System.IO
open RaiiLang.SyntaxAst
open RaiiLang.SyntaxParse
open RaiiLang.Kir
open RaiiLang.KirGen
open RaiiLang.KirInfer
open RaiiLang.KirBorrowCheck

[<EntryPoint>]
let main _ =
  let sourceCode = File.ReadAllText("tests/swap/swap.raii")
  sourceCode |> parse |> astRoot |> kirGen |> kirInfer |> kirBorrowCheck |> ignore
  0
