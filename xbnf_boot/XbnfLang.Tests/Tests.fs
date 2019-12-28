module XbnfLang.Tests.Snapshots

open System
open System.IO
open XbnfLang.Analyze
open XbnfLang.Dump
open XbnfLang.Lower
open XbnfLang.Parse
open XbnfLang.Reduce
open XbnfLang.Sugar
open XbnfLang.Tokenize
open Xunit

let inline is< ^T> (expected: ^T) (actual: ^T) =
  Assert.Equal(actual, expected)

let findTestsDirectory () =
  let cwd = Environment.CurrentDirectory
  Seq.unfold (fun (dir: string) -> let p = Path.GetDirectoryName(dir) in Some (p, p)) cwd
  |> Seq.take 10
  |> Seq.find (fun (dir: string) -> Path.GetFileName(dir) = "xbnf-lang")
  |> fun dir -> Path.Combine(dir, "xbnf_boot/tests")

let snapshotTest (name: string) =
  let testsDir = findTestsDirectory ()

  let sourceName = sprintf "%s/%s/%s.xbnf" testsDir name name
  if File.Exists(sourceName) |> not then
    false
  else

  let withLog title f x =
    let y = f x
    let fileName =
      sprintf "%s/%s/%s_%s_snapshot.txt" testsDir name name title
    let content =
      match box y with
      | :? string as y ->
        y
      | _ ->
        sprintf "%A" y
    File.WriteAllText(fileName, content.TrimEnd() + "\n")
    y

  let sourceCode = File.ReadAllText(sourceName)

  sourceCode
  |> tokenize
  |> withLog "parse" parse
  |> lower
  |> reduce
  |> analyze
  |> sugar
  |> withLog "dump" dump
  |> ignore

  true

[<Fact>]
let snapshotTests () =
  let testsDir = findTestsDirectory ()

  Directory.EnumerateDirectories(testsDir)
  |> Seq.map (Path.GetFileName >> snapshotTest)
  |> Seq.length
  |> fun n -> n >= 1 |> is true
