module Tests

open System
open System.IO
open PhancieLang.Ast
open PhancieLang.AstAnalyze
open PhancieLang.AstGen
open PhancieLang.Cir
open PhancieLang.CirDump
open PhancieLang.CirGen
open PhancieLang.KirDump
open PhancieLang.KirGen
open PhancieLang.SyntaxParse
open PhancieLang.SyntaxTokenize
open Xunit

let inline is< ^T> (expected: ^T) (actual: ^T) =
  Assert.Equal(actual, expected)

let findTestsDirectory () =
  let cwd = Environment.CurrentDirectory
  Seq.unfold (fun (dir: string) -> let p = Path.GetDirectoryName(dir) in Some (p, p)) cwd
  |> Seq.take 10
  |> Seq.find (fun (dir: string) -> Path.GetFileName(dir) = "phancie-lang")
  |> fun dir -> Path.Combine(dir, "tests")

let snapshotTest (name: string) =
  let testsDir = findTestsDirectory ()

  let sourceName = sprintf "%s/%s/%s.phancie" testsDir name name
  if File.Exists(sourceName) |> not then
    false
  else

  let writeLog suffix x =
    let fileName =
      sprintf "%s/%s/%s%s" testsDir name name suffix
    let content =
      match box x with
      | :? string as x ->
        x
      | _ ->
        sprintf "%A" x
    File.WriteAllText(fileName, content.TrimEnd() + "\n")

  let tee suffix f x =
    writeLog suffix (f x)
    x

  let sourceCode = File.ReadAllText(sourceName)
  let aErrors = ResizeArray()

  sourceCode
  |> parse
  |> tee "_parse_snapshot.txt" nodeToSnapshot
  |> astRoot
  |> fun ast -> astAnalyze aErrors ast; ast
  |> kirGen
  |> tee "_dump_snapshot.txt" kirDump
  |> cirGen
  |> tee ".c" cirDump
  |> ignore

  writeLog "_error_snapshot.txt" (aErrors |> Seq.map aErrorToString |> String.concat "\n\n")

  true

[<Fact>]
let testSnapshots () =
  let testsDir = findTestsDirectory ()

  Directory.EnumerateDirectories(testsDir)
  |> Seq.map (Path.GetFileName >> snapshotTest)
  |> Seq.length
  |> fun n -> n >= 1 |> is true
