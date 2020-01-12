module Tests

open System
open System.IO
open RaiiLang.Cir
open RaiiLang.CirDump
open RaiiLang.CirGen
open RaiiLang.KirBorrowCheck
open RaiiLang.KirDump
open RaiiLang.KirGen
open RaiiLang.KirInfer
open RaiiLang.SyntaxAst
open RaiiLang.SyntaxParse
open RaiiLang.SyntaxTokenize
open Xunit

let inline is< ^T> (expected: ^T) (actual: ^T) =
  Assert.Equal(actual, expected)

let findTestsDirectory () =
  let cwd = Environment.CurrentDirectory
  Seq.unfold (fun (dir: string) -> let p = Path.GetDirectoryName(dir) in Some (p, p)) cwd
  |> Seq.take 10
  |> Seq.find (fun (dir: string) -> Path.GetFileName(dir) = "raii-lang")
  |> fun dir -> Path.Combine(dir, "tests")

let snapshotTest (name: string) =
  let testsDir = findTestsDirectory ()

  let sourceName = sprintf "%s/%s/%s.raii" testsDir name name
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

  sourceCode
  |> parse
  |> tee "_parse_snapshot.txt" nodeToSnapshot
  |> astRoot
  |> kirGen
  |> kirInfer
  |> kirBorrowCheck
  |> tee "_dump_snapshot.txt" kirDump
  |> cirGen
  |> tee ".c" cirDump
  |> ignore

  true

[<Fact>]
let testSnapshots () =
  let testsDir = findTestsDirectory ()

  Directory.EnumerateDirectories(testsDir)
  |> Seq.map (Path.GetFileName >> snapshotTest)
  |> Seq.length
  |> fun n -> n >= 1 |> is true
