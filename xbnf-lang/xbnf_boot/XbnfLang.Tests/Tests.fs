module XbnfLang.Tests.Snapshots

open System
open System.IO
open XbnfLang.Analyze
open XbnfLang.Dump
open XbnfLang.FSharpLite
open XbnfLang.Interpret
open XbnfLang.Lower
open XbnfLang.Parse
open XbnfLang.Reduce
open XbnfLang.Sugar
open XbnfLang.Tokenize
open XbnfLang.Types
open Xunit

let inline is< ^T> (expected: ^T) (actual: ^T) = Assert.Equal(expected, actual)

let findTestsDirectory () =
  let cwd = Environment.CurrentDirectory

  Seq.unfold (fun (dir: string) -> let p = Path.GetDirectoryName(dir) in Some(p, p)) cwd
  |> Seq.take 10
  |> Seq.find (fun (dir: string) -> Path.GetFileName(dir) = "xbnf-lang")
  |> fun dir -> Path.Combine(dir, "xbnf_boot/tests")

let snapshotTest (name: string) =
  let testsDir = findTestsDirectory ()

  let sourceName =
    sprintf "%s/%s/%s.xbnf" testsDir name name

  if File.Exists(sourceName) |> not then
    false
  else

    let withLog title f x =
      let y = f x

      let fileName =
        sprintf "%s/%s/%s_%s_snapshot.txt" testsDir name name title

      let content =
        match box y with
        | :? string as y -> y
        | _ -> sprintf "%A" y

      File.WriteAllText(fileName, content.TrimEnd() + "\n")
      y

    let sourceCode = File.ReadAllText(sourceName)

    let rules =
      sourceCode
      |> tokenize
      |> withLog "parse" parse
      |> lower
      |> reduce
      |> analyze

    rules |> sugar |> withLog "dump" dump |> ignore

    let option =
      let modulePath = [ "XbnfLang"; "ParseV2" ]

      let openPaths =
        [ [ "XbnfLang"; "HelpersV2" ]
          [ "XbnfLang"; "TypesV2" ] ]

      FSharpLiteOption(modulePath, openPaths)

    rules
    |> fsharpLiteGen option
    |> withLog "fsharp_lite" fsharpLiteDump
    |> ignore

    true

[<Fact>]
let snapshotTests () =
  let testsDir = findTestsDirectory ()

  Directory.EnumerateDirectories(testsDir)
  |> Seq.map (Path.GetFileName >> snapshotTest)
  |> Seq.length
  |> fun n -> n >= 1 |> is true

// -----------------------------------------------
// Interpret
// -----------------------------------------------

let private interpret grammar input =
  let grammar = parseGrammar grammar

  let tokens =
    tokenize input
    |> List.filter (fun (t, _, _) -> t <> EofToken)
    |> List.map (fun (t, y, x) ->
      match t with
      | StrToken s
      | LoudToken s
      | SnakeToken s -> s, y, x
      | EofToken -> failwith "never"
      | PunToken _ -> failwithf "unexpected token (%A) at %d:%d" t y x)

  let node, tokens = parseTokensWith grammar tokens

  match tokens with
  | [] -> ()
  | (t, y, x) :: _ ->
    eprintfn "node: %s" (dumpNode node)
    failwithf "expected EOF but %A at %d:%d" t y x

  dumpNode node

[<Fact>]
let interpretTestSingleToken () =
  interpret
    """
      root = TOKEN
    """
    """
      TOKEN
    """
  |> is """(root TOKEN)"""

[<Fact>]
let interpretTestConcat () =
  interpret
    """
      left = L
      right = R
      root = left right
    """
    """
      L R
    """

  |> is """(root (left L) (right R))"""

[<Fact>]
let interpretTestOrRule () =
  interpret
    """
      digit = N | M
      plus = digit "+" digit
    """
    """
      N "+" M
    """

  |> is """(plus (digit N) "+" (digit M))"""

[<Fact>]
let interpretTestMany () =
  interpret
    """
      bit = F | T
      bits = bit+
    """
    """
      T F T T
    """

  |> is """(bits (bit T) (bit F) (bit T) (bit T))"""

[<Fact>]
let interpretTestFormula () =
  interpret
    """
      bit = F | T
      mul = bit (("*" | "/") bit)*
      add = mul (("+" | "-") mul)*
      expr = add
    """
    """
      T "+" F "*" T "-" T
    """
  |> is """(expr (add (mul (bit T)) "+" (mul (bit F) "*" (bit T)) "-" (mul (bit T))))"""

[<Fact>]
let interpretTestRecursion () =
  interpret
    """
      bit = F | T
      pow = bit ("**" pow)?
      expr = pow
    """
    """
      T "**" T "**" F
    """
  |> is """(expr (pow (bit T) "**" (pow (bit T) "**" (pow (bit F)))))"""

[<Fact>]
let interpretTestLeftRecursion () =
  interpret
    """
      plus = plus "+" N
      expr = plus
    """
    """
      N "+" N "+" N
    """
  |> is """(expr (plus (plus N "+" N) N)"""
