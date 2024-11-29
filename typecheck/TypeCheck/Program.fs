module TypeCheck.Main

open TypeCheck.Syntax
open TypeCheck.TypeChecker
open TypeCheck.TypeSimplifier

let parse (text: string) : Ast =
  let tokens = Tokenizer.tokenize text

  try
    Parser.parseTokens tokens
  with Parser.ParseException(message, _token, range) ->
    failwithf "Parse error at %O: %s" range message

let typeCheck (text: string) : Ast * InferenceResult =
  let ast = parse text
  let ctx = builtInCtx ()
  let coalesceTy = coalesceTySimplified
  let result = inferTypes coalesceTy ctx ast
  ast, result

let dumpAst (ast: Ast) : unit =
  eprintfn "Syntax:"

  for decl in ast.Decls do
    let kw =
      match decl.IsRec with
      | Rec -> "let rec"
      | NotRec -> "let"

    eprintfn "    %s %s = %A" kw decl.Name decl.Rhs

let dumpInferenceResult (result: InferenceResult) : unit =
  for (name, result) in result do
    match result with
    | Ok ty -> printfn "%O" ty

    | Error msg -> eprintfn "Type error: %s: %s" name msg

let assertEq<'T when 'T: equality> (name: string) (expected: 'T) (actual: 'T) =
  if actual <> expected then
    eprintfn "ERROR: Assertion violated '%s'" name
    eprintfn "    Actual = %O" actual
    eprintfn "    Expected = %O" expected

let test (text: string) =
  let expects: (string * string) list =
    [ for line in text.Split([| '\r'; '\n' |]) do
        if line.StartsWith("//?") then
          let line = line.Substring("//?".Length)
          let i = line.IndexOf(":")

          if i >= 0 then
            let name = line.Substring(0, i).Trim()
            let ty = line.Substring(i + 1).Trim()
            yield name, ty ]

  eprintfn "expects (%d)" expects.Length
  for v, t in expects do
    eprintfn "  val %s: %s" v t

  let _, result = typeCheck text
  dumpInferenceResult result

  List.length result |> assertEq "number of assertion" (List.length expects)

  for (name, ty), (expectedName, expected) in List.zip result expects do
    name |> assertEq "name of binding" expectedName

    match ty with
    | Ok ty -> string ty |> assertEq "type" expected

    | Error msg -> eprintfn "Type error: %s: %s" name msg

[<EntryPoint>]
let main _ =
  System.Console.InputEncoding <- System.Text.Encoding.UTF8

  let text = stdin.ReadToEnd()
  test text
  0
