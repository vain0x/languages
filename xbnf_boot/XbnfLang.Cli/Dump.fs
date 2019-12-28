module XbnfLang.Dump

open XbnfLang.Helpers
open XbnfLang.Types

let eol = "\n"

/// Binding power.
type Bp =
  | OrBp
  | ConcatBp
  | SuffixBp
  | AtomBp

let dumpTerm term acc =
  let rec go (term, _) (superBp: Bp) acc =
    let left (bp: Bp) acc =
      if superBp <= bp then
        acc
      else
        acc |> cons "("

    let right (bp: Bp) acc =
      if superBp <= bp then
        acc
      else
        acc |> cons ")"

    match term with
    | TokenTerm name ->
      acc |> cons name

    | SymbolTerm name ->
      acc |> cons name

    | OptTerm item ->
      acc |> left SuffixBp |> go item SuffixBp |> cons "?" |> right SuffixBp

    | ManyTerm item ->
      acc |> left SuffixBp |> go item SuffixBp |> cons "*" |> right SuffixBp

    | Many1Term item ->
      acc |> left SuffixBp |> go item SuffixBp |> cons "+" |> right SuffixBp

    | SepTerm (item, sep) ->
      acc |> cons "(" |> go item OrBp |> cons ")" |> cons sep |> cons "*"

    | Sep1Term (item, sep) ->
      acc |> cons "(" |> go item OrBp |> cons ")" |> cons sep |> cons "+"

    | ConcatTerm (first, second) ->
      acc
      |> left ConcatBp
      |> go first ConcatBp
      |> cons " "
      |> go second ConcatBp
      |> right ConcatBp

    | OrTerm (first, second) ->
      acc
      |> left OrBp
      |> go first OrBp
      |> cons " | "
      |> go second OrBp
      |> right OrBp

  acc |> go term OrBp

let dumpStmt stmt acc =
  match stmt with
  | RuleStmtTerm (name, body, comments, _) ->
    let acc =
      comments |> List.fold (fun acc comment ->
        acc |> cons "# " |> cons comment |> cons eol
      ) acc

    let altHead = String.replicate (name.Length + 1) " " + "| "
    let acc = acc |> cons name |> cons " "

    let bodies =
      let rec go body =
        match body with
        | OrTerm (first, second), _ ->
          List.append (go first) (go second)

        | _ ->
          [body]

      go body

    bodies
    |> List.fold (fun (head, acc) body ->
      let acc = acc |> cons head |> dumpTerm body |> cons eol
      altHead, acc
    ) ("= ", acc)
    |> snd

let dump stmts =
  let altHead = eol

  stmts
  |> List.fold (fun (head, acc) stmt ->
    let acc = acc |> cons head |> dumpStmt stmt
    altHead, acc
  ) ("", [])
  |> snd
  |> List.rev
  |> String.concat ""
