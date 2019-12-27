module XbnfLang.Analyze

open XbnfLang.FirstSet
open XbnfLang.Helpers
open XbnfLang.Nullability
open XbnfLang.Types

let analyze rules =
  let isNullable = isNullableFun rules
  let firstSet = firstSet isNullable rules

  let analyzeRule rule =
    match rule with
    | Rule (name, body, comments, location) ->
      let comments =
        [
          yield! comments

          if body |> nodeIsNullable isNullable then
            "NULLABLE"

          let set = firstSet name
          let first = set |> Set.toList |> String.concat ", "
          "FIRST = {" + first + "}"
        ]
      Rule (name, body, comments, location)

  rules |> List.map analyzeRule
