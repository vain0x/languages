module XbnfLang.Analyze

open XbnfLang.FirstSet
open XbnfLang.FollowSet
open XbnfLang.Helpers
open XbnfLang.Nullability
open XbnfLang.Types

let analyze rules =
  let isNullable = isNullableFun rules
  let firstSet = firstSet isNullable rules
  let followSet = followSet isNullable firstSet rules

  let analyzeRule rule =
    match rule with
    | Rule (name, body, comments, location) ->
      let comments =
        [ yield! comments

          if body |> nodeIsNullable isNullable then
            "NULLABLE"

          let first =
            firstSet name |> Set.toList |> String.concat ", "

          "FIRST = {" + first + "}"

          let first =
            followSet name |> Set.toList |> String.concat ", "

          "FOLLOW = {" + first + "}" ]

      Rule(name, body, comments, location)

  rules |> List.map analyzeRule
