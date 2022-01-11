module XbnfLang.FollowSet

open System.Collections.Generic
open XbnfLang.Helpers
open XbnfLang.Nullability
open XbnfLang.FirstSet
open XbnfLang.Types

let ruleToFirstSet isNullable firstSet rule =
  match rule with
  | Rule (_, body, _, _) -> nodeToFirstSet isNullable firstSet body

let followSet isNullable firstSet rules =
  let map = HashMap()

  let followSet (name: string) =
    match map.TryGetValue(name) with
    | true, set -> set
    | false, _ -> Set.empty

  let mutable stuck = false

  // symbol → ... node ... という規則に関して、
  // FOLLOW 集合を更新する。
  let update symbol node =
    // node に続いて、集合 follow に含まれる各字句が出現しうることが分かったとき、
    // FOLLOW 集合を更新する。
    let rec go follow (node, _) =
      match node with
      | EmptyNode _
      | TokenNode _ -> ()

      | SymbolNode name ->
        let current = followSet name

        if Set.isSubset follow current |> not then
          map.[name] <- current |> Set.union follow
          stuck <- false

      | Many1Node item ->
        // x の直後に follow の字句が出現しうることに加えて、
        // x+ → x x という導出が可能なことから、
        // x の直後に FIRST(x) の字句も出現しうることが分かる。
        let itemFollow =
          nodeToFirstSet isNullable firstSet item
          |> Set.union follow

        go itemFollow item

      | ConcatNode (first, second) ->
        go follow second

        // first の直後に FIRST(second) が出現しうることが分かる。
        // NULLABLE(second) なら FOLLOW(second) も出現しうる。
        let firstFollow =
          if nodeIsNullable isNullable second then
            nodeToFirstSet isNullable firstSet second
            |> Set.union follow
          else
            nodeToFirstSet isNullable firstSet second

        go firstFollow first

      | OrNode (first, second) ->
        go follow first
        go follow second

    // symbol → node なので、
    // symbol の直後に出現しうる字句は node の直後にも出現しうることが分かる。
    let nodeFollow = followSet symbol
    go nodeFollow node

  while not stuck do
    stuck <- true

    for rule in rules do
      match rule with
      | Rule (name, body, _, _) -> update name body

  followSet
