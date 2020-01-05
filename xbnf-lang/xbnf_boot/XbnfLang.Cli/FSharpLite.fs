module rec XbnfLang.FSharpLite

open System
open XbnfLang.FirstSet
open XbnfLang.Helpers
open XbnfLang.Nullability
open XbnfLang.Types

type FSharpBindingPower =
  | MinFbp
  | OrFbp
  | TupleFbp
  | AndFbp
  | ConsFbp
  | AppFbp
  | AtomFbp

type FSharpBin =
  | AppFB
  | ConsFB
  | AndFB
  | OrFB

type FSharpTerm =
  | IdentFT
    of string

  | StringFT
    of string

  | ListFT
    of FSharpTerm list

  | TupleFT
    of FSharpTerm list

  | BinFT
    of FSharpBin * FSharpTerm * FSharpTerm

type FSharpPat =
  | TermFP
    of FSharpTerm

  | OrFP
    of FSharpPat * FSharpPat

type FSharpExpr =
  FSharpTerm

type FSharpStmt =
  | TermFS
    of FSharpTerm

  | MatchFS
    of cond:FSharpTerm * arms:(FSharpPat * FSharpExpr option * FSharpStmt list) list

  | LetValFS
    of FSharpPat * FSharpStmt list

  | LetFunFS
    of string * string list * body:FSharpStmt list

type FSharpModule =
  | FSharpModule
    of modulePath:string list * openPaths:string list list * FSharpStmt list

type FSharpLiteOption =
  | FSharpLiteOption
    of modulePath:string list * openPaths:string list list

let appFT first second =
  BinFT (AppFB, first, second)

let appManyFT first args =
  args |> List.fold appFT first

let consFT first second =
  BinFT (ConsFB, first, second)

let andFT first second =
  BinFT (AndFB, first, second)

let orFT first second =
  BinFT (OrFB, first, second)

let consFP first second =
  TermFP (consFT first second)

let orFP first second =
  OrFP (first, second)

// -----------------------------------------------
// Gen helpers
// -----------------------------------------------

let wordToPascal (word: string) =
  assert (String.IsNullOrEmpty(word) |> not)

  Char.ToUpperInvariant(word.[0]).ToString() + word.Substring(1)

let snakeToCamelCase (name: string) =
  if name = "" || name.StartsWith("_") then name else

  name.ToLowerInvariant().Split([|'_'|])
  |> Seq.mapi (fun i word ->
    if i = 0 then
      word
    else
      word |> wordToPascal
  )
  |> String.concat ""

let snakeToPascalCase (name: string) =
  if name = "" || name.StartsWith("_") then name else

  name.ToLowerInvariant().Split([|'_'|])
  |> Seq.map wordToPascal
  |> String.concat ""

let tokenToTerm (name: string) args =
  let k cal args =
    match args with
    | [] ->
      cal

    | [arg] ->
      appFT cal arg

    | _ ->
      appFT cal (TupleFT args)

  if name.[0] = '"' || name.[0] = '\'' then
    let name = name.Replace("\'", "\"")
    k (IdentFT "PunToken") ((name.Substring(1, name.Length - 2) |> StringFT) :: args)
  else
    k (snakeToPascalCase name + "Token" |> IdentFT) args

let tokenDataTerm (name: string) args data =
  TupleFT [tokenToTerm name args; data]

let symbolToTerm name args =
  appFT (snakeToCamelCase name + "Node" |> IdentFT) args

// -----------------------------------------------
// Gen "isFollowedBy"
// -----------------------------------------------

let fsharpLiteGenIsFollowedByFuns rules =
  let append first second =
    match first with
    | (_, Some _, _)
    | (_, _, true) ->
      first

    | (firstTokens, _, _) ->
      let (secondTokens, secondSymbolOpt, secondHalt) = second
      List.append firstTokens secondTokens, secondSymbolOpt, secondHalt

  let halt (tokens, symbolOpt, _) =
    tokens, symbolOpt, true

  let rec goLook (node: NodeData) =
    match node with
    | TokenNode name, _ ->
      [[name], None, false]

    | SymbolNode name, _ ->
      [[], Some name, true]

    | EmptyNode, _ ->
      [[], None, false]

    | ConcatNode (first, second), _ ->
      first |> goLook |> List.collect (fun first ->
        second |> goLook |> List.map (fun second ->
          append first second
        ))

    | OrNode (first, second), _ ->
      List.append (goLook first) (goLook second)

    | Many1Node item, _ ->
      // x+ は x が見えた時点で OK とする。
      goLook item |> List.map halt

  let gen rule =
    match rule with
    | Rule (symbol, body, _, _) ->
      let arms =
        goLook body
        |> List.groupBy halt
        |> List.map (fun ((tokens, symbolOpt, _), _) ->
          let tokensTerm =
            match symbolOpt with
            | Some _ ->
              IdentFT "tokens"

            | None ->
              IdentFT "_"

          let pat =
            match tokens with
            | [] ->
              IdentFT "_"

            | _ ->
              tokens
              |> List.map (fun name -> tokenToTerm name [IdentFT "_"])
              |> List.reduce consFT
              |> fun head -> consFT head tokensTerm

          let guard =
            match symbolOpt with
            | Some symbol ->
              let funTerm = IdentFT ("isFollowedBy" + snakeToPascalCase symbol)
              appFT funTerm tokensTerm |> Some

            | None ->
              None

          let body =
            IdentFT "true"

          TermFP pat, guard, [TermFS body]
        )

      let arms =
        [
          yield! arms
          TermFP (IdentFT "_"), None, [TermFS (IdentFT "false")]
        ]

      let body = [MatchFS (IdentFT "tokens", arms)]

      let funName = "isFollowedBy" + snakeToPascalCase symbol
      LetFunFS (funName, ["tokens"], body)

  rules |> List.map gen

// -----------------------------------------------
// Gen parse
// -----------------------------------------------

let fsharpLiteGenParseFuns rules =
  let isNullable = isNullableFun rules
  let firstSet = firstSet isNullable rules

  let raiseTerm _name =
    appFT (IdentFT "failwith") (StringFT "Parse error")

  let pairTerm =
    TupleFT [
      IdentFT "tokens"
      IdentFT "state"
    ]

  let x = IdentFT "x"
  let a = IdentFT "a"

  let emitTokenTerm token =
    appManyFT (IdentFT "emit") [
      appFT (IdentFT "TokenEvent") (tokenDataTerm token [x] a)
      IdentFT "tokens"
      IdentFT "state"
    ]

  let rec go k (node: NodeData) =
    match node with
    | TokenNode name, _ ->
      // match tokens with
      k (
        MatchFS (
          IdentFT "tokens",
          [
            // | (TOKEN x, a) :: tokens ->
            //   emit (TokenEvent (TOKEN x, a)) tokens state
            TermFP (consFT (TupleFT [tokenToTerm name [x]; a]) (IdentFT "tokens")),
              None,
              [TermFS (emitTokenTerm name)]

            // | _ ->
            //   error
            TermFP (IdentFT "_"),
              None,
              [TermFS (raiseTerm name)]
          ])
      )

    | SymbolNode name, _ ->
      // parse<name> emit tokens state
      k (
        TermFS (
          appManyFT ("parse" + snakeToPascalCase name |> IdentFT) [
            IdentFT "emit"
            IdentFT "tokens"
            IdentFT "state"
          ])
      )

    | EmptyNode, _ ->
      // tokens, state
      k (TermFS pairTerm)

    // | ConcatNode ((OrNode (f1, f2), fa), second), a ->
    //   [
    //     LetFunFS (
    //       "f",
    //       ["tokens"; "state"],
    //       go second
    //     )
    //     yield! go node
    //   ]

    //   let node =
    //     OrNode (
    //       (ConcatNode (f1, second), fa),
    //       (ConcatNode (f2, second), fa)
    //     ), a
    //   go node

    | ConcatNode _, _ ->
      let rec flatten node =
        match node with
        | ConcatNode (first, second), _ ->
          List.append (flatten first) (flatten second)

        | _ ->
          [node]

      flatten node
      |> List.rev
      |> List.mapi (fun i node ->
        if i = 0 then
          go k node
        else
          go (fun body -> [LetValFS (TermFP pairTerm, [body])]) node
      )
      |> List.rev
      |> List.concat

    | OrNode ((Many1Node item, _), (EmptyNode _, _)), _ ->
      // item*

      let __ = IdentFT "_"
      let goTerm = appManyFT (IdentFT "go") [IdentFT "tokens"; IdentFT "state"]

      [
        LetFunFS (
          "rec go",
          ["tokens"; "state"],
          [
            MatchFS (
              IdentFT "tokens",
              [
                match item with
                | ConcatNode ((TokenNode token, _), item), _ ->
                  TermFP (consFT (TupleFT [tokenToTerm token [x]; a]) (IdentFT "tokens")),
                    None,
                    [
                      TermFS (emitTokenTerm token)
                      yield! go (fun stmt -> [LetValFS (TermFP pairTerm, [stmt])]) item
                      TermFS goTerm
                    ]

                  // | _ -> tokens, state
                  TermFP __,
                    None,
                    [TermFS pairTerm]

                | _ ->

                let pat =
                  nodeToFirstSet isNullable firstSet item
                  |> Set.toSeq
                  |> Seq.map (fun token -> consFT (TupleFT [tokenToTerm token [__]; __]) __ |> TermFP)
                  |> Seq.reduce orFP

                pat,
                  None,
                  [
                    yield! go (fun stmt -> [LetValFS (TermFP pairTerm, [stmt])]) item
                    TermFS goTerm
                  ]

                // | _ -> tokens, state
                TermFP __,
                  None,
                  [TermFS pairTerm]
              ])
          ])

        // go tokens state
        yield! k (TermFS goTerm)
      ]

    | OrNode _, _ ->
      let rec flatten node =
        match node with
        | OrNode (first, second), _ ->
          List.append (flatten first) (flatten second)

        | _ ->
          [node]

      let __ = IdentFT "_"
      let mutable handledSet = Set.empty
      let mutable isExhaustive = false

      k (
        MatchFS (
          IdentFT "tokens",
          [
            for node in flatten node do
              match node with
              | EmptyNode _, _ ->
                TermFP (IdentFT "_"),
                  None,
                  [TermFS pairTerm]

                isExhaustive <- true

              | TokenNode token, _ ->
                let x = IdentFT "x"
                let a = IdentFT "a"

                // | (TOKEN x, a) :: tokens ->
                //   emit (TokenEvent (TOKEN x, a)) tokens state
                TermFP (consFT (TupleFT [tokenToTerm token [x]; a]) (IdentFT "tokens")),
                  None,
                  [
                    TermFS (
                      appManyFT (IdentFT "emit") [
                        appFT (IdentFT "TokenEvent") (tokenDataTerm token [x] a)
                        IdentFT "tokens"
                        IdentFT "state"
                      ]
                    )
                  ]

              | _ ->

              let nodeFirst =
                let s = nodeToFirstSet isNullable firstSet node
                Set.difference s handledSet

              if nodeFirst |> Set.isEmpty |> not then
                let pat =
                  nodeFirst
                  |> Seq.map (fun token -> TermFP (consFT (TupleFT [tokenToTerm token [__]; __]) __))
                  |> Seq.reduce orFP
                pat,
                  None,
                  go (fun stmt -> [stmt]) node

              handledSet <- Set.union handledSet nodeFirst

            // | _ -> <error>
            if not isExhaustive then
              TermFP __,
                None,
                [TermFS (raiseTerm "???")]
          ])
      )

    | Many1Node item, _ ->
      let __ = IdentFT "_"
      let goTerm = appManyFT (IdentFT "go") [IdentFT "tokens"; IdentFT "state"]

      [
        LetFunFS (
          "rec go",
          ["tokens"; "state"],
          [
            yield! go (fun stmt -> [LetValFS (TermFP pairTerm, [stmt])]) item

            MatchFS (
              IdentFT "tokens",
              [
                let pat =
                  nodeToFirstSet isNullable firstSet item
                  |> Set.toSeq
                  |> Seq.map (fun token -> consFT (TupleFT [tokenToTerm token [__]; __]) __ |> TermFP)
                  |> Seq.reduce orFP

                pat,
                  None,
                  [TermFS goTerm]

                // | _ -> tokens, state
                TermFP __,
                  None,
                  [TermFS pairTerm]
              ])
          ])

        // go tokens state
        yield! k (TermFS goTerm)
      ]

  let gen rule =
    match rule with
    | Rule (symbol, body, _, _) ->
      let funName = "parse" + snakeToPascalCase symbol
      LetFunFS (funName, ["emit"; "tokens"; "state"], go (fun stmt -> [stmt]) body)

  rules |> List.map gen

// -----------------------------------------------
// Gen main
// -----------------------------------------------

let fsharpLiteGen option rules =
  match option with
  | FSharpLiteOption (modulePath, openPaths) ->
    let moduleBody =
      [
        yield! fsharpLiteGenIsFollowedByFuns rules
        yield! fsharpLiteGenParseFuns rules
      ]
    FSharpModule (modulePath, openPaths, moduleBody)

// -----------------------------------------------
// Dump
// -----------------------------------------------

let bpNext bp =
  match bp with
  | MinFbp ->
    OrFbp

  | OrFbp ->
    TupleFbp

  | TupleFbp ->
    AndFbp

  | AndFbp ->
    ConsFbp

  | ConsFbp ->
    AppFbp

  | AppFbp ->
    AtomFbp

  | AtomFbp ->

    AtomFbp

let binToBp bin =
  match bin with
  | AppFB ->
    AppFbp

  | ConsFB ->
    ConsFbp

  | AndFB ->
    AndFbp

  | OrFB ->
    OrFbp

let binToString bin =
  match bin with
  | AppFB ->
    " "

  | ConsFB ->
    " :: "

  | AndFB ->
    " && "

  | OrFB ->
    " || "

let fsharpLiteDumpPath path acc =
  path
  |> List.fold (fun (sep, acc) segment ->
    let acc = acc |> cons sep |> cons segment
    ".", acc
  ) ("", acc)
  |> snd

let fsharpLiteDumpTerm term eol acc =
  let rec go term eol (superBp: FSharpBindingPower) acc =
    let left bp acc =
      if superBp <= bp then
        acc
      else
        acc |> cons "("

    let right bp acc =
      if superBp <= bp then
        acc
      else
        acc |> cons ")"

    match term with
    | StringFT content ->
      // FIXME: escape
      acc
      |> cons "\""
      |> cons content
      |> cons "\""

    | IdentFT name ->
      acc |> cons name

    | TupleFT items ->
      let acc = acc |> left TupleFbp

      let _, acc =
        items |> List.fold (fun (sep, acc) item ->
          let acc =
            acc
            |> cons sep
            |> go item eol MinFbp
          ", ", acc
        ) ("", acc)

      acc |> right TupleFbp

    | ListFT [] ->
      acc |> cons "[]"

    | ListFT [item] ->
      acc
      |> cons "["
      |> go item eol MinFbp
      |> cons "]"

    | ListFT items ->
      let deepEol = eol + "  "
      let acc = acc |> cons "["

      let acc =
        items
        |> List.fold (fun acc item ->
          acc
          |> cons deepEol
          |> go item deepEol MinFbp
        ) acc

      acc
      |> cons eol
      |> cons "]"

    | BinFT (bin, first, second) ->
      let bp = binToBp bin

      acc
      |> left bp
      |> go first eol bp
      |> cons (binToString bin)
      |> go second eol (bpNext bp)
      |> right bp

  acc |> go term eol MinFbp

let fsharpLiteDumpPat pat eol acc =
  match pat with
  | TermFP term ->
    acc |> fsharpLiteDumpTerm term eol

  | OrFP (first, second) ->
    acc
    |> fsharpLiteDumpPat first eol
    |> cons eol
    |> cons "| "
    |> fsharpLiteDumpPat second eol

let fsharpLiteDumpStmt stmt eol acc =
  let deepEol = eol + "  "

  match stmt with
  | TermFS term ->
    acc |> fsharpLiteDumpTerm term eol

  | MatchFS (cond, arms) ->
    assert (arms |> List.isEmpty |> not)

    let acc =
      acc
      |> cons "match "
      |> fsharpLiteDumpTerm cond eol
      |> cons " with"

    let _, acc =
      arms |> List.fold (fun (sep, acc) (pat, guardOpt, body) ->
        let acc =
          acc
          |> cons sep
          |> cons eol
          |> cons "| "
          |> fsharpLiteDumpPat pat eol

        let acc =
          match guardOpt with
          | Some guard ->
            acc
            |> cons deepEol
            |> cons "when "
            |> fsharpLiteDumpTerm guard eol

          | None ->
            acc

        let acc =
          acc
          |> cons " ->"
          |> cons deepEol
          |> fsharpLiteDumpSemi body deepEol
        "\n", acc
      ) ("", acc)

    acc

  | LetValFS (pat, body) ->
    acc
    |> cons "let "
    |> fsharpLiteDumpPat pat eol
    |> cons " ="
    |> cons deepEol
    |> fsharpLiteDumpSemi body deepEol

  | LetFunFS (name, args, body) ->
    let acc =
      acc
      |> cons "let "
      |> cons name

    let acc =
      args |> List.fold (fun acc name ->
        acc |> cons " " |> cons name
      ) acc

    acc
    |> cons " ="
    |> cons deepEol
    |> fsharpLiteDumpSemi body deepEol

let fsharpLiteDumpSemi stmts eol acc =
  assert (stmts |> List.isEmpty |> not)

  let doubleEol = "\n" + eol

  stmts |> List.fold (fun (sep, acc) stmt ->
    let acc =
      acc
      |> cons sep
      |> fsharpLiteDumpStmt stmt eol
    doubleEol, acc
  ) ("", acc)
  |> snd

let fsharpLiteDumpModule m eol acc =
  match m with
  | FSharpModule (modulePath, openPaths, body) ->
    let doubleEol = "\n" + eol

    let acc =
      acc
      |> cons "module rec "
      |> fsharpLiteDumpPath modulePath

    let _, acc =
      openPaths
      |> List.fold (fun (sep, acc) openPath ->
        let acc =
          acc
          |> cons sep
          |> cons "open "
          |> fsharpLiteDumpPath openPath
        eol, acc
      ) (doubleEol, acc)

    body |> List.fold (fun acc stmt ->
      acc
      |> cons doubleEol
      |> fsharpLiteDumpStmt stmt eol
    ) acc

let fsharpLiteDump m =
  []
  |> fsharpLiteDumpModule m "\n"
  |> List.rev
  |> String.concat ""
