module rec MilonePatternCompile.CompileTests

open MilonePatternCompile.Compile

let private deeper indent = indent + "  "

let private cons h t = h :: t

// let private ppPat pat acc =
//   match pat with
//   | WildcardPat -> acc |> cons "_"
//   | VarPat name -> acc |> cons name
//   | UnitPat -> acc |> cons "()"
//   | LeftPat itemPat -> acc |> cons "Left " |> ppPat itemPat
//   | RightPat itemPat -> acc |> cons "Right " |> ppPat itemPat
//   | PairPat (p1, p2) -> acc |> ppPat p1 |> cons ", " |> ppPat p2

let private ppExpr indent expr acc =
  match expr with
  | AbortExpr _ -> acc |> cons "abort ()"

  | VarExpr name -> acc |> cons name

  | LetExpr (name, init, next) ->
      acc
      |> cons "let "
      |> cons name
      |> cons " = "
      |> ppExpr (deeper indent) init
      |> cons " in\n"
      |> cons indent
      |> ppExpr indent next

  | DoExpr (init, next) ->
      acc
      |> cons "do "
      |> ppExpr indent init
      |> cons ";\n"
      |> cons indent
      |> ppExpr indent next

  | LetPairExpr (x, y, init, next) ->
      acc
      |> cons "let "
      |> cons x
      |> cons ", "
      |> cons y
      |> cons " = "
      |> ppExpr (deeper indent) init
      |> cons " in\n"
      |> cons indent
      |> ppExpr indent next

  | EitherExpr (cond, (lVar, lBody), (rVar, rBody)) ->
      acc
      |> cons "match "
      |> ppExpr (deeper indent) cond
      |> cons " with\n"
      |> cons indent
      |> cons "| "
      |> cons lVar
      |> cons " ->\n  "
      |> cons indent
      |> ppExpr (deeper indent) lBody
      |> cons "\n"
      |> cons indent
      |> cons "| "
      |> cons rVar
      |> cons " ->\n  "
      |> cons indent
      |> ppExpr (deeper indent) rBody

  | IfExpr (cond, thenCl, elseCl) ->
      acc
      |> cons "if "
      |> ppExpr (deeper indent) cond
      |> cons " then\n  "
      |> cons indent
      |> ppExpr (deeper indent) thenCl
      |> cons "\n"
      |> cons indent
      |> cons "else\n  "
      |> cons indent
      |> ppExpr (deeper indent) elseCl

let compile arms ty =
  eprintfn "\n> cond:%A arms=%A" ty arms

  let expr, exhaust, redundant = cover arms ("cond", ty)

  eprintfn "  exhaust=%A redundant=%A" exhaust redundant
  eprintfn
    "%s"
    ([]
     |> ppExpr "" expr
     |> List.rev
     |> String.concat "")

let test () =
  let ut = UnknownTy
  let vp = VarPat
  let wp = WildcardPat
  let vx = VarExpr

  compile [ wp, None, vx "doNext ()" ] UnitTy
  compile [ UnitPat, None, vx "doNext ()" ] UnitTy
  compile [ vp "x", None, vx "useX x" ] (UnknownTy "int")

  compile
    [ LeftPat(vp "l"), None, vx "useL l"
      RightPat(vp "r"), None, vx "useR r" ]
    (EitherTy(ut "'l", ut "'r"))

  compile
    [ LeftPat(LeftPat(vp "ll")), None, VarExpr "use1 ll"
      RightPat(LeftPat(vp "rl")), None, VarExpr "use2 rl"
      LeftPat(RightPat(vp "lr")), None, VarExpr "use3 lr"
      RightPat(RightPat(vp "rr")), None, VarExpr "use4 rr" ]
    (EitherTy(EitherTy(ut "'ll", ut "'lr"), EitherTy(ut "'rl", ut "'rr")))

  compile [ PairPat(VarPat "x", VarPat "y"), None, VarExpr "useXY x y" ] (PairTy(UnknownTy "t1", UnknownTy "t2"))

  // left/pair nesting
  compile
    [ PairPat(LeftPat(VarPat "lx"), LeftPat(VarPat "ly")), None, VarExpr "useXY lx ly"
      PairPat(wp, LeftPat(VarPat "ly")), None, VarExpr "useXY null ly"
      PairPat(LeftPat(VarPat "lx"), wp), None, VarExpr "useXY lx null"
      PairPat(RightPat(wp), RightPat(wp)), None, VarExpr "useXY null null" ]
    (PairTy(EitherTy(UnknownTy "t1", UnknownTy "t2"), EitherTy(UnknownTy "t3", UnknownTy "t4")))

  eprintfn "// non-exhaustive"
  compile [ LeftPat(vp "l"), None, vx "useL l" ] (EitherTy(ut "'l", ut "'r"))

  eprintfn "// redundant"
  compile
    [ LeftPat wp, None, vx "()"
      RightPat wp, None, vx "()"
      LeftPat(vp "_l"), None, vx "unreachable" ]
    (EitherTy(ut "'l", ut "'r"))

  eprintfn "// guarded"
  compile
    [ LeftPat(LeftPat wp), Some(vx "gl1"), vx "()"
      RightPat wp, Some(vx "gr1"), vx "()"
      LeftPat(RightPat wp), Some(vx "gl2"), vx "()"
      LeftPat(wp), None, vx "non_guarded1"
      RightPat(wp), None, vx "non_guarded2" ]
    (EitherTy(EitherTy(ut "'ll", ut "'lr"), EitherTy(ut "'rl", ut "'rr")))

  0
