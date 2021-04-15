// See: [Semantic Domain\: Pattern compilation made easy](https://semantic-domain.blogspot.com/2012/08/pattern-compilation-made-easy.html)
// [Semantic Domain\: An ML implementation of match compilation](https://semantic-domain.blogspot.com/2012/08/an-ml-implementation-of-match.html)
module rec MilonePatternCompile.Compile

// match は1つの値に対してパターンマッチを行う。
// match cond: ty with (| pat -> body)+

// 内部的に機能を拡張して、複数の値に対してパターンマッチを行うことにする。処理中、入力とパターンの個数 N は常に一致する (不変条件)。
// match cond1: ty1, cond2: ty2, ..., condN: tyN with
// (| pat1, pat2, ..., patN -> body)+

// ガードがあるとダメっぽい

type Var = string

type Ty =
  | UnknownTy of name: string
  | NeverTy
  | UnitTy
  | EitherTy of Ty * Ty
  | PairTy of Ty * Ty

type Pat =
  | UnitPat
  | WildcardPat
  | VarPat of Var
  | LeftPat of Pat
  | RightPat of Pat
  | PairPat of Pat * Pat

type Expr =
  | AbortExpr of Expr
  | VarExpr of Var
  | LetExpr of Var * init: Expr * next: Expr

  /// `do init; next` or `let () = init in next`
  | DoExpr of init: Expr * next: Expr

  /// `let x, y = pair in next`
  | LetPairExpr of Var * Var * init: Expr * next: Expr

  /// ```fsharp
  /// match either with
  /// | Left x -> f x
  /// | Right y -> g y
  /// ```
  | EitherExpr of cond: Expr * leftArm: (Var * Expr) * rightArm: (Var * Expr)

  | IfExpr of cond: Expr * thenCl: Expr * elseCl: Expr

let private freshVar: unit -> Var =
  let mutable lastId = 0
  fun () ->
    lastId <- lastId + 1
    "v" + string lastId

type private ClauseId = int

/// (pats, guardOpt, body). Internal representation of clause.
type private InternalClause = ClauseId * Pat list * Expr option * Expr

/// Clause of match expression.
type Clause = Pat * Expr option * Expr

// -1 pat
let private simplifyUnknown cond (clauses: InternalClause list): InternalClause list =
  clauses
  |> List.map (fun (c, pats, guardOpt, arm) ->
       match pats with
       | WildcardPat :: pats -> c, pats, guardOpt, arm
       | VarPat v :: pats -> c, pats, guardOpt, LetExpr(v, VarExpr cond, arm)
       | _ -> failwithf "NEVER: type error: %A." pats)

// -1 pat
let private simplifyUnit cond (clauses: InternalClause list): InternalClause list =
  clauses
  |> List.map (fun (c, pats, guardOpt, arm) ->
       match pats with
       | UnitPat :: pats
       | WildcardPat :: pats -> c, pats, guardOpt, arm
       | VarPat v :: pats -> c, pats, guardOpt, LetExpr(v, VarExpr cond, arm)
       | _ -> failwith "NEVER: type error")

// +- 0 pats
let private simplifyLeft cond (clauses: InternalClause list): InternalClause list =
  clauses
  |> List.choose (fun (c, pats, guardOpt, arm) ->
       match pats with
       | WildcardPat :: _ -> Some(c, pats, guardOpt, arm)
       | VarPat v :: pats -> Some(c, WildcardPat :: pats, guardOpt, LetExpr(v, VarExpr cond, arm))
       | LeftPat itemPat :: pats -> Some(c, itemPat :: pats, guardOpt, arm)
       | RightPat _ :: _ -> None
       | _ -> failwith "NEVER: type error")

// +- 0 pats
let private simplifyRight cond (clauses: InternalClause list): InternalClause list =
  clauses
  |> List.choose (fun (c, pats, guardOpt, arm) ->
       match pats with
       | WildcardPat :: _ -> Some(c, pats, guardOpt, arm)
       | VarPat v :: pats -> Some(c, WildcardPat :: pats, guardOpt, LetExpr(v, VarExpr cond, arm))
       | LeftPat _ :: _ -> None
       | RightPat itemPat :: pats -> Some(c, itemPat :: pats, guardOpt, arm)
       | _ -> failwith "NEVER: type error")

// +1 pats
let private simplifyPair cond (clauses: InternalClause list): InternalClause list =
  clauses
  |> List.map (fun (c, pats, guardOpt, arm) ->
       match pats with
       | WildcardPat :: _ -> c, WildcardPat :: pats, guardOpt, arm
       | VarPat v :: pats -> c, WildcardPat :: WildcardPat :: pats, guardOpt, LetExpr(v, VarExpr cond, arm)
       | PairPat (p1, p2) :: pats -> c, p1 :: p2 :: pats, guardOpt, arm
       | _ -> failwithf "NEVER: type error. %A" pats)

let private usedPats = ResizeArray()
let private isExhaustive = ref true

let private doCover (clauses: InternalClause list) (conds: (Var * Ty) list) =
  match conds with
  | [] ->
      let rec go acc (hole: Expr -> Expr) (clauses: InternalClause list): bool * ClauseId list * Expr =
        match clauses with
        | [] -> false, acc, hole (AbortExpr(VarExpr "not_covering"))

        | (c, pats, None, body) :: arms ->
            // len tys = len pats.
            assert (List.isEmpty pats)

            true, c :: acc, hole body

        | (c, pats, Some guard, body) :: arms ->
            assert (List.isEmpty pats)

            arms
            |> go (c :: acc) (fun rest -> IfExpr(guard, body, rest))

      let exhaust, usedIds, body = go [] id clauses

      if not exhaust then isExhaustive := false

      usedPats.AddRange(usedIds |> List.rev)

      body

  | (cond, UnknownTy _) :: conds -> doCover (simplifyUnknown cond clauses) conds

  | (cond, NeverTy) :: _ -> AbortExpr(VarExpr cond)

  | (cond, UnitTy) :: conds ->
      let e =
        doCover (simplifyUnit cond clauses) conds

      DoExpr(VarExpr cond, e)

  | (cond, EitherTy (t1, t2)) :: conds ->
      let u1 = freshVar ()
      let u2 = freshVar ()

      let e1 =
        doCover (simplifyLeft cond clauses) ((u1, t1) :: conds)

      let e2 =
        doCover (simplifyRight cond clauses) ((u2, t2) :: conds)

      EitherExpr(VarExpr cond, (u1, e1), (u2, e2))

  | (cond, PairTy (t1, t2)) :: conds ->
      let u1 = freshVar ()
      let u2 = freshVar ()
      let arms = simplifyPair cond clauses

      let e =
        doCover arms ((u1, t1) :: (u2, t2) :: conds)

      LetPairExpr(u1, u2, VarExpr cond, e)

/// (Compiled match expression, isExhaustive, redundant arm indices).
///
/// Compiles a match expression.
let cover (clauses: Clause list) (cond: Var * Ty): Expr * bool * int list =
  // Convert clauses to internal representation.
  let internalClauses =
    clauses
    |> List.mapi (fun i (pat, guardOpt, body) -> i, [ pat ], guardOpt, body)

  // Do compilation.
  let expr = doCover internalClauses [ cond ]

  // Clean up global status.
  let exhaust = !isExhaustive
  isExhaustive := true

  let redundant = List.ofSeq usedPats
  usedPats.Clear()

  expr, exhaust, redundant
