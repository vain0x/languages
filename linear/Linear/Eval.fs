module Linear.Eval

open Linear.Location
open Linear.TypedIR

let private unwrap opt = Option.get opt

let private idOf name =
  let (TName (_, id, _)) = name
  id

exception EvalError of Message: string * Range: Range with
  override this.ToString() =
    $"Runtime error: {this.Message} at {this.Range}"

let private fail message range : 'A = raise (EvalError(message, range))

[<RequireQualifiedAccess>]
type EValue =
  | Int of int
  | Bool of bool
  | Unit
  | Fun of Id * EValue list
  | Pair of EValue * EValue
  | NewtypeVariant of Id * EValue
  | Linear of EValue * owned: bool ref

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private EvalState =
  { Env: Map<Id, EValue>
    Resources: (bool ref * Range) list }

let private emptyState () : EvalState = { Env = Map.empty; Resources = [] }

type private Rx = TModule
type private Sx = EvalState

let private noRange = Range.ofPos Pos.zero

let rec private evalPat cond (state: Sx) (pat: TPat) : Sx =
  match pat with
  | TVarPat (name, _) -> { state with Env = state.Env |> Map.add (idOf name) cond }

  | TWildcardPat _
  | TUnitPat _ -> state

  | TPairPat (lPat, rPat) ->
    match cond with
    | EValue.Pair (l, r) ->
      let state = evalPat l state lPat
      let state = evalPat r state rPat
      state

    | _ -> fail "Expected pair" noRange

  | TWrapPat (_, payloadPat) ->
    let payload =
      match cond with
      | EValue.NewtypeVariant (_, payload) -> payload
      | _ -> fail "Expected variant" noRange

    evalPat payload state payloadPat

let rec private evalExpr (rx: Rx) (state: Sx) (expr: TExpr) : EValue * Sx =
  match expr with
  | TIntExpr (value, _) -> EValue.Int value, state
  | TBoolExpr (value, _) -> EValue.Bool value, state
  | TUnitExpr _ -> EValue.Unit, state

  | TSymbolExpr (kind, id, _, pos) ->
    match kind with
    | TSymbolKind.Var ->
      match state.Env |> Map.tryFind id with
      | Some v -> v, state
      | _ -> fail "Undefined variable" (Range.ofPos pos)

    | TSymbolKind.Fun ->
      if rx.Funs |> Map.containsKey id then
        EValue.Fun(id, []), state
      else
        fail "Undefined function" (Range.ofPos pos)

    | _ -> fail "Variant can't appear as value" (Range.ofPos pos)

  | TAppExpr (TSymbolExpr (TSymbolKind.Variant, id, _, pos), payloadExpr) ->
    let payload, state = evalExpr rx state payloadExpr
    EValue.NewtypeVariant(id, payload), state

  | TAppExpr (lExpr, rExpr) ->
    let lValue, state = evalExpr rx state lExpr
    let rValue, state = evalExpr rx state rExpr

    match lValue with
    | EValue.Fun (funId, argAcc) ->
      let argAcc = rValue :: argAcc

      let f = rx.Funs |> Map.tryFind funId |> unwrap

      if List.length argAcc = List.length f.ParamList then
        let args = List.rev argAcc
        let parentEnv = state.Env

        let result, state =
          let env =
            List.zip args f.ParamList
            |> List.fold (fun env (arg, (name, _)) -> env |> Map.add (idOf name) arg) state.Env

          let state = { state with Env = env }
          evalExpr rx state f.Body

        result, { state with Env = parentEnv }
      else
        EValue.Fun(funId, argAcc), state

    | _ -> fail "Expected function" noRange

  | TUnaryExpr (unary, argExpr, pos) ->
    let argExpr, state = evalExpr rx state argExpr

    match unary with
    | TUnary.Assert ->
      match argExpr with
      | EValue.Bool true -> EValue.Unit, state
      | _ ->
        eprintfn "env = %A" state.Env
        fail "Assertion violation" (Range.ofPos pos)

    | TUnary.UUAcquire ->
      let owned = ref true
      EValue.Linear(argExpr, owned), { state with Resources = (owned, Range.ofPos pos) :: state.Resources }

    | TUnary.UUDispose ->
      match argExpr with
      | EValue.Linear (item, owned) ->
        if not owned.contents then
          fail "Double free" (Range.ofPos pos)

        owned.contents <- false
        item, state

      | _ -> fail "Expected linear type" (Range.ofPos pos)

  | TBinaryExpr (binary, lExpr, rExpr) ->
    let l, state = evalExpr rx state lExpr
    let r, state = evalExpr rx state rExpr

    match binary, l, r with
    | TBinary.Add, EValue.Int l, EValue.Int r -> EValue.Int(l + r), state
    | TBinary.Add, _, _ -> fail "Can't add" noRange

    | TBinary.Equal, EValue.Int l, EValue.Int r -> EValue.Bool(l = r), state
    | TBinary.Equal, _, _ -> fail "Can't equal" noRange

    | TBinary.Pair, _, _ -> EValue.Pair(l, r), state

  | TIfExpr (cond, thenClause, elseClause) ->
    let cond, state = evalExpr rx state cond

    let clause =
      match cond with
      | EValue.Bool true -> thenClause
      | _ -> elseClause

    evalExpr rx state clause

  | TLetExpr (pat, init) ->
    let init, state = evalExpr rx state init
    EValue.Unit, evalPat init state pat

  | TBlockExpr (exprs, last) ->
    let state =
      exprs
      |> List.fold (fun state expr -> evalExpr rx state expr |> snd) state

    evalExpr rx state last

let private evalBlock (rx: Rx) (state: Sx) (expr: TExpr) : EValue * Sx =
  let parent = state
  let result, state = evalExpr rx state expr

  state.Resources
  |> List.iter (fun (owned, range) ->
    if owned.contents then
      fail "Leaked resource" range)

  result, parent

let eval (m: TModule) : unit =
  let rx: Rx = m

  m.Decls
  |> List.fold
       (fun state decl ->
         match decl with
         | TExpectDecl (desc, body, range) ->
           printfn "trace: expect %A %A" desc range

           let _, state =
             try
               evalBlock rx state body
             with
             | _ ->
               eprintfn "Failed in %s %A" desc range
               reraise ()

           state

         | TExpectErrorDecl (desc, body, range) ->
           printfn "trace: expect_error %A %A" desc range

           let ok =
             try
               let _ = evalBlock rx state body
               false
             with
             | EvalError (message, r) ->
               printfn "trace: expected_error %s at %A is resolved at runtime:\n  %s at %A" desc range message r

               // Expected.
               true

           if not ok then
             fail (sprintf "Unexpectedly success %s" desc) range

           state)
       (emptyState ())
  |> ignore
