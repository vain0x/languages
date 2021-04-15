module rec MiloneSyntaxV2.Eval

open MiloneSyntaxV2.EscapeSequence
open MiloneSyntaxV2.Source
open MiloneSyntaxV2.Syntax

module M = MiloneStd.StdMap

let private map f (x, state) = f x, state

let private ifOption folder opt state =
  Option.fold (fun state x -> folder x state) state opt

let private forList folder xs state =
  List.fold (fun state x -> folder x state) state xs

let private mapList f xs state =
  List.fold
    (fun (acc, state) x ->
      let y, state = f x state
      (y :: acc), state)
    ([], state)
    xs
  |> map List.rev

let private todo () = failwith "TODO"

let private ur () = failwith "NEVER: Unreachable"

// -----------------------------------------------
// Value
// -----------------------------------------------

type private Value =
  | IntValue of int
  | FloatValue of float
  | BoolValue of bool
  | CharValue of char
  | StringValue of string
  | TupleValue of Value list
  | ListValue of Value list
  | FunValue of (Value list -> Value)

let private unitValue: Value = TupleValue []

let private asBool value =
  match value with
  | BoolValue value -> value
  | _ -> failwithf "Type error: expected bool %A" value

// -----------------------------------------------
// Context
// -----------------------------------------------

type private Env = M.TreeMap<string, Value> list

[<NoEquality; NoComparison>]
type private EvalCtx =
  { Env: Env
    Global: M.TreeMap<string, Value> list
    Current: string
    Imports: string list }

let private noEnv: Env = [ M.empty compare ]

let private evalCtxNew (): EvalCtx =
  { Env = noEnv
    Global = [ M.empty compare ]
    Current = "::"
    Imports = [ "::" ] }

// -----------------------------------------------
// Misc
// -----------------------------------------------

let private evalMissing hint pos =
  failwithf "FATAL: Missing %s at %s" hint (Pos.toString pos)

let private unwrapName name: string =
  match name with
  | AName (text, _) -> text
  | AMissingName (_, pos) -> evalMissing "name" pos

let private evalName name (ctx: EvalCtx): Value =
  match name with
  | AName (name, pos) ->
      ctx.Env
      |> List.tryPick (fun map -> map |> M.tryFind name)
      |> Option.defaultWith (fun () -> failwithf "Undefined name : %s at %s" name (Pos.toString pos))

  | AMissingName (_, pos) -> evalMissing "name" pos

let private evalLit lit: Value =
  match lit with
  | AIntLit text -> System.Int32.Parse(text) |> IntValue
  | AFloatLit text -> System.Double.Parse(text) |> FloatValue

  | ABoolLit false -> BoolValue false
  | ABoolLit true -> BoolValue true

  | ACharLit text ->
      let c =
        match text with
        | "\\x00" -> '\x00'
        | "\\n" -> '\n'
        | _ -> text.[0]

      CharValue c

  | AStringLit text ->
      StringValue(
        text
          .Substring(1, text.Length - 2)
          .Replace("\\n", "\n")
      )

  | AUnitLit -> unitValue

let private evalPath (path: APath) ctx: Value =
  let ((head, tail), _) = path

  if List.isEmpty tail |> not then todo ()

  evalName head ctx

// ctx
// |> evalName name
// |> forList (fun name ctx -> ctx |> cons "." |> evalName name) tail

// -----------------------------------------------
// Types
// -----------------------------------------------

let private evalTy ty ctx = todo ()
// match ty with
// | APathTy path -> ctx |> evalPath path
// | _ -> todo ()

// -----------------------------------------------
// Patterns
// -----------------------------------------------

let private newVar (name: string) value (ctx: EvalCtx): EvalCtx =
  match ctx.Env with
  | [] -> ur ()

  | map :: env ->
      { ctx with
          Env = (M.add name value map) :: env }

let private evalPat pat cond ctx: EvalCtx option =
  match pat with
  | ALitPat (lit, _) ->
      if doEvalCompareFun (evalLit lit) cond = 0 then
        Some ctx
      else
        None

  | AWildcardPat _ -> Some ctx

  | AVarPat name -> ctx |> newVar (unwrapName name) cond |> Some

  | APathPat path ->
      if doEvalCompareFun (evalPath path ctx) cond = 0 then
        Some ctx
      else
        None

  | _ -> todo ()

// | ACallPat of ACallPat
// | ANodePat of APatKind * APat list * Pos
// | AAsPat of AAsPat
// | AOrPat of APat * APat * Pos

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private evalIndex l r ctx =
  let l, ctx = ctx |> evalExpr l
  let r, ctx = ctx |> evalExpr r

  match l, r with
  | StringValue l, IntValue r -> CharValue l.[r], ctx
  | _ -> failwithf "Type error: index l=%A r=%A" l r

let private evalMinusEp arg ctx =
  let arg, ctx = ctx |> evalExpr arg

  match arg with
  | IntValue arg -> IntValue(-arg), ctx
  | FloatValue arg -> FloatValue(-arg), ctx
  | _ -> invalidOp "type error"

// -----------------------------------------------
// Binary expressions
// -----------------------------------------------

let private evalAddEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l + r), ctx
  | FloatValue l, FloatValue r -> FloatValue(l + r), ctx
  | CharValue l, CharValue r -> CharValue(l + r), ctx
  | StringValue l, StringValue r -> StringValue(l + r), ctx
  | _ -> todo ()

let private evalSubEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l - r), ctx
  | FloatValue l, FloatValue r -> FloatValue(l - r), ctx
  | _ -> todo ()

let private evalMulEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l * r), ctx
  | FloatValue l, FloatValue r -> FloatValue(l * r), ctx
  | _ -> todo ()

let private evalDivEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l / r), ctx
  | FloatValue l, FloatValue r -> FloatValue(l / r), ctx
  | _ -> todo ()

let private evalModuloEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l % r), ctx
  | FloatValue l, FloatValue r -> FloatValue(l % r), ctx
  | _ -> todo ()

let private evalBitAndEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l &&& r), ctx
  | _ -> todo ()

let private evalBitOrEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l ||| r), ctx
  | _ -> todo ()

let private evalBitXorEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l ^^^ r), ctx
  | _ -> todo ()

let private evalLeftShiftEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l <<< r), ctx
  | _ -> todo ()

let private evalRightShiftEb l r ctx =
  match l, r with
  | IntValue l, IntValue r -> IntValue(l >>> r), ctx
  | _ -> todo ()

let private doEvalCompareFun l r =
  match l, r with
  | IntValue l, IntValue r -> compare l r
  | FloatValue l, FloatValue r -> compare l r
  | CharValue l, CharValue r -> compare l r
  | StringValue l, StringValue r -> compare l r
  | _ -> todo ()

let private doEvalComparisonBinary k l r ctx =
  (BoolValue(k (doEvalCompareFun l r)), ctx)

let private evalEqualEb l r ctx = doEvalComparisonBinary ((=) 0) l r ctx
let private evalNotEqualEb l r ctx = doEvalComparisonBinary ((<>) 0) l r ctx
let private evalLessEb l r ctx = doEvalComparisonBinary ((>) 0) l r ctx
let private evalLessEqualEb l r ctx = doEvalComparisonBinary ((>=) 0) l r ctx
let private evalGreaterEb l r ctx = doEvalComparisonBinary ((<) 0) l r ctx
let private evalGreaterEqualEb l r ctx = doEvalComparisonBinary ((<=) 0) l r ctx

let private evalBinary binary l r ctx: Value * EvalCtx =
  let onStrict f ctx =
    let l, ctx = ctx |> evalExpr l
    let r, ctx = ctx |> evalExpr r
    f l r ctx

  match binary with
  | AAddEb -> onStrict evalAddEb ctx
  | ASubEb -> onStrict evalSubEb ctx
  | AMulEb -> onStrict evalMulEb ctx
  | ADivEb -> onStrict evalDivEb ctx
  | AModuloEb -> onStrict evalModuloEb ctx
  | ABitAndEb -> onStrict evalBitAndEb ctx
  | ABitOrEb -> onStrict evalBitOrEb ctx
  | ABitXorEb -> onStrict evalBitXorEb ctx
  | ALeftShiftEb -> onStrict evalLeftShiftEb ctx
  | ARightShiftEb -> onStrict evalRightShiftEb ctx
  | AEqualEb -> onStrict evalEqualEb ctx
  | ANotEqualEb -> onStrict evalNotEqualEb ctx
  | ALessEb -> onStrict evalLessEb ctx
  | ALessEqualEb -> onStrict evalLessEqualEb ctx
  | AGreaterEb -> onStrict evalGreaterEb ctx
  | AGreaterEqualEb -> onStrict evalGreaterEqualEb ctx

  | ALogicalOrEb ->
      let l, ctx = ctx |> evalExpr l

      if asBool l then
        l, ctx
      else
        evalExpr r ctx

  | ALogicalAndEb ->
      let l, ctx = ctx |> evalExpr l

      if asBool l then
        evalExpr r ctx
      else
        l, ctx

// -----------------------------------------------
// Other expressions
// -----------------------------------------------

let private evalIfUnit cond body ctx =
  let cond, ctx = evalExpr cond ctx |> map asBool

  if cond then
    evalExpr body ctx
  else
    unitValue, ctx

let private evalIfElse cond body alt ctx =
  let cond, ctx = evalExpr cond ctx |> map asBool

  evalExpr (if cond then body else alt) ctx

let private evalNodeExpr kind args ctx: Value * EvalCtx =
  match kind, args with
  | ATupleEk, _ -> mapList evalExpr args ctx |> map TupleValue

  | AListEk, _ ->
      // TODO: handle spread
      mapList evalExpr args ctx |> map ListValue

  | ASpreadEk, _ -> invalidOp "Spread outside of call/list"

  | AIndexEk, [ l; r ] -> evalIndex l r ctx
  | APrefixEk AMinusEp, [ arg ] -> evalMinusEp arg ctx
  | ABinaryEk binary, [ l; r ] -> evalBinary binary l r ctx

  | ARangeEk, _ -> todo ()

  | AIfEk, [ cond; body ] -> evalIfUnit cond body ctx
  | AIfEk, [ cond; body; alt ] -> evalIfElse cond body alt ctx

  | AIndexEk, _
  | APrefixEk _, _
  | ABinaryEk _, _
  | AIfEk, _ -> ur ()

let private evalCallExpr (expr: ACallExpr) ctx =
  let callee, ctx = evalExpr expr.Callee ctx

  let args, ctx =
    mapList
      evalExpr
      (expr.ArgList
       |> List.map (fun ((_, arg): AArg) -> arg))
      ctx

  match callee with
  | FunValue f -> f args, ctx

  | _ -> failwithf "call: %A %A" callee args

let private splitLast xs =
  let n = List.length xs

  if n = 0 then
    [], None
  else
    let xs, tail = List.splitAt (n - 1) xs
    xs, List.tryHead tail

let private evalBlockExpr (block: ABlockExpr) ctx =
  let stmts, last = splitLast block.Stmts

  let ctx = ctx |> forList evalStmt stmts

  match last with
  | Some (AExprStmt last) -> evalExpr last ctx
  | Some last -> unitValue, evalStmt last ctx
  | None -> unitValue, ctx

let private evalExpr expr ctx: Value * EvalCtx =
  match expr with
  | AMissingExpr pos -> ctx |> evalMissing "expr" pos

  | ALitExpr (lit, _) -> evalLit lit, ctx
  | APathExpr path -> evalPath path ctx, ctx

  | ANodeExpr (kind, args, _) -> evalNodeExpr kind args ctx
  | AClosureExpr _ -> todo ()
  | ACallExpr expr -> evalCallExpr expr ctx
  | AMatchExpr _ -> todo ()
  | ABlockExpr block -> evalBlockExpr block ctx

// -----------------------------------------------
// Statements
// -----------------------------------------------

let private evalParam param ctx =
  match param with
  | Some name, None -> ctx |> evalName name

  | _ -> todo ()

let private evalLetStmt (stmt: ALetStmt) ctx =
  let init, ctx = evalExpr stmt.Init ctx

  match evalPat stmt.Pat init ctx with
  | Some ctx -> ctx
  | None -> failwithf "refuted %A" stmt

let private evalFnStmt (stmt: AFnStmt) (ctx: EvalCtx) =
  let mutable ctx = ctx

  let evalFn (args: Value list) =
    if List.length stmt.ParamList <> List.length args then
      failwithf
        "arity mismatch: params=%d args=%d %A at %s"
        (List.length stmt.ParamList)
        (List.length args)
        args
        (Pos.toString stmt.Range.Start)

    ctx
    |> forList
         (fun ((name, _), arg) ctx -> newVar (unwrapName (Option.get name)) arg ctx)
         (List.zip stmt.ParamList args)
    |> evalExpr stmt.Body
    |> fst

  ctx <-
    ctx
    |> newVar (unwrapName stmt.Name) (FunValue evalFn)

  ctx

let private evalStmt stmt ctx: EvalCtx =
  match stmt with
  | AMissingStmt pos -> ctx |> evalMissing "stmt" pos
  | AExprStmt expr -> ctx |> evalExpr expr |> snd
  | ALetStmt stmt -> evalLetStmt stmt ctx
  | AFnStmt stmt -> evalFnStmt stmt ctx

  | AEnumStmt _
  | AStructStmt _
  | ATypeStmt _ -> ctx

  | AModStmt stmt -> todo ()
  | AUseStmt stmt -> todo ()

// -----------------------------------------------
// Interface
// -----------------------------------------------

let eval (projectName: string) (moduleName: string) (root: ARoot): string =
  let stmts = root.Stmts

  let ctx =
    { (evalCtxNew ()) with
        Current = sprintf "::%s::%s::" projectName moduleName }
    |> newVar
         "puts"
         (FunValue
           (fun value ->
             printfn "puts: %A" value
             unitValue))
    |> newVar
         "assert"
         (FunValue
           (fun value ->
             assert (List.head value |> asBool)
             unitValue))

  ctx |> forList evalStmt stmts |> ignore
  ""
