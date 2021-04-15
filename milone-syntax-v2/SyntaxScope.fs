module rec MiloneSyntaxV2.SyntaxScope

open MiloneSyntaxV2.Source
open MiloneSyntaxV2.Syntax

module M = MiloneStd.StdMap

let private map f (x, state) = f x, state

let private ifOption folder opt state =
  Option.fold (fun state x -> folder x state) state opt

let private forList folder xs state =
  List.fold (fun state x -> folder x state) state xs

let private splitLast xs =
  let n = List.length xs

  if n = 0 then
    [], None
  else
    let xs, tail = List.splitAt (n - 1) xs
    xs, List.tryHead tail

let private mapList f xs state =
  List.fold
    (fun (acc, state) x ->
      let y, state = f x state
      (y :: acc), state)
    ([], state)
    xs
  |> map List.rev

/// Merges a list to another, unordered.
let private mergeList mapping addition xs =
  List.fold (fun xs y -> mapping y :: xs) xs addition

let private todo () = failwith "TODO"

let private ur () = failwith "NEVER: Unreachable"

module private LocalScope =
  let empty: LocalScope = M.empty compare

module private ScopeNode =
  let finish (node: ScopeNode): ScopeNode =
    { node with
        Children = List.rev node.Children }

// -----------------------------------------------
// State
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ScopeState =
  { Node: ScopeNode
    UnresolvedNameAcc: (NsKind * AName) list
    UseHeadAcc: AName list }

let private newState node: ScopeState =
  { Node = node
    UnresolvedNameAcc = []
    UseHeadAcc = [] }

// -----------------------------------------------
// Context
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private ScopeContext =
  {
    // State:
    ScopeStack: ScopeState list
    CurrentUnresolvedNameAcc: (NsKind * AName) list
    CurrentUseHeadAcc: AName list

    // Current node:
    Children: ScopeNode list
    LocalScope: LocalScope
    ModOpt: Pos option
    Range: Range

    // Result:
    Sites: Sites
    UnresolvedNames: (NsKind * AName * ScopeNode) list
    UseHeads: (AName * ScopeNode) list

    ToplevelSymbols: (NsKind * Pos) list
    SymbolTree: SymbolTree }

type private Ctx = ScopeContext

let private ctxNew (range: Range): ScopeContext =
  { ScopeStack = []
    CurrentUnresolvedNameAcc = []
    CurrentUseHeadAcc = []

    Children = []
    LocalScope = LocalScope.empty
    ModOpt = None
    Range = range

    Sites = M.empty compare
    ToplevelSymbols = []
    SymbolTree = M.empty compare
    UnresolvedNames = []
    UseHeads = [] }

let private ctxFinish (ctx: ScopeContext): ScopeAnalysis =
  assert (List.isEmpty ctx.ScopeStack)

  let state = stash ctx

  { RootScope = state.Node |> ScopeNode.finish

    Sites = ctx.Sites
    UnresolvedNames = ctx.UnresolvedNames
    UseHeads = ctx.UseHeads

    ToplevelSymbols = ctx.ToplevelSymbols
    SymbolTree = ctx.SymbolTree }

let private currentNode (ctx: Ctx): ScopeNode =
  { Children = ctx.Children
    ModOpt = ctx.ModOpt
    LocalScope = ctx.LocalScope
    Range = ctx.Range }

let private stash (ctx: Ctx): ScopeState =
  { Node = currentNode ctx
    UnresolvedNameAcc = ctx.CurrentUnresolvedNameAcc
    UseHeadAcc = ctx.CurrentUseHeadAcc }

let private enterScope (range: Range) (ctx: Ctx): Ctx =
  printfn "[TRACE] current: %A" (ctx.LocalScope |> M.toList |> List.map fst)
  printfn "[TRACE] enterScope pos=%s depth=%d" (Pos.toString range.Start) (List.length ctx.ScopeStack)

  let parent = stash ctx

  { ctx with
      ScopeStack = parent :: ctx.ScopeStack
      Children = []
      Range = range }

let private leaveScope (ctx: Ctx): Ctx =
  printfn "[TRACE] leaveScope depth=%d" (List.length ctx.ScopeStack)

  match ctx.ScopeStack with
  | parent :: stack ->
      let child = currentNode ctx |> ScopeNode.finish
      printfn "[TRACE] current values: %A" (parent.Node.LocalScope |> M.toList |> List.map fst)

      { ctx with
          ScopeStack = stack
          CurrentUnresolvedNameAcc = parent.UnresolvedNameAcc
          CurrentUseHeadAcc = parent.UseHeadAcc

          Children = child :: parent.Node.Children
          ModOpt = parent.Node.ModOpt
          LocalScope = parent.Node.LocalScope
          Range = parent.Node.Range

          UnresolvedNames = mergeList (fun (x,y) -> x, y, child) ctx.CurrentUnresolvedNameAcc ctx.UnresolvedNames
          UseHeads = mergeList (fun x -> x, child) ctx.CurrentUseHeadAcc ctx.UseHeads }

  | [] -> failwith "NEVER"

let private enterMod (name: AName) (range: Range) (ctx: Ctx): Ctx =
  let name, pos = unwrapName name
  printfn "[TRACE] enterMod %s pos=%s" name (Pos.toString range.Start)

  let ctx = enterScope range ctx

  { ctx with
      Children = []
      ModOpt = Some pos
      Range = range }

let private leaveMod (ctx: Ctx): Ctx =
  printfn "[TRACE] leaveMod from %A" ctx.ModOpt

  leaveScope ctx

let private addUnresolved (kind: NsKind) (name: AName) (ctx: Ctx): Ctx =
  printfn "[TRACE] addUnresolved %A %A" kind name

  { ctx with
      CurrentUnresolvedNameAcc =
        (kind, name)
        :: ctx.CurrentUnresolvedNameAcc }

let private addUseHead (name: AName) (ctx: Ctx): Ctx =
  printfn "[TRACE] addUseHead %A" name

  { ctx with CurrentUseHeadAcc = name :: ctx.CurrentUseHeadAcc }

let private addSite (kind: NsKind) (pos: Pos) (site: Site) (ctx: Ctx): Ctx =
  printfn "[TRACE] addSite %A %s -> %A" kind (Pos.toString pos) site

  { ctx with
      Sites = ctx.Sites |> M.add (kind, pos) site }

let private defineSymbol (kind: NsKind) (name: AName) (ctx: Ctx): Ctx =
  let name, pos = unwrapName name

  printfn "[TRACE] defineSymbol %A %s -> %s" kind name (Pos.toString pos)

  let sites = ctx.Sites |> M.add (kind, pos) DefSite

  match ctx.ModOpt with
  | None ->
      printfn "[TRACE] toplevelSymbol %A .%s -> %s" kind name (Pos.toString pos)

      { ctx with
          Sites = sites
          ToplevelSymbols = (kind, pos) :: ctx.ToplevelSymbols }

  | Some parentPos ->
      printfn "[TRACE] symbolTree %A %s.%s -> %s" kind (Pos.toString parentPos) name (Pos.toString pos)

      let entries =
        ctx.SymbolTree
        |> M.tryFind parentPos
        |> Option.defaultValue (M.empty compare)
        |> M.add (kind, name) pos

      { ctx with
          Sites = sites
          SymbolTree = ctx.SymbolTree |> M.add parentPos entries }

let private importSymbol (kind: NsKind) (name: AName) ctx: Ctx =
  let name, pos = unwrapName name

  { ctx with
      LocalScope = ctx.LocalScope |> M.add (kind, name) pos }

let private defineLocalValue (name: AName) (ctx: Ctx): Ctx =
  ctx
  |> defineSymbol ValueNs name
  |> importSymbol ValueNs name

let private defineLocalTy (name: AName) (ctx: Ctx): Ctx =
  ctx
  |> defineSymbol TyNs name
  |> importSymbol TyNs name

// -----------------------------------------------
// Misc
// -----------------------------------------------

let private evalMissing hint pos =
  failwithf "FATAL: Missing %s at %s" hint (Pos.toString pos)

let private unwrapName name: string * Pos =
  match name with
  | AName (text, pos) -> text, pos
  | AMissingName (_, pos) -> evalMissing "name" pos

let private resolveNameAs (kind: NsKind) name (ctx: Ctx): Ctx =
  match name with
  | AName (name, pos) ->
      match ctx.LocalScope |> M.tryFind (kind, name) with
      | Some defPos -> ctx |> addSite kind pos (UseSite defPos)
      | None -> ctx |> addUnresolved kind (AName(name, pos))

  | AMissingName (_, pos) -> evalMissing "name" pos

let private resolveNameOrUseHead name (ctx: Ctx): Ctx =
  match name with
  | AName (name, pos) ->
      match ctx.LocalScope |> M.tryFind (TyNs, name) with
      | Some defPos -> ctx |> addSite TyNs pos (UseSite defPos)
      | None -> ctx |> addUseHead (AName(name, pos))

  | AMissingName (_, pos) -> evalMissing "name" pos

let private resolvePathAs (kind: NsKind) (path: APath) ctx: Ctx =
  let ((head, tail), _) = path

  let kind = if List.isEmpty tail then kind else TyNs

  resolveNameAs kind head ctx

// -----------------------------------------------
// Types
// -----------------------------------------------

let private resolveTy ty ctx = todo ()
// match ty with
// | APathTy path -> ctx |> evalPath path
// | _ -> todo ()

// -----------------------------------------------
// Patterns
// -----------------------------------------------

let private resolvePat pat ctx: Ctx =
  match pat with
  | ALitPat _
  | AWildcardPat _ -> ctx

  | AVarPat name -> defineLocalValue name ctx

  | APathPat path -> resolvePathAs ValueNs path ctx

  | ACallPat pat ->
      ctx
      |> resolvePathAs ValueNs pat.Callee
      |> forList (fun (_, param) -> resolvePat param) pat.ParamList

  | ANodePat (_, args, _) -> ctx |> forList resolvePat args

  | AAsPat pat ->
      ctx
      |> resolvePat pat.Body
      |> defineLocalValue pat.Name

  | AOrPat (l, r, _) ->
      // FIXME: set of binding must match
      ctx |> resolvePat l |> resolvePat r

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private resolveClosureExpr (expr: AClosureExpr) ctx: Ctx =
  ctx
  |> enterScope expr.Range
  |> forList (fun (name, _) -> ifOption (defineLocalValue) name) expr.ParamList
  |> resolveExpr expr.Body
  |> leaveScope

let private resolveCallExpr (expr: ACallExpr) ctx: Ctx =
  ctx
  |> resolveExpr expr.Callee
  |> forList (fun ((_, arg): AArg) -> resolveExpr arg) expr.ArgList

let private resolveMatchExpr (expr: AMatchExpr) ctx: Ctx =
  let resolveClause (clause: AMatchClause) ctx =
    ctx
    |> enterScope clause.Range
    |> resolvePat clause.Pat
    |> ifOption resolveExpr clause.Guard
    |> resolveExpr clause.Body
    |> leaveScope

  ctx
  |> resolveExpr expr.Cond
  |> forList resolveClause expr.Clauses

let private resolveBlockExpr (expr: ABlockExpr) ctx: Ctx =
  ctx
  |> enterScope expr.Range
  |> forList declareStmt expr.Stmts
  |> forList resolveStmt expr.Stmts
  |> leaveScope

let private resolveExpr expr ctx: Ctx =
  match expr with
  | AMissingExpr pos -> ctx |> evalMissing "expr" pos
  | ALitExpr _ -> ctx
  | APathExpr path -> resolvePathAs ValueNs path ctx
  | ANodeExpr (_, args, _) -> ctx |> forList resolveExpr args
  | AClosureExpr expr -> resolveClosureExpr expr ctx
  | ACallExpr expr -> resolveCallExpr expr ctx
  | AMatchExpr expr -> resolveMatchExpr expr ctx
  | ABlockExpr expr -> resolveBlockExpr expr ctx

// -----------------------------------------------
// Statements (declare)
// -----------------------------------------------

let private defineSymbolsIn (kind: NsKind) (parentName: AName) (children: AName list) (ctx: Ctx): Ctx =
  let _, parentPos = unwrapName parentName

  let entries =
    ctx.SymbolTree
    |> M.tryFind parentPos
    |> Option.defaultValue (M.empty compare)
    |> forList
         (fun name ->
           let name, pos = unwrapName name
           M.add (kind, name) pos)
         children

  { ctx with
      SymbolTree = ctx.SymbolTree |> M.add parentPos entries }

let private declareFnStmt (stmt: AFnStmt) ctx: Ctx = defineLocalValue stmt.Name ctx

let private declareEnumStmt (stmt: AEnumStmt) ctx: Ctx =
  ctx
  |> defineLocalTy stmt.Name
  |> defineSymbolsIn ValueNs stmt.Name (List.map (fun (variant: AVariantDecl) -> variant.Name) stmt.Variants)

let private declareStructStmt (stmt: AStructStmt) ctx: Ctx =
  ctx
  |> defineLocalValue stmt.Name
  |> defineLocalTy stmt.Name

let private declareModStmt (stmt: AModStmt) ctx: Ctx = ctx |> defineLocalTy stmt.Name

let private declareUseStmt (stmt: AUseStmt) ctx: Ctx =
  let rec goTail parent (tree: AUseTree) ctx =
    match tree with
    | AUseName (name, aliasOpt) ->
        match aliasOpt with
        | Some alias ->
            ctx
            |> defineSymbolsIn TyNs parent [ alias ]
            |> defineSymbolsIn ValueNs parent [ alias ]

        | None ->
            ctx
            |> defineSymbolsIn TyNs parent [ name ]
            |> defineSymbolsIn ValueNs parent [ name ]

    | AUsePath (name, tail, _) ->
        ctx
        |> defineSymbolsIn TyNs parent [ name ]
        |> goTail name tail

    | AUseGroup (children, _) -> ctx |> forList (goTail parent) children

  let rec goHead (tree: AUseTree) ctx =
    match tree with
    | AUseName (name, aliasOpt) ->
        let alias = Option.defaultValue name aliasOpt

        ctx
        |> defineSymbol TyNs name
        |> importSymbol TyNs alias
        |> defineSymbol ValueNs name
        |> importSymbol ValueNs alias

    | AUsePath (name, tail, _) -> ctx |> defineSymbol TyNs name |> goTail name tail

    | AUseGroup (children, _) -> ctx |> forList goHead children

  ctx |> goHead stmt.Body

let private declareStmt stmt ctx: Ctx =
  match stmt with
  | AMissingStmt _
  | AExprStmt _
  | ALetStmt _ -> ctx

  | AFnStmt stmt -> declareFnStmt stmt ctx

  | AEnumStmt stmt -> declareEnumStmt stmt ctx
  | AStructStmt stmt -> declareStructStmt stmt ctx
  | ATypeStmt stmt -> defineLocalTy stmt.Name ctx

  | AModStmt stmt -> declareModStmt stmt ctx
  | AUseStmt stmt -> declareUseStmt stmt ctx

// -----------------------------------------------
// Statements (resolve)
// -----------------------------------------------

let private resolveParam (param: AParam) ctx =
  match param with
  | Some name, _ -> ctx |> defineLocalValue name
  | None, _ -> ctx

let private resolveLetStmt (stmt: ALetStmt) ctx: Ctx =
  ctx
  |> resolveExpr stmt.Init
  |> resolvePat stmt.Pat

let private resolveFnStmt (stmt: AFnStmt) (ctx: Ctx): Ctx =
  ctx
  |> enterScope stmt.Range
  |> forList resolveParam stmt.ParamList
  |> resolveExpr stmt.Body
  |> leaveScope

let private resolveModStmt (stmt: AModStmt) (ctx: Ctx): Ctx =
  ctx
  |> enterMod stmt.Name stmt.Range
  |> forList declareStmt stmt.Body
  |> forList resolveStmt stmt.Body
  |> leaveMod

let private resolveUseStmt (stmt: AUseStmt) ctx: Ctx =
  let rec goHead (tree: AUseTree) ctx =
    match tree with
    | AUseName (head, _) -> ctx |> resolveNameOrUseHead head
    | AUsePath (head, _, _) -> ctx |> resolveNameOrUseHead head

    | AUseGroup (children, _) -> ctx |> forList goHead children

  goHead stmt.Body ctx

let private resolveStmt stmt ctx: Ctx =
  match stmt with
  | AMissingStmt pos -> evalMissing "stmt" pos ctx
  | AExprStmt expr -> resolveExpr expr ctx
  | ALetStmt stmt -> resolveLetStmt stmt ctx
  | AFnStmt stmt -> resolveFnStmt stmt ctx

  | AEnumStmt _
  | AStructStmt _
  | ATypeStmt _ ->
      // FIXME: resolve body types
      ctx

  | AModStmt stmt -> resolveModStmt stmt ctx
  | AUseStmt _ -> ctx

// -----------------------------------------------
// Interface
// -----------------------------------------------

let resolveScopes (root: ARoot): ScopeAnalysis =
  let ctx = ctxNew root.Range

  ctx
  |> forList declareStmt root.Stmts
  |> forList resolveStmt root.Stmts
  |> ctxFinish
