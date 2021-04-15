module rec MiloneSyntaxV2.SemaNameResolve

open MiloneSyntaxV2.Source
open MiloneSyntaxV2.Syntax
open MiloneSyntaxV2.SyntaxScope

module M = MiloneStd.StdMap

type ProjectId = int
type ModuleId = int

type ProjectContext =
  { Name: string
    ProjectEnv: M.TreeMap<string, ProjectId * ProjectContext>
    ModuleEnv: M.TreeMap<string, ModuleId * ModuleContext>
    Modules: M.TreeMap<ModuleId, ModuleContext> }

type ModuleContext =
  { Name: string
    Project: ProjectContext
    Tokens: TokenData list
    Root: ARoot
    Scopes: ScopeAnalysis }

type NameResolution =
  { Binding: M.TreeMap<ModuleId * Pos, ModuleId * NsKind * Pos>
    UnresolvedUseHeads: (ModuleId * AName * Pos) list }

let private newCtx (): NameResolution =
  { Binding = M.empty compare
    UnresolvedUseHeads = [] }

let private forList folder xs state =
  List.fold (fun state x -> folder x state) state xs

let makeLookup (tokens: TokenData list): (Pos -> string) =
  let nameMap =
    tokens
    |> List.fold
         (fun map token ->
           match token with
           | TokenData (IdentToken, text, pos) -> map |> Map.add pos text
           | _ -> map)
         Map.empty

  fun pos ->
    nameMap
    |> Map.tryFind pos
    |> Option.defaultValue ""

let private resolveNamesInModule
  (res: NameResolution)
  (moduleId: ModuleId)
  (moduleContext: ModuleContext)
  : NameResolution =

  res

let resolveNames (project: ProjectContext): NameResolution =
  let ctx =
    project.Modules
    |> M.fold resolveNamesInModule (newCtx ())

  ctx
