module rec MiloneSyntaxV2.Syntax

open MiloneSyntaxV2.Source

module M = MiloneStd.StdMap

// -----------------------------------------------
// SyntaxError
// -----------------------------------------------

[<NoEquality; NoComparison>]
type SyntaxError = SyntaxError of string * Range

// -----------------------------------------------
// Token
// -----------------------------------------------

type Token =
  | EofToken

  // Trivia
  | BadToken
  | BlankToken
  | NewLinesToken
  | CommentToken

  | IntToken
  | FloatToken
  | CharToken
  | StringToken
  | IdentToken
  | KeywordToken
  | PunToken

[<NoEquality; NoComparison>]
type TokenData = TokenData of Token * text: string * Pos

// -----------------------------------------------
// TokenizeHost
// -----------------------------------------------

/// Some external state used for tokenization.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TokenizeHost = { KeywordMap: M.TreeMap<string, unit> }

let tokenizeHostNew (): TokenizeHost =
  let keywords =
    [ "as"
      "else"
      "enum"
      "false"
      "fn"
      "if"
      "in"
      "let"
      "match"
      "mod"
      "priv"
      "pub"
      "struct"
      "true"
      "type"
      "unit"
      "use"
      "where" ]

  let keywords =
    keywords
    |> List.map (fun keyword -> keyword, ())
    |> M.ofList compare

  { KeywordMap = keywords }

// -----------------------------------------------
// TokenizeResult
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TokenizeResult =
  { Tokens: TokenData list
    Errors: SyntaxError list }

// -----------------------------------------------
// Abstract syntax tree (AST)
// -----------------------------------------------

/// Literal.
[<NoEquality; NoComparison>]
type ALit =
  | AIntLit of string
  | AFloatLit of string
  | ABoolLit of value: bool
  | ACharLit of string
  | AStringLit of string
  | AUnitLit

/// Occurrence or potential occurrence of names.
[<Struct; NoEquality; NoComparison>]
type AName =
  | AName of name: string * pos: Pos
  | AMissingName of missingId: int * missingPos: Pos

/// Static path to module. (Path without generic arguments.)
type AModulePath = AName * AName list

/// Static path to symbol, including generic arguments.
type APath = AModulePath * AGenericArgList

/// Declaration of generic parameter list.
type AGenericParamList = AName list

/// List of generic arguments.
type AGenericArgList = ATy list

/// Parameter or field declaration.
type AParam = AName option * ATy

/// List of parameter declarations.
type AParamList = AParam list

/// Function's result type ascription.
type AResult = ATy * Pos

/// Argument or field initializer.
type AArg = AName option * AExpr

/// List of arguments or field initializers.
type AArgList = AArg list

// -----------------------------------------------
// AST: Types
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AFnTy =
  { ParamList: AParamList
    Result: AResult option
    Pos: Pos }

[<NoEquality; NoComparison>]
type ATy =
  | AMissingTy of Pos
  | AInferTy of AName
  | APathTy of APath
  | AUnitTy of Pos
  | ATupleTy of ATy list * Pos
  | AFnTy of AFnTy

// -----------------------------------------------
// AST: Patterns
// -----------------------------------------------

type APatKind =
  | ATuplePk
  | AListPk of hasTail: bool
  | AMinusPk
  | AConsPk

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ACallPat =
  { Callee: APath
    ParamList: (AName option * APat) list
    Pos: Pos }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AAsPat = { Name: AName; Body: APat; Pos: Pos }

[<NoEquality; NoComparison>]
type APat =
  | ALitPat of ALit * Pos
  | AWildcardPat of AName
  | AVarPat of AName
  | APathPat of APath
  | ACallPat of ACallPat
  | ANodePat of APatKind * APat list * Pos
  | AAsPat of AAsPat
  | AOrPat of APat * APat * Pos

// -----------------------------------------------
// AST: Expressions
// -----------------------------------------------

[<NoEquality; NoComparison>]
type AExprPrefix = | AMinusEp

[<NoEquality; NoComparison>]
type AExprBinary =
  | AAddEb
  | ASubEb
  | AMulEb
  | ADivEb
  | AModuloEb
  | ABitAndEb
  | ABitOrEb
  | ABitXorEb
  | ALeftShiftEb
  | ARightShiftEb
  | AEqualEb
  | ANotEqualEb
  | ALessEb
  | ALessEqualEb
  | AGreaterEb
  | AGreaterEqualEb
  | ALogicalOrEb
  | ALogicalAndEb

[<NoEquality; NoComparison>]
type AExprKind =
  | ATupleEk
  | AListEk

  /// `..expr` in list.
  | ASpreadEk

  | AIndexEk
  | APrefixEk of AExprPrefix
  | ABinaryEk of AExprBinary
  | ARangeEk
  | AIfEk

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AClosureExpr =
  { ParamList: AParamList
    Result: AResult option
    Body: AExpr
    Range: Range }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ACallExpr =
  { Callee: AExpr
    ArgList: AArgList
    Pos: Pos }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AMatchClause =
  { Pat: APat
    Guard: AExpr option
    Body: AExpr
    Range: Range }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AMatchExpr =
  { Cond: AExpr
    Clauses: AMatchClause list
    Pos: Pos }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ABlockExpr = { Stmts: AStmt list; Range: Range }

[<NoEquality; NoComparison>]
type AExpr =
  | AMissingExpr of Pos
  | ALitExpr of ALit * Pos
  | APathExpr of APath
  | ANodeExpr of AExprKind * AExpr list * Pos
  | AClosureExpr of AClosureExpr
  | ACallExpr of ACallExpr
  | AMatchExpr of AMatchExpr
  | ABlockExpr of ABlockExpr

// -----------------------------------------------
// AST: Statements
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ALetStmt =
  { Pat: APat
    Ty: ATy option
    Init: AExpr
    Pos: Pos }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AFnStmt =
  { Name: AName
    GenericParamList: AGenericParamList
    ParamList: AParamList
    Result: AResult option
    Body: AExpr
    Range: Range }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AVariantDecl = { Name: AName; Fields: AParamList }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AEnumStmt =
  { Name: AName
    GenericParamList: AGenericParamList
    Variants: AVariantDecl list
    Range: Range }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AStructStmt =
  { Name: AName
    GenericParamList: AGenericParamList
    Fields: AParamList
    Range: Range }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ATypeStmt =
  { Name: AName
    GenericParamList: AGenericParamList
    Body: ATy option
    Range: Range }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AModStmt =
  { Name: AName
    Body: AStmt list
    Range: Range }

[<NoEquality; NoComparison>]
type AUseTree =
  | AUseName of AName * aliasOpt: AName option
  | AUsePath of AName * AUseTree * Pos
  | AUseGroup of AUseTree list * Pos

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type AUseStmt = { Body: AUseTree; Pos: Pos }

[<NoEquality; NoComparison>]
type AStmt =
  | AMissingStmt of Pos
  | AExprStmt of AExpr
  | ALetStmt of ALetStmt
  | AFnStmt of AFnStmt
  | AEnumStmt of AEnumStmt
  | AStructStmt of AStructStmt
  | ATypeStmt of ATypeStmt
  | AModStmt of AModStmt
  | AUseStmt of AUseStmt

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ARoot = { Stmts: AStmt list; Range: Range }

// -----------------------------------------------
// Namespaces
// -----------------------------------------------

type NsKind =
  | ValueNs
  | TyNs

// -----------------------------------------------
// Scopes
// -----------------------------------------------

/// Position of name.
type NamePos = Pos

/// Position of name, which is defined there.
type DefPos = Pos

/// Local scope.
///
/// ((ns, name) -> defPos) means:
///   a definition, defined at the position, is visible to the local scope
///   via the name in namespace.
type LocalScope = M.TreeMap<NsKind * string, DefPos>

/// Whether a symbol is definition or use?
[<NoEquality; NoComparison>]
type Site =
  | DefSite
  | UseSite of defSite: DefPos

/// Set of names with link to def-site.
///
/// - ((ns, pos) -> DefSite) means:
///     a definition is defined at the position (pos).
/// - ((ns, pos) -> UseSite defPos) means:
///     the name at the position (pos) occurs as a use of a definition,
///     which is defined at the position (defPos).
type Sites = M.TreeMap<NsKind * NamePos, Site>

/// Relationship of symbols in a syntax tree.
type SymbolTree = M.TreeMap<Pos, M.TreeMap<NsKind * string, Pos>>

/// Node of a scope tree.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ScopeNode =
  { Children: ScopeNode list

    /// Only in a module, the def-site of module symbol.
    ModOpt: Pos option

    LocalScope: LocalScope
    Range: Range }

/// Result of scope analysis on a source file.
///
/// This is responsible to answer queries such as:
///
/// - For "name" (syntax node with name kind) in the syntax tree:
///     whether is the name defined in the same syntax tree?
///         If defined, where it is?
///         Or, does the name itself definition-site?
///     If not defined, what is its local scope to be used for further resolution?
///         Which use statements should be resolved before that?
/// - For a position in source text:
///     what is the local scope of the position
///     to be used for completion?
/// - For entire syntax tree:
///     what symbols can be referred from another module?
///     How these symbols form namespace structure?
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ScopeAnalysis =
  { RootScope: ScopeNode

    Sites: Sites
    UnresolvedNames: (NsKind * AName * ScopeNode) list
    UseHeads: (AName * ScopeNode) list

    ToplevelSymbols: (NsKind * Pos) list
    SymbolTree: SymbolTree }
