module rec TypeCheck.TypeChecker

open System.Collections.Generic
open TypeCheck.Helpers
open TypeCheck.Syntax

exception TypeCheckException of msg: string

let err (msg: string) = raise (TypeCheckException msg)

/// レベル
///
/// let の右辺の何段階目にいるかを表す。
type Level = int

// -----------------------------------------------
// Variable
// -----------------------------------------------

let lastVarId: int ref = ref 0

let freshVarId () : string =
  lastVarId.Value <- lastVarId.Value + 1
  let id = lastVarId.Value
  sprintf "%02X" id

/// 型検査中の型変数 (可変)
///
/// 不変条件: 境界 (bounds) に出現する型変数は、これと同じか小さいレベルを持つ (外側の型変数を参照しない)
[<ReferenceEquality>]
type Variable =
  {
    Id: string
    TyVar: TyVar

    /// この型変数を含んでいるレベル
    Level: Level

    // mutable Current: SimpleType option
    mutable Bounds: ResizeArray<SimpleType>
    mutable Recursive: bool
  }

  override this.ToString() : string =
    sprintf "%O@%d" this.TyVar this.Level

let freshVar (level: Level) : Variable =
  let hint = "a"
  let id = freshVarId ()
  let tyVar = TyVar(hint, id)

  { Id = id
    TyVar = tyVar
    Level = level
    // Current = None
    Bounds = ResizeArray()
    Recursive = false }

// -----------------------------------------------
// SimpleType
// -----------------------------------------------

/// 型検査中の型の表現
type SimpleType =
  | FunST of SimpleType * SimpleType

  | RecordST of fields: (string * SimpleType) list

  | PrimitiveST of string

  | VarST of Variable

  override this.ToString() =
    let rec go (ty: SimpleType) =
      let paren ty =
        let isPrimary =
          match ty with
          | RecordST _
          | PrimitiveST _
          | VarST _ -> true
          | _ -> false

        if isPrimary then go ty else $"({go ty})"

      match ty with
      | FunST(l, r) -> sprintf "%s -> %s" (paren l) (paren r)
      | RecordST fields ->
        sprintf
          "{ %s }"
          (fields
           |> Seq.map (fun (name, ty) -> sprintf "%s = %s" name (go ty))
           |> String.concat "; ")
      | PrimitiveST name -> name
      | VarST v -> v.ToString()

    go this

let private levelCache = Dictionary()

/// 型のレベルを計算する (内部にある型変数の最大のレベル)
let getLevel (ty: SimpleType) : int =
  match levelCache.TryGetValue(ty) with
  | true, level -> level
  | _ ->
    let m =
      match ty with
      | FunST(lhs, rhs) -> max (getLevel lhs) (getLevel rhs)

      | RecordST fields -> fields |> List.fold (fun m (_, fieldTy) -> max m (getLevel fieldTy)) 0

      | PrimitiveST _ -> 0
      | VarST v -> v.Level

    levelCache.Add(ty, m)
    eprintfn "trace: level ty=%O -> %d" ty m
    m

// -----------------------------------------------
// TypeScheme
// -----------------------------------------------

/// 型スキーム
///
/// 全称量化された型変数を含むことがある
type TypeScheme =
  | MonoTS of SimpleType

  /// 全称量化された型変数を含む
  ///
  /// レベルが limit 以上の型変数は量化されているとみなす
  | PolyTS of limit: Level * SimpleType

let instantiate (level: Level) (tyScheme: TypeScheme) : SimpleType =
  match tyScheme with
  | MonoTS ty -> ty
  | PolyTS(limit, ty) -> freshenAbove limit ty level

// -----------------------------------------------
// 組み込みの型とシンボル
// -----------------------------------------------

let ErrorST = PrimitiveST "error"

let BoolST = PrimitiveST "bool"

let IntST = PrimitiveST "int"

/// 組み込みのシンボルからなる環境
let builtInCtx () : Ctx =
  let builtins =
    [
      // true: bool
      ("true", MonoTS BoolST)

      // false: bool
      ("false", MonoTS BoolST)

      // not: bool -> bool
      ("not", MonoTS(FunST(BoolST, BoolST)))

      // succ: int -> int
      ("succ", MonoTS(FunST(IntST, IntST)))

      // add: int -> int -> int
      ("add", MonoTS(FunST(IntST, FunST(IntST, IntST))))

      // if: λT. bool -> T -> T -> T
      ("if",
       let v = VarST(freshVar 1)
       PolyTS(0, FunST(BoolST, FunST(v, FunST(v, v))))) ]

  Map.ofSeq builtins

// -----------------------------------------------
// freshenAbove
// -----------------------------------------------

/// 型変数をフレッシュ化する
let freshenAbove (limit: Level) (ty: SimpleType) (level: Level) : SimpleType =
  eprintfn "trace: freshenAbove limit=%d ty=%O level=%d" limit ty level

  let freshened = Dictionary<Variable, Variable>()

  let rec doFreshen (recurse: SimpleType -> SimpleType) (tv: Variable) : Variable =
    let v = freshVar level
    freshened.Add(tv, v)

    tv.Bounds <- ResizeArray(tv.Bounds |> Seq.map recurse)
    v

  let rec go (ty: SimpleType) : SimpleType =
    if getLevel ty <= limit then
      ty
    else
      match ty with
      | VarST tv ->
        let tv =
          match freshened.TryGetValue(tv) with
          | true, v -> v
          | _ -> doFreshen go tv

        VarST tv

      | FunST(lhs, rhs) ->
        let lhs = go lhs
        let rhs = go rhs
        FunST(lhs, rhs)

      | RecordST fields ->
        let fields = fields |> List.map (fun (name, fieldTy) -> (name, go fieldTy))

        RecordST fields

      | PrimitiveST _ -> ty

  go ty

// -----------------------------------------------
// constrain (制限する)
// -----------------------------------------------

let constrain (lhs: SimpleType) (rhs: SimpleType) : unit =
  eprintfn "trace: constrain %O <: %O" lhs rhs

  let cache = HashSet<SimpleType * SimpleType>()

  let preventRecursion lhs rhs : bool =
    if cache.Contains((lhs, rhs)) then
      true
    else
      // 型変数以外の型は普通の木構造なので、constrain が循環に陥るケースでは、必ず型変数が出現する。
      // そのため、引数のどちらかが型変数であるようなケースだけ弾けばいい。
      match lhs, rhs with
      | VarST _, _
      | _, VarST _ -> cache.Add(lhs, rhs) |> ignore

      | _ -> ()

      false

  let rec go (lhs: SimpleType) (rhs: SimpleType) : unit =
    if preventRecursion lhs rhs |> not then
      eprintfn "  con %O <: %O" lhs rhs

      match lhs, rhs with
      | _ when lhs = rhs -> ()

      | FunST(l0, r0), FunST(l1, r1) ->
        go l1 l0 // ~負の位置なので順番を逆にする~
        go r0 r1

      | RecordST fs0, RecordST fs1 ->
        for n1, t1 in fs1 do
          match fs0 |> List.tryFind (fun (n, _) -> n = n1) with
          | None -> err (sprintf "missing field: %s in %O" n1 t1)
          | Some(_, t0) -> go t0 t1

      | VarST v, rhs when getLevel rhs <= v.Level ->
        // v.UpperBounds.Add(rhs)

        // for t in v.LowerBounds do
        //   go t rhs

        v.Bounds.Add(rhs)

        for t in v.Bounds do
          go t rhs

      | lhs, VarST v when getLevel lhs <= v.Level ->
        // v.LowerBounds.Add(lhs)

        // for t in v.UpperBounds do
        //   go lhs t

        v.Bounds.Add(lhs)

        for t in v.Bounds do
          go lhs t

      | VarST v, rhs ->
        let rhs = extrude v.Level rhs
        go lhs rhs

      | lhs, VarST v ->
        let lhs = extrude v.Level lhs
        go lhs rhs

      | _ -> err (sprintf "cannot constrain %O <: %O" lhs rhs)

  go lhs rhs

// -----------------------------------------------
// extrude (押し出す)
// -----------------------------------------------

/// 型に出現する型変数のレベルを lvl まで下げる
let extrude (lvl: Level) (ty: SimpleType) : SimpleType =
  eprintfn "trace: extrude lvl=%d ty=%O" lvl ty

  let cache = Dictionary<Variable, Variable>()

  let rec go (lvl: Level) (ty: SimpleType) : SimpleType =
    if getLevel ty <= lvl then
      ty
    else
      match ty with
      | FunST(l, r) ->
        let l = go lvl l
        let r = go lvl r
        FunST(l, r)

      | RecordST fs ->
        let fs = fs |> List.map (fun (name, ty) -> (name, go lvl ty))

        RecordST(fs)

      | VarST tv ->
        let v =
          match cache.TryGetValue(tv) with
          | true, v -> v
          | _ ->
            // tv を、より低いレベルを持つフレッシュな型変数に置き換える。
            // 不変条件のため、境界に含まれる変数も再帰的に extrude する必要がある。
            let nvs = freshVar lvl
            cache.Add(tv, nvs)

            // let extrudeBounds (bounds: ResizeArray<SimpleType>) : unit =
            //   for i in 0 .. bounds.Count - 1 do
            //     bounds.[i] <- go lvl bounds.[i]

            // tv.UpperBounds.Add(VarST nvs)
            tv.Bounds.Add(VarST nvs)
            nvs.Bounds <- ResizeArray(tv.Bounds |> Seq.map (go lvl))
            nvs

        VarST v

      | PrimitiveST _ -> ty

  go lvl ty

// -----------------------------------------------
// Ctx
// -----------------------------------------------

/// 環境 (変数の名前から型スキーマへのマップ)
type Ctx = Map<string, TypeScheme>

module Ctx =
  let empty: Ctx = Map.empty

  /// 環境に変数を導入する
  let bind (name: string) (scheme: TypeScheme) (ctx: Ctx) : Ctx = Map.add name scheme ctx

  /// 環境から変数を名前で探す
  let findVar (name: string) (ctx: Ctx) : TypeScheme =
    match ctx |> Map.tryFind name with
    | Some scheme -> scheme
    | None -> err (sprintf "identifier not found: %s" name)

// -----------------------------------------------
// typeTerm
// -----------------------------------------------

/// let 式の右辺 (初期化式) の型を解決する
///
/// 結果はレベル lvl の多相な型スキーム (PolyTS) になる
let typeLetRhs (lvl: Level) (ctx: Ctx) (isRec: IsRec) (name: string) (rhs: Term) : SimpleType =
  match isRec with
  | Rec ->
    // let rec ではいま束縛しようとしている変数を右辺で参照できるので、右辺の型検査を行う時点でその変数を環境に入れておく必要がある。
    // 仮に n: v とする。
    let v = VarST(freshVar (lvl + 1))
    let ctx = ctx |> Ctx.bind name (MonoTS v)

    let ty = typeTerm lvl ctx rhs

    // 型 ty の値を型 v の名前に束縛したいので、ty <: v がいる。
    constrain ty v

    ty

  | NotRec -> typeTerm (lvl + 1) ctx rhs

/// 項の型検査を行う
let typeTerm (lvl: Level) (ctx: Ctx) (term: Term) : SimpleType =
  eprintfn "trace: typeTerm lvl=%d term=%O" lvl term

  match term with
  | VarTerm name ->
    let scheme = ctx |> Ctx.findVar name
    scheme |> instantiate lvl

  | LambdaTerm(name, body) ->
    let paramTy = VarST(freshVar lvl)

    let bodyTy =
      let ctx = ctx |> Ctx.bind name (MonoTS paramTy)
      typeTerm lvl ctx body

    FunST(paramTy, bodyTy)

  | AppTerm(f, a) ->
    let fTy = typeTerm lvl ctx f
    let aTy = typeTerm lvl ctx a
    let res = VarST(freshVar lvl)

    // a: A, f: F <: (A -> T) => f a: T
    constrain fTy (FunST(aTy, res))

    res

  | BoolLitTerm _ -> BoolST

  | IntLitTerm _ -> IntST

  | SelectTerm(record, name) ->
    let recordTy = typeTerm lvl ctx record
    let res = VarST(freshVar lvl)

    // r: R <: { f: T } => (r.f): T
    constrain recordTy (RecordST [ (name, res) ])

    res

  | RecordTerm fs ->
    let fs = fs |> List.map (fun (n, t) -> (n, typeTerm lvl ctx t))

    RecordST fs

  | LetTerm(isRec, name, rhs, body) ->
    let ty = typeLetRhs lvl ctx isRec name rhs
    let scheme = PolyTS(lvl, ty)

    let ctx = ctx |> Ctx.bind name scheme
    typeTerm lvl ctx body

// -----------------------------------------------
// API
// -----------------------------------------------

type TyResult = Result<Ty, string>

/// トップレベルの名前と、それの型検査の結果のマップ
type InferenceResult = (string * TyResult) list

let inferTypes (coalesceTy: SimpleType -> Ty) (ctx: Ctx) (ast: Ast) : InferenceResult =
  let mutable ctx = ctx
  let results = ResizeArray<string * TyResult>()

  for decl in ast.Decls do
    eprintfn "trace: ---- inferType (%s) ----" decl.Name
    eprintfn "trace: rhs = %O" decl.Rhs

    let lvl = 0

    let { IsRec = isRec
          Name = name
          Rhs = rhs } =
      decl

    let result, ty =
      try
        let ty = typeLetRhs lvl ctx isRec name rhs
        Ok(coalesceTy ty), ty
      with TypeCheckException msg ->
        Error msg, ErrorST

    ctx <- ctx |> Ctx.bind name (PolyTS(lvl, ty))
    results.Add(name, result)

  List.ofSeq results

/// 型を推論の内部的な表現から通常の (イミュータブルな) 表現に変換する
let coalesceTy (simpleTy: SimpleType) : Ty =
  let recursive = Dictionary<Variable, TyVar>()

  // mutable hash set
  let inProcess = HashSet<Variable>()

  let rec go simpleTy : Ty =
    match simpleTy with
    | VarST tv ->
      let polarVar = tv

      if inProcess.Contains(polarVar) then
        let v =
          match recursive.TryGetValue(polarVar) with
          | true, v -> v
          | _ ->
            let v = (freshVar 0).TyVar
            recursive.Add(polarVar, v)
            v

        VarTy v
      else
        // let (bounds, folder) = tv.LowerBounds, (fun lhs rhs -> UnionTy(lhs, rhs))

        // inProcess.Add(polarVar) |> ignore

        // let result: Ty =
        //   bounds |> Seq.map (fun ty -> go ty) |> Seq.fold folder (VarTy tv.TyVar)

        // inProcess.Remove(polarVar) |> ignore

        let bounds, folder = tv.Bounds, (fun lhs rhs -> InterTy(lhs, rhs))

        inProcess.Add(tv) |> ignore

        let result: Ty =
          bounds |> Seq.map go |> Seq.fold folder (VarTy tv.TyVar)

        inProcess.Remove(tv) |> ignore

        match recursive.TryGetValue(polarVar) with
        | true, v -> MuTy(v, result)
        | _ -> result

    | FunST(lhs, rhs) ->
      let lhs = go lhs
      let rhs = go rhs
      FnTy(lhs, rhs)

    | RecordST fields ->
      let fields = fields |> List.map (fun (name, ty) -> (name, go ty))

      RecordTy fields

    | PrimitiveST name -> PrimTy name

  go simpleTy
