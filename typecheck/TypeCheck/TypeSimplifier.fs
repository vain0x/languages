module TypeCheck.TypeSimplifier

open System.Collections.Generic
open TypeCheck.Helpers
open TypeCheck.Syntax
open TypeCheck.TypeChecker

type PolarVar = Variable

// -----------------------------------------------
// CompactType
// -----------------------------------------------

/// 型の集まり
///
/// 正の位置に出現するときは併合型を、負の位置に出現するときは交差型を表す
type CompactType =
  { Vars: Variable list
    Prims: string list
    Record: (string * CompactType) list option
    Fn: (CompactType * CompactType) option }

  override this.ToString() : string =
    seq {
      yield! this.Vars |> Seq.map string
      yield! this.Prims

      match this.Record with
      | Some fields ->
        yield
          fields
          |> Seq.map (fun (n, t) -> sprintf "%s: %s" n (string t))
          |> String.concat ", "
          |> sprintf "{ %s }"

      | None -> ()
    }
    |> String.concat ", "
    |> sprintf "<%s>"

module CompactType =
  let empty: CompactType =
    { Vars = []
      Prims = []
      Record = None
      Fn = None }

  let ofVar v : CompactType = { empty with Vars = [ v ] }

  let ofVars vs : CompactType = { empty with Vars = vs }

  let ofPrim name : CompactType = { empty with Prims = [ name ] }

  let ofRecord fs : CompactType = { empty with Record = Some fs }

  let ofFn l r : CompactType = { empty with Fn = Some(l, r) }

  let isEmpty (ty: CompactType) : bool =
    ty.Vars |> List.isEmpty
    && ty.Prims |> List.isEmpty
    && ty.Record |> Option.isNone
    && ty.Fn |> Option.isNone

  let rec merge (lhs: CompactType) (rhs: CompactType) : CompactType =
    let vars = List.append lhs.Vars rhs.Vars
    let prims = List.append lhs.Prims rhs.Prims

    let record =
      match lhs.Record, rhs.Record with
      | None, None -> None

      | Some record, None
      | None, Some record -> Some record

      | Some lhs, Some rhs ->
        [ for n, l in lhs do
            match rhs |> List.tryFind (fun (name, _) -> name = n) with
            | Some(_, r) -> yield n, merge l r
            | None -> () ]
        |> Some

    let fn =
      match lhs.Fn, rhs.Fn with
      | None, None -> None

      | Some fn, None
      | None, Some fn -> Some fn

      | Some(l0, l1), Some(r0, r1) ->
        let s = merge l0 l1
        let t = merge r0 r1
        Some(s, t)

    { Vars = vars
      Prims = prims
      Record = record
      Fn = fn }

// -----------------------------------------------
// CompactTyScheme
// -----------------------------------------------

type CompactTyScheme =
  { Term: CompactType
    RecVars: Dictionary<Variable, CompactType> }

/// 推論結果の型をコンパクトな表現に変換する。
///
/// 例えば a <: <{ x: A }, { x: B, y: C }> を a <: { x: A ∧ B, y: C } に変形する。
let toCompactTy (ty: SimpleType) : CompactTyScheme =
  let recursive = Dictionary<PolarVar, Variable>()
  let recVars = Dictionary<Variable, CompactType>()
  let inProcess = HashSet<PolarVar>()

  let mutable parents = HashSet<Variable>()

  let withoutParents f =
    let oldParents = parents
    parents <- HashSet()

    try
      f ()
    finally
      parents <- oldParents

  let rec go ty : CompactType =
    match ty with
    | PrimitiveST name -> CompactType.ofPrim name

    | FunST(l, r) ->
      withoutParents (fun () ->
        let l = go l
        let r = go r
        CompactType.ofFn l r)

    | RecordST fs -> withoutParents (fun () -> fs |> List.map (fun (n, t) -> n, go t) |> CompactType.ofRecord)

    | VarST tv ->
      // let bounds = tv.LowerBounds
      let bounds = tv.Bounds

      if inProcess.Contains(tv) then
        if parents.Contains(tv) then
          // > we have a spurious cycle: ignore the bound
          CompactType.empty
        else
          let v =
            match recursive.TryGetValue(tv) with
            | true, v -> v

            | _ ->
              let v = freshVar 0
              recursive.Add(tv, v)
              v

          CompactType.ofVar v
      else
        inProcess.Add(tv) |> ignore
        parents.Add(tv) |> ignore

        let mutable bound = CompactType.ofVar tv

        for b in bounds do
          let b = go b
          bound <- CompactType.merge bound b

        parents.Remove(tv) |> ignore
        inProcess.Remove(tv) |> ignore

        match recursive.TryGetValue(tv) with
        | true, v ->
          recVars.Add(v, bound)
          CompactType.ofVar v

        | _ -> bound

  let ty = go ty
  let cty: CompactTyScheme = { Term = ty; RecVars = recVars }
  cty

type PolarVars = (Variable list)

let closeOver xs f = []

// def closeOver[A](xs: Set[A])(f: A => Set[A]): Set[A] =
//   closeOverCached(Set.empty, xs)(f)
// def closeOverCached[A](done: Set[A], todo: Set[A])(f: A => Set[A]): Set[A] =
//   if (todo.isEmpty) done else {
//     val newDone = done ++ todo
//     closeOverCached(newDone, todo.flatMap(f) -- newDone)(f)
//   }

let canonicalizeTy (ty: SimpleType) : CompactTyScheme =
  let recursive = Dictionary<PolarVars, Variable>()
  let recVars = Dictionary<Variable, CompactType>()
  let inProcess = HashSet<PolarVars>()

  let rec go0 (ty: SimpleType) : CompactType =
    match ty with
    | PrimitiveST name -> CompactType.ofPrim name

    | FunST(l, r) ->
      let l = go0 l
      let r = go0 r
      CompactType.ofFn l r

    | RecordST fs -> fs |> List.map (fun (n, t) -> n, go0 t) |> CompactType.ofRecord

    | VarST tv ->
      closeOver [ tv ] (fun ty ->
        match ty with
        | VarST tv1 ->
          // let bounds = tv1.LowerBounds

          [ for t in tv1.Bounds do
              match t with
              | VarST v -> yield v
              | _ -> () ]

        | _ -> [])
      |> CompactType.ofVars

  and go1 (ty: CompactType) =
    if ty |> CompactType.isEmpty then
      ty
    else if

      inProcess.Contains(ty.Vars)
    then
      ty.Vars
      |> List.map (fun v ->
        match recursive.TryGetValue(ty.Vars) with
        | true, v -> v
        | _ ->
          let v = freshVar 0
          recursive.Add(ty.Vars, v)
          v)
      |> CompactType.ofVars
    else
      let bound =
        seq {
          for tv in ty.Vars do
            // let bounds = tv.LowerBounds
            let bounds = tv.Bounds

            for b in bounds do
              match b with
              | VarST v -> ()
              | _ -> go0 b
        }
        |> Seq.fold (CompactType.merge) CompactType.empty

      let res = CompactType.merge ty bound

      if ty.Vars |> List.isEmpty |> not then
        inProcess.Add(ty.Vars) |> ignore

      try
        let adapted: CompactType =
          { Vars = res.Vars
            Prims = res.Prims
            Record = res.Record |> Option.map (fun fs -> fs |> List.map (fun (n, t) -> n, go1 t))
            Fn = res.Fn |> Option.map (fun (l, r) -> go1 l, go1 r) }

        match recursive.TryGetValue(ty.Vars) with
        | true, v ->
          recVars.Add(v, adapted)
          CompactType.ofVar v

        | _ -> adapted

      finally
        inProcess.Remove(ty.Vars) |> ignore

  let ty = go0 ty
  let ty = go1 ty
  let cty: CompactTyScheme = { Term = ty; RecVars = recVars }
  cty

// アイディア: 型変数 'a, 'b が正の位置で必ず同時に出現するなら、この2つの型変数は区別できないので、ユニファイしてよい。
//    例: ('a & 'b) -> ('a, 'b) は 'a -> ('a, 'a) と等しい
//    例: ('a & 'b) -> 'b -> ('a, 'b) は 'a -> 'a' -> ('a, 'a) とは異なる
//        ( )
//    例: 'a -> 'b -> ('a | 'b) は 'a -> 'a -> 'a と等しい。
let simplifyTy (cty: CompactTyScheme) : CompactTyScheme =
  let allVars = HashSet(cty.RecVars.Keys)
  let recVars = Dictionary<TyVar, _>()
  let coOccurrences = Dictionary<TyVar, HashSet<_>>()

  let varSubst = Dictionary<TyVar, _>()

  let rec go (ty: CompactType) : (unit -> CompactType) =
    for tv in ty.Vars do
      allVars.Add(tv) |> ignore

      let newOccurrences =
        HashSet(
          seq {
            for tv in ty.Vars do
              VarTy(tv.TyVar)

            for prim in ty.Prims do
              PrimTy prim
          }
        )

      match coOccurrences.TryGetValue(tv.TyVar) with
      | true, os -> os.RemoveWhere(fun x -> newOccurrences.Contains(x) |> not) |> ignore
      | false, _ -> coOccurrences.Add(tv.TyVar, newOccurrences) |> ignore

      match cty.RecVars.TryGetValue(tv) with
      | true, _b ->
        // b: bounds
        if recVars.ContainsKey(tv.TyVar) |> not then
          failwith "unimplemented"
      | false, _ -> ()

      ()

    let rec_ = ty.Record |> Option.map (fun r -> r |> List.map (fun (n, t) -> n, go t))
    let fun_ = ty.Fn |> Option.map (fun (l, r) -> go l, go r)

    fun () ->
      let newVars =
        ty.Vars
        |> List.choose (fun (tv: Variable) ->
          match varSubst.TryGetValue(tv.TyVar) with
          | true, Some(tv2) -> Some tv2
          | true, None -> None
          | false, _ -> Some tv)

      ({ ty with
          Vars = newVars
          Record = rec_ |> Option.map (fun r -> r |> List.map (fun (n, t) -> n, t ()))
          Fn = fun_ |> Option.map (fun (l, r) -> l (), r ()) }
      : CompactType)

  let gone = go cty.Term

  // eprintfn "[occ] %A" coOccurrences
  // eprintfn "[rec] %A" recVars

  for v0 in allVars do
    if not (recVars.ContainsKey(v0.TyVar)) then
      match coOccurrences.TryGetValue(v0.TyVar) with
      | true, _ ->
        eprintfn $"[!] {v0}"
        varSubst.Add(v0.TyVar, None)

      | false, _ -> ()

  for v in allVars do
    if not (varSubst.ContainsKey(v.TyVar)) then
      let os =
        match coOccurrences.TryGetValue(v.TyVar) with
        | true, os -> Array.ofSeq os
        | false, _ -> [||]

      eprintfn "[v] %O %A" v os

      for os in [ os ] do
        // w: var or primitive
        for w in os do
          match w with
          | VarTy w ->
            if
              w <> v.TyVar
              && not (varSubst.ContainsKey(w))
              && (recVars.ContainsKey(v.TyVar) = recVars.ContainsKey(w))
            then
              eprintfn "[w] %O" w
              // merge w into v
              varSubst.Add(w, Some v)

              let coOccur =
                match coOccurrences.TryGetValue(w) with
                | true, set -> set.Contains(VarTy v.TyVar)
                | false, _ -> true

              if coOccur then
                match recVars.TryGetValue(w) with
                | true, wBounds ->
                  // v, w は再帰変数
                  // w は v にマージされたので消す
                  recVars.Remove(w) |> ignore
                  let vBounds = recVars[v.TyVar]
                  recVars[v.TyVar] <- fun () -> CompactType.merge (vBounds ()) (wBounds ())

                | false, _ ->
                  // v, w は再帰変数でない
                  let wCo = coOccurrences[w]

                  coOccurrences[v.TyVar]
                    .RemoveWhere(fun x -> x <> VarTy v.TyVar && not (wCo.Contains(x)))
                  |> ignore

          | PrimTy prim ->
            match coOccurrences.TryGetValue(v.TyVar) with
            | true, co when co.Contains(PrimTy prim) -> varSubst.Add(v.TyVar, None) |> ignore

            | _ -> ()

          | _ -> failwith "unreachable"

  // let recVars =Dictionary(recVars |> Seq.map (fun (KeyValue(v, t)) -> KeyValuePair(v, t ())))
  let recVars = Dictionary()
  { Term = gone (); RecVars = recVars }: CompactTyScheme

let coalesceCompactTy (cty: CompactTyScheme) : Ty = failwith ""

let coalesceTySimplified (ty: SimpleType) : Ty =
  // ty |> toCompactTy |> simplifyTy |> coalesceCompactTy

  eprintfn "trace: coalesceTy: %O" ty

  let recursive = Dictionary<Variable, TyVar>()
  let inProcess = HashSet<Variable>()
  let unification = Dictionary()

  let rec go (ty: SimpleType) =
    match ty with

    | VarST tv ->
      eprintfn "  %O: bounds [%O]" tv (tv.Bounds |> Seq.map string |> String.concat ", ")

      if inProcess.Contains(tv) then
        match recursive.TryGetValue(tv) with
        | true, tv1 -> VarTy tv1

        | false, _ ->
          let tv1 = (freshVar 0).TyVar
          recursive.Add(tv, tv1)
          VarTy tv1
      else
        inProcess.Add(tv) |> ignore

        let res =
          tv.Bounds
          |> Seq.map go
          |> Seq.fold (fun lhs rhs -> InterTy(lhs, rhs)) (VarTy tv.TyVar)

        eprintfn "  subst %O -> %O" tv res
        inProcess.Remove(tv) |> ignore

        // match recursive.TryGetValue(tv) with
        // | true, r ->
        //   InterTy(res, RecTy(r))
        // | false, _ ->
        //   res

        res

    | FunST(l, r) -> FnTy(go l, go r)
    | RecordST fields -> fields |> List.map (fun (n, ty) -> n, go ty) |> RecordTy
    | PrimitiveST name -> PrimTy name

  let result = go ty
  eprintfn "  => %O" result
  result
