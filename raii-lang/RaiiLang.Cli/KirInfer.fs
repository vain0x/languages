module rec RaiiLang.KirInfer

open RaiiLang.Helpers
open RaiiLang.Kir

type KirInferContext =
  {
    TyMap: HashMap<string, KTy>
  }

let kiContextNew (): KirInferContext =
  {
    TyMap = HashMap()
  }

let kiNode (context: KirInferContext) node =
  match node with
  | KBool _ ->
    KBoolTy

  | KInt _ ->
    KIntTy

  | KStr _ ->
    KStrTy

  | KName name ->
    match context.TyMap.TryGetValue(name) with
    | true, ty ->
      ty

    | false, _ ->
      KIntTy

  | KPrim (_, _, _, next) ->
    next |> kiNode context

  | KJump (KLabel label, args) ->
    let argTys =
      args |> List.map (fun (KArg (callBy, arg)) ->
        let ty = arg |> kiNode context
        callBy, ty
      )

    let funTy =
      KFunTy argTys

    context.TyMap.[label] <- funTy

    KNeverTy

  | KFix (_, _, _, body, next) ->
    body |> kiNode context |> ignore
    next |> kiNode context

let kirInfer (node: KNode) =
  let context = kiContextNew ()
  node |> kiNode context |> ignore
  context
