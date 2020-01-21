module rec PhancieLang.KirDump

open PhancieLang.Helpers
open PhancieLang.Kir

let kdCont cont acc =
  match cont with
  | KLabelCont (KLabel (labelName, _, _, _)) ->
    acc |> cons labelName

  | KReturnCont _ ->
    acc |> cons "return"

let kdParamTy paramTy indent acc =
  match paramTy with
  | KParamTy (mode, ty, _) ->
    acc
    |> cons (modeToString mode)
    |> cons " "
    |> kdTy ty indent

let kdParamTys paramTys indent acc =
  let acc = acc |> cons "("

  let _, acc =
    paramTys
    |> List.fold (fun (sep, acc) paramTy ->
      let acc =
        acc
        |> cons sep
        |> kdParamTy paramTy indent
      ", ", acc
    ) ("", acc)

  acc

let kdTy ty indent acc =
  match ty with
  | KNeverTy, _ ->
    acc |> cons "never"

  | KUnitTy, _ ->
    acc |> cons "()"

  | KBoolTy, _ ->
    acc |> cons "bool"

  | KIntTy, _ ->
    acc |> cons "int"

  | KStrTy, _ ->
    acc |> cons "string"

  | KFnTy (paramTys, resultTy), _ ->
    acc
    |> cons "fn"
    |> kdParamTys paramTys indent
    |> cons " -> "
    |> kdTy resultTy indent

let kdParam param indent acc =
  match param with
  | KParam (mode, name, ty, _) ->
    acc
    |> cons (modeToString mode)
    |> cons " "
    |> cons name
    |> cons ": "
    |> kdTy ty indent

let kdParamList paramList indent acc =
  let acc = acc |> cons "("

  let _, acc =
    paramList
    |> List.fold (fun (sep, acc) param ->
      let acc =
        acc
        |> cons sep
        |> kdParam param indent
      ", ", acc
    ) ("", acc)

  acc |> cons ")"

let kdArg arg indent acc =
  match arg with
  | KArg (passBy, term, _) ->
    acc
    |> cons (passByToString passBy)
    |> cons " "
    |> kdTerm term indent

let kdArgList args indent acc =
  let acc = acc |> cons "("

  let acc =
    match args with
    | [] ->
      acc

    | [arg] ->
      acc |> kdArg arg indent

    | _ ->
      let deepIndent = indent + "  "

      args
      |> List.fold (fun acc arg ->
        acc
        |> cons deepIndent
        |> kdArg arg indent
      ) acc
      |> cons indent

  acc |> cons ")"

let kdFix fix indent acc =
  let deepIndent = indent + "  "

  let kind, name, paramList, resultTy, bodyOpt =
    match fix with
    | KLabelFix (KLabel (name, paramList, bodySlot, syn)) ->
      "label", name, paramList, struct (KNeverTy, syn), !bodySlot

    | KFnFix (KFn (name, paramList, resultTy, bodySlot, _)) ->
      "fn", name, paramList, resultTy, !bodySlot

  acc
  |> cons kind
  |> cons " "
  |> cons name
  |> kdParamList paramList indent
  |> cons " -> "
  |> kdTy resultTy indent
  |> cons " {"
  |> cons deepIndent
  |> kdNodeOpt bodyOpt deepIndent
  |> cons indent
  |> cons "}"

let kdTerm term _indent acc =
  match term with
  | KBoolLiteral false, _ ->
    acc |> cons "false"

  | KBoolLiteral true, _ ->
    acc |> cons "true"

  | KIntLiteral intText, _ ->
    acc |> cons intText

  | KStrLiteral segments, _ ->
    acc |> strUnescape segments

  | KLocalTerm (KParam (_, name, _, _)), _ ->
    acc |> cons name

let kdNode (node: KNodeData) indent acc =
  match node with
  | KNoop, _ ->
    acc |> cons "__noop"

  | KPrim (prim, args, [result], [next]), _ ->
    acc
    |> cons "let "
    |> kdParam result indent
    |> cons " = "
    |> cons (kPrimToString prim)
    |> kdArgList args indent
    |> cons ")"
    |> cons indent
    |> kdNode next indent

  | KPrim (KIfPrim, [KArg (_, cond, _)], [], [body; alt]), _ ->
    let deepIndent = indent + "  "

    acc
    |> cons "if "
    |> kdTerm cond indent
    |> cons " {"
    |> cons deepIndent
    |> kdNode body indent
    |> cons indent
    |> cons "} else {"
    |> cons deepIndent
    |> kdNode alt indent
    |> cons indent
    |> cons "}"

  | KPrim (prim, args, results, nexts), _ ->
    let deepIndent = indent + "  "

    let acc =
      acc
      |> cons "branch "
      |> cons (kPrimToString prim)
      |> kdArgList args indent
      |> cons indent
      |> cons "into ["

    let acc =
      results |> List.fold (fun acc result ->
        acc
        |> cons indent
        |> kdParam result indent
      ) acc

    let acc =
      acc |> cons "] with "

    let acc =
      nexts |> List.fold (fun acc next ->
        acc
        |> cons "{"
        |> cons deepIndent
        |> kdNode next indent
        |> cons indent
        |> cons "}"
      ) acc

    acc

  | KJump (cont, args), _ ->
    acc
    |> kdCont cont
    |> kdArgList args indent

  | KFix (fixes, next), _ ->
    let acc = acc |> cons "fix "

    let _, acc =
      fixes |> List.fold (fun (sep, acc) fix ->
        let acc =
          acc
          |> cons sep
          |> kdFix fix indent

        eol + indent + "and ", acc
      ) ("", acc)

    acc
    |> cons eol
    |> cons indent
    |> kdNode next indent

let kdNodeOpt nodeOpt indent acc =
  match nodeOpt with
  | Some node ->
    acc |> kdNode node indent

  | None ->
    acc |> cons "???"

let kirDump (node: KNodeData) =
  []
  |> kdNode node eol
  |> List.rev
  |> String.concat ""
