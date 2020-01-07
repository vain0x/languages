module rec RaiiLang.KirDump

open RaiiLang.Helpers
open RaiiLang.Kir

let kdCont cont acc =
  match cont with
  | KLabelCont (KLabel (labelName, _, _)) ->
    acc |> cons labelName

  | KReturnCont _ ->
    acc |> cons "return"

let kdTy ty indent acc =
  match ty |> kTyDeref with
  | KInferTy (name, _) ->
    acc |> cons "'" |> cons name

  | KNeverTy ->
    acc |> cons "never"

  | KUnitTy ->
    acc |> cons "()"

  | KBoolTy ->
    acc |> cons "bool"

  | KIntTy ->
    acc |> cons "int"

  | KStrTy ->
    acc |> cons "string"

  | KFunTy (paramList, result) ->
    acc
    |> cons "Fn"
    |> kdParamList paramList indent
    |> cons " -> "
    |> kdResult result indent

let kdParam param indent acc =
  match param with
  | KParam (mode, name, ty) ->
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

let kdArg arg _indent acc =
  match arg with
  | KArg (passBy, name) ->
    acc
    |> cons (passByToString passBy)
    |> cons " "
    |> cons name

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

let kdResult (KResult resultTy) indent acc =
  acc |> kdTy resultTy indent

let kdNode node indent acc =
  match node with
  | KName name ->
    acc |> cons name

  | KPrim (prim, args, next) ->
    let acc = acc |> cons "jump " |> kdCont next |> cons "("

    let acc = acc |> cons (kPrimToString prim)

    let acc =
      if prim |> kPrimIsLiteral then
        assert (args |> List.isEmpty)
        acc
      else
        acc |> kdArgList args indent

    acc |> cons ")"

  | KIf (cond, body, alt) ->
    let deepIndent = indent + "  "

    acc
    |> cons "if "
    |> cons cond
    |> cons " {"
    |> cons deepIndent
    |> kdNode body deepIndent
    |> cons indent
    |> cons "} else {"
    |> cons deepIndent
    |> kdNode alt deepIndent
    |> cons indent
    |> cons "}"

  | KFix (fix, next) ->
    let deepIndent = indent + "  "

    let kind, name, paramList, result, body =
      match fix with
      | KLabelFix (KLabel (name, paramList, body)) ->
        "label", name, paramList, KResult KNeverTy, !body

      | KFnFix (KFn (name, paramList, result, body)) ->
        "fn", name, paramList, result, !body

    acc
    |> cons "fix "
    |> cons kind
    |> cons " "
    |> cons name
    |> kdParamList paramList indent
    |> cons " -> "
    |> kdResult result indent
    |> cons " {"
    |> cons deepIndent
    |> kdNode body deepIndent
    |> cons indent
    |> cons "}"
    |> cons eol
    |> cons indent
    |> kdNode next indent

let kirDump (node: KNode) =
  []
  |> kdNode node eol
  |> List.rev
  |> String.concat ""
