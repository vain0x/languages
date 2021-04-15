module rec MiloneSyntaxV2.Dump

open MiloneSyntaxV2.Source
open MiloneSyntaxV2.Syntax

let private deeper indent = indent + "  "

let private cons head tail = head :: tail

let private ifOption folder opt state =
  Option.fold (fun state x -> folder x state) state opt

let private forList folder xs state =
  List.fold (fun state x -> folder x state) state xs

let private todo () = failwith "TODO"

// -----------------------------------------------
// Misc
// -----------------------------------------------

let private dumpMissing hint pos acc =
  acc
  |> cons "(* "
  |> cons hint
  |> cons " "
  |> cons (Pos.toString pos)
  |> cons " *)"

let private dumpName name acc =
  match name with
  | AName (name, _) -> acc |> cons name
  | AMissingName (_, pos) -> acc |> dumpMissing "name" pos

let private dumpLit lit acc =
  match lit with
  | AIntLit text -> acc |> cons text
  | AFloatLit text -> acc |> cons text
  | ABoolLit false -> acc |> cons "false"
  | ABoolLit true -> acc |> cons "true"
  | ACharLit text -> acc |> cons text
  | AStringLit text -> acc |> cons text

let private dumpPath path acc =
  let name, tail = path

  acc
  |> dumpName name
  |> forList (fun name acc -> acc |> cons "." |> dumpName name) tail

// -----------------------------------------------
// Types
// -----------------------------------------------

let private dumpTy ty acc =
  match ty with
  | APathTy path -> acc |> dumpPath path
  | _ -> todo ()

// -----------------------------------------------
// Patterns
// -----------------------------------------------

let private dumpPat pat acc =
  match pat with
  | AVarPat name -> acc |> dumpName name
  | _ -> todo ()

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private dumpBinary binary =
  match binary with
  | AAddEb -> "+"
  | ASubEb -> "-"
  | AMulEb -> "*"
  | ADivEb -> "/"
  | AModuloEb -> "%"
  | ABitAndEb -> "&&&"
  | ABitOrEb -> "|||"
  | ABitXorEb -> "^^^"
  | ALeftShiftEb -> "<<<"
  | ARightShiftEb -> ">>>"
  | AEqualEb -> "="
  | ANotEqualEb -> "<>"
  | ALessEb -> "<"
  | ALessEqualEb -> "<="
  | AGreaterEb -> ">"
  | AGreaterEqualEb -> ">="
  | ALogicalOrEb -> "||"
  | ALogicalAndEb -> "&&"

let private dumpNodeExpr indent kind args acc =
  match kind, args with
  | ABinaryEk binary, [ l; r ] ->
      acc
      |> dumpExpr indent l
      |> cons " "
      |> cons (dumpBinary binary)
      |> cons " "
      |> dumpExpr indent r

  | _ -> todo ()

let private dumpBlockExpr indent (block: ABlockExpr) acc =
  match block.Stmts with
  | [] -> acc |> cons "()"

  | stmt :: stmts ->
      acc
      |> dumpStmt indent stmt
      |> forList
           (fun stmt acc ->
             acc
             |> cons "\n"
             |> cons indent
             |> dumpStmt indent stmt)
           stmts

let private dumpExpr indent expr acc =
  match expr with
  | AMissingExpr pos -> acc |> dumpMissing "expr" pos

  | ALitExpr (lit, _) -> acc |> dumpLit lit
  | APathExpr path -> acc |> dumpPath path

  | ANodeExpr (kind, args, _) -> dumpNodeExpr indent kind args acc
  | AClosureExpr _ -> todo ()
  | ACallExpr _ -> todo ()
  | AMatchExpr _ -> todo ()
  | ABlockExpr block -> dumpBlockExpr indent block acc

// -----------------------------------------------
// Statements
// -----------------------------------------------

let private dumpParam param acc =
  match param with
  | Some name, None -> acc |> dumpName name

  | Some name, Some ty ->
      acc
      |> cons "("
      |> dumpName name
      |> cons ": "
      |> dumpTy ty
      |> cons ")"

  | None, Some ty -> acc |> cons "(_: " |> dumpTy ty |> cons ")"

  | None, None -> acc |> cons "_"

let private dumpLetStmt indent (stmt: ALetStmt) acc =
  let deep = deeper indent

  acc
  |> cons "let "
  |> dumpPat stmt.Pat
  |> cons " =\n"
  |> cons deep
  |> dumpExpr deep stmt.Init

let private dumpFnStmt indent (stmt: AFnStmt) acc =
  let deep = deeper indent

  acc
  |> cons "// @ "
  |> cons (Pos.toString stmt.Pos)
  |> cons "\n"
  |> cons indent
  |> cons "let "
  |> dumpName stmt.Name
  |> forList (fun param acc -> acc |> cons " " |> dumpParam param) stmt.ParamList
  |> ifOption (fun (ty, _) acc -> acc |> cons ": " |> dumpTy ty) stmt.Result
  |> cons " =\n"
  |> cons deep
  |> dumpExpr deep stmt.Body

let private dumpStmt indent stmt acc =
  match stmt with
  | AMissingStmt pos -> acc |> dumpMissing "stmt" pos
  | AExprStmt expr -> acc |> dumpExpr indent expr
  | ALetStmt stmt -> dumpLetStmt indent stmt acc
  | AFnStmt stmt -> dumpFnStmt indent stmt acc
  | AEnumStmt stmt -> todo ()
  | ARecordStmt stmt -> todo ()
  | ATypeStmt stmt -> todo ()
  | AModStmt stmt -> todo ()
  | AUseStmt stmt -> todo ()

// -----------------------------------------------
// Interface
// -----------------------------------------------

let dump (projectName: string) (moduleName: string) (root: ARoot): string =
  let (ARoot stmts) = root

  let acc =
    []
    |> cons "module rec "
    |> cons projectName
    |> cons "."
    |> cons moduleName

  let indent = ""

  acc
  |> forList (fun stmt acc -> acc |> cons "\n\n" |> dumpStmt indent stmt) stmts
  |> List.rev
  |> String.concat ""
