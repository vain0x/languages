open Location
module T = Syntax_token

type name = {
  name_text : string;
  name_referent : syntax_referent ref;
  name_loc : loc;
}

and param = { param_name : T.token; param_ty : ty; param_loc : loc }

and ty = Name_ty of name

and if_expr = { if_loc : loc; if_then : expr option; if_else : expr option }

and expr =
  | Number_expr of string * loc
  | String_expr of string * loc
  | Name_expr of name
  | If_expr of if_expr
  | Semi_expr of expr list

and do_stmt = { do_loc : loc; do_body : expr option }

and val_stmt = { val_loc : loc; let_name : name; val_body : expr option }

and fn_stmt = {
  fn_loc : loc;
  fn_params : param list;
  fn_result_ty : ty option;
  fn_name : name;
}

and stmt =
  | Do_stmt of do_stmt
  | Val_stmt of val_stmt
  | Fn_stmt of fn_stmt
  | Semi_stmt of string

and syntax_referent = No_referent | Def_referent | Use_referent of stmt
