module rec PhancieLang.AstGen

open PhancieLang.Ast
open PhancieLang.Helpers
open PhancieLang.Syntax

let astName (syn: SyntaxNode) =
  assert (synToKind syn = NameNode)

  let ident =
    syn
    |> synToFirstToken ((=) IdentToken)
    |> Option.map tokenToText

  AName (ident, ref None, syn)

let astTy (syn: SyntaxNode) =
  assert (synToKind syn = TyNode)

  let name =
    syn
    |> synToFirstNode ((=) NameNode)
    |> Option.map astName

  ATy (name, syn)

let astParam (syn: SyntaxNode) =
  assert (synToKind syn = ParamNode)

  let mode =
    syn
    |> synToFirstToken (tokenAsMode >> Option.isSome)
    |> Option.bind (fun token -> tokenAsMode token.Token)
    |> Option.defaultValue ValMode

  let name =
    syn
    |> synToFirstNode ((=) NameNode)
    |> Option.map astName

  let ty =
    syn
    |> synToFirstNode ((=) TyNode)
    |> Option.map astTy

  AParam (mode, name, ty, syn)

let astResult (syn: SyntaxNode) =
  assert (synToKind syn = ResultNode)

  let ty =
    syn
    |> synToFirstNode ((=) TyNode)
    |> Option.map astTy

  AResult (ty, syn)

let astArg (syn: SyntaxNode) =
  assert (synToKind syn = ArgNode)

  let passBy =
    syn
    |> synToFirstToken (tokenAsPassBy >> Option.isSome)
    |> Option.bind (fun token -> tokenAsPassBy token.Token)
    |> Option.defaultValue ByIn

  let term =
    syn
    |> synToFirstNode nodeIsTerm
    |> Option.map astTerm

  AArg (passBy, term, syn)

let astBoolLiteral (syn: SyntaxNode) =
  assert (synToKind syn = BoolLiteralNode)

  let value =
    syn
    |> synToFirstToken (fun token -> token = FalseToken || token = TrueToken)
    |> Option.map (fun token -> token.Token = TrueToken)
    |> Option.defaultValue false

  ABoolLiteral (value, syn)

let astIntLiteral (syn: SyntaxNode) =
  assert (synToKind syn = IntLiteralNode)

  let intToken =
    syn
    |> synToFirstToken ((=) IntToken)
    |> Option.map tokenToText

  AIntLiteral (intToken, syn)

let astStrLiteral (syn: SyntaxNode) =
  assert (synToKind syn = StrLiteralNode)

  let segments =
    syn
    |> synToFilterToken ((=) StrVerbatimToken)
    |> List.map (tokenToText >> StrVerbatim)

  AStrLiteral (segments, syn)

let astGroup (syn: SyntaxNode) =
  assert (synToKind syn = GroupNode)

  let item =
    syn
    |> synToFirstNode nodeIsTerm
    |> Option.map astTerm

  AGroupTerm (item, syn)

let astBlock (syn: SyntaxNode) =
  assert (synToKind syn = BlockNode)

  let item =
    syn
    |> synToFilterNode nodeIsStmt
    |> List.map astStmt

  ABlockTerm (item, syn)

let astBreak (syn: SyntaxNode) =
  assert (synToKind syn = BreakNode)

  ABreakTerm (ref None, syn)

let astContinue (syn: SyntaxNode) =
  assert (synToKind syn = ContinueNode)

  AContinueTerm (ref None, syn)

let astLoop (syn: SyntaxNode) =
  assert (synToKind syn = LoopNode)

  let body =
    syn
    |> synToFirstNode nodeIsTerm
    |> Option.map astTerm

  ALoopTerm (body, ref None, syn)

let astCall (syn: SyntaxNode) =
  assert (synToKind syn = CallNode)

  let cal =
    syn
    |> synToFirstNode nodeIsTerm
    |> Option.map astTerm

  let args =
    syn
    |> synToFilterNode ((=) ArgNode)
    |> List.map astArg

  ACallTerm (cal, args, syn)

let astIf (syn: SyntaxNode) =
  assert (synToKind syn = IfNode)

  let cond =
    syn
    |> synToFirstNode nodeIsTerm
    |> Option.map astTerm

  let body =
    syn
    |> synToFirstNode ((=) ThenNode)
    |> Option.bind (synToFirstNode nodeIsTerm)
    |> Option.map astTerm

  let alt =
    syn
    |> synToFirstNode ((=) ElseNode)
    |> Option.bind (synToFirstNode nodeIsTerm)
    |> Option.map astTerm

  AIfTerm (cond, body, alt, ref None, syn)

let astWhile (syn: SyntaxNode) =
  assert (synToKind syn = WhileNode)

  let terms =
    syn
    |> synToFilterNode nodeIsTerm
    |> List.map astTerm

  // while キーワードの左と右からそれぞれ探す方がいい。
  let cond = terms |> List.tryItem 0
  let body = terms |> List.tryItem 1

  AWhileTerm (cond, body, ref None, syn)

let astBin (syn: SyntaxNode) =
  assert (synToKind syn = BinNode)

  let asBin (t: TokenData) = aBinFromToken t.Token

  let bin =
    syn
    |> synToFirstToken (aBinFromToken >> Option.isSome)
    |> Option.bind asBin

  let terms =
    syn
    |> synToFilterNode nodeIsTerm
    |> List.map astTerm

  let first = terms |> List.tryItem 0
  let second = terms |> List.tryItem 1

  ABinTerm (bin, first, second, ref None, syn)

let astTerm (syn: SyntaxNode) =
  assert (syn |> synToKind |> nodeIsTerm)

  match synToKind syn with
  | BoolLiteralNode ->
    astBoolLiteral syn

  | IntLiteralNode ->
    astIntLiteral syn

  | StrLiteralNode ->
    astStrLiteral syn

  | NameNode ->
    astName syn |> ANameTerm

  | GroupNode ->
    astGroup syn

  | BlockNode ->
    astBlock syn

  | BreakNode ->
    astBreak syn

  | ContinueNode ->
    astContinue syn

  | LoopNode ->
    astLoop syn

  | CallNode ->
    astCall syn

  | BinNode ->
    astBin syn

  | IfNode ->
    astIf syn

  | WhileNode ->
    astWhile syn

  | _ ->
    failwith "NEVER: nodeIsTerm bug"

let astStmt (syn: SyntaxNode) =
  assert (syn |> synToKind |> nodeIsStmt)

  match synToKind syn with
  | ExprNode ->
    let term =
      syn
      |> synToFirstNode nodeIsTerm
      |> Option.map astTerm

    ATermStmt (term, syn)

  | LetNode ->
    let first =
      syn
      |> synToFirstNode ((=) ParamNode)
      |> Option.map astParam

    let second =
      syn
      |> synToFirstNode ((=) ArgNode)
      |> Option.map astArg

    ALetStmt (first, second, syn)

  | ExternFnNode ->
    let name =
      syn
      |> synToFirstNode ((=) NameNode)
      |> Option.map astName

    let args =
      syn
      |> synToFilterNode ((=) ParamNode)
      |> List.map astParam

    let result =
      syn
      |> synToFirstNode ((=) ResultNode)
      |> Option.map astResult

    AExternFnStmt (name, args, result, ref None, syn)

  | FnNode ->
    let name =
      syn
      |> synToFirstNode ((=) NameNode)
      |> Option.map astName

    let args =
      syn
      |> synToFilterNode ((=) ParamNode)
      |> List.map astParam

    let result =
      syn
      |> synToFirstNode ((=) ResultNode)
      |> Option.map astResult

    let body =
      syn
      |> synToFirstNode ((=) BlockNode)
      |> Option.map astTerm

    AFnStmt (name, args, result, body, ref None, syn)

  | StructNode ->
    let name =
      syn
      |> synToFirstNode ((=) NameNode)
      |> Option.map astName

    let fields =
      syn
      |> synToFilterNode ((=) ParamNode)
      |> List.map astParam

    AStructStmt (name, fields, syn)

  | SemiNode ->
    let stmts =
      syn
      |> synToFilterNode nodeIsStmt
      |> List.map astStmt

    ASemiStmt (stmts, syn)

  | _ ->
    failwith "NEVER: nodeIsStmt bug"

let astRoot (node: NodeData) =
  assert (node.Node |> nodeIsRoot)

  node |> synFromNode |> astStmt
