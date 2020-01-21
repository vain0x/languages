module rec PhancieLang.AstGen

open PhancieLang.Ast
open PhancieLang.Helpers
open PhancieLang.Syntax

let astName (node: NodeData) =
  assert (node.Node = NameNode)

  let ident =
    node
    |> nodeToFirstToken ((=) IdentToken)
    |> Option.map tokenToText

  AName (ident, ref None, node)

let astTy (node: NodeData) =
  assert (node.Node = TyNode)

  let name =
    node
    |> nodeToFirstNode ((=) NameNode)
    |> Option.map astName

  ATy (name, node)

let astParam (node: NodeData) =
  assert (node.Node = ParamNode)

  let mode =
    node
    |> nodeToFirstToken (tokenAsMode >> Option.isSome)
    |> Option.bind (fun token -> tokenAsMode token.Token)
    |> Option.defaultValue ValMode

  let name =
    node
    |> nodeToFirstNode ((=) NameNode)
    |> Option.map astName

  let ty =
    node
    |> nodeToFirstNode ((=) TyNode)
    |> Option.map astTy

  AParam (mode, name, ty, node)

let astResult (node: NodeData) =
  assert (node.Node = ResultNode)

  let ty =
    node
    |> nodeToFirstNode ((=) TyNode)
    |> Option.map astTy

  AResult (ty, node)

let astArg (node: NodeData) =
  assert (node.Node = ArgNode)

  let passBy =
    node
    |> nodeToFirstToken (tokenAsPassBy >> Option.isSome)
    |> Option.bind (fun token -> tokenAsPassBy token.Token)
    |> Option.defaultValue ByIn

  let term =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map astTerm

  AArg (passBy, term, node)

let astBoolLiteral (node: NodeData) =
  assert (node.Node = BoolLiteralNode)

  let value =
    node
    |> nodeToFirstToken (fun token -> token = FalseToken || token = TrueToken)
    |> Option.map (fun token -> token.Token = TrueToken)
    |> Option.defaultValue false

  ABoolLiteral (value, node)

let astIntLiteral (node: NodeData) =
  assert (node.Node = IntLiteralNode)

  let intToken =
    node
    |> nodeToFirstToken ((=) IntToken)
    |> Option.map tokenToText

  AIntLiteral (intToken, node)

let astStrLiteral (node: NodeData) =
  assert (node.Node = StrLiteralNode)

  let segments =
    node
    |> nodeToFilterToken ((=) StrVerbatimToken)
    |> List.map (tokenToText >> StrVerbatim)

  AStrLiteral (segments, node)

let astGroup (node: NodeData) =
  assert (node.Node = GroupNode)

  let item =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map astTerm

  AGroupTerm (item, node)

let astBlock (node: NodeData) =
  assert (node.Node = BlockNode)

  let item =
    node
    |> nodeToFilterNode nodeIsStmt
    |> List.map astStmt

  ABlockTerm (item, node)

let astBreak (node: NodeData) =
  assert (node.Node = BreakNode)

  ABreakTerm (ref None, node)

let astContinue (node: NodeData) =
  assert (node.Node = ContinueNode)

  AContinueTerm (ref None, node)

let astLoop (node: NodeData) =
  assert (node.Node = LoopNode)

  let body =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map astTerm

  ALoopTerm (body, ref None, node)

let astCall (node: NodeData) =
  assert (node.Node = CallNode)

  let cal =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map astTerm

  let args =
    node
    |> nodeToFilterNode ((=) ArgNode)
    |> List.map astArg

  ACallTerm (cal, args, node)

let astIf (node: NodeData) =
  assert (node.Node = IfNode)

  let cond =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map astTerm

  let body =
    node
    |> nodeToFirstNode ((=) ThenNode)
    |> Option.bind (nodeToFirstNode nodeIsTerm)
    |> Option.map astTerm

  let alt =
    node
    |> nodeToFirstNode ((=) ElseNode)
    |> Option.bind (nodeToFirstNode nodeIsTerm)
    |> Option.map astTerm

  AIfTerm (cond, body, alt, ref None, node)

let astWhile (node: NodeData) =
  assert (node.Node = WhileNode)

  let terms =
    node
    |> nodeToFilterNode nodeIsTerm
    |> List.map astTerm

  // while キーワードの左と右からそれぞれ探す方がいい。
  let cond = terms |> List.tryItem 0
  let body = terms |> List.tryItem 1

  AWhileTerm (cond, body, ref None, node)

let astBin (node: NodeData) =
  assert (node.Node = BinNode)

  let asBin (t: TokenData) = aBinFromToken t.Token

  let bin =
    node
    |> nodeToFirstToken (aBinFromToken >> Option.isSome)
    |> Option.bind asBin

  let terms =
    node
    |> nodeToFilterNode nodeIsTerm
    |> List.map astTerm

  let first = terms |> List.tryItem 0
  let second = terms |> List.tryItem 1

  ABinTerm (bin, first, second, ref None, node)

let astTerm (node: NodeData) =
  assert (node.Node |> nodeIsTerm)

  match node.Node with
  | BoolLiteralNode ->
    astBoolLiteral node

  | IntLiteralNode ->
    astIntLiteral node

  | StrLiteralNode ->
    astStrLiteral node

  | NameNode ->
    astName node |> ANameTerm

  | GroupNode ->
    astGroup node

  | BlockNode ->
    astBlock node

  | BreakNode ->
    astBreak node

  | ContinueNode ->
    astContinue node

  | LoopNode ->
    astLoop node

  | CallNode ->
    astCall node

  | BinNode ->
    astBin node

  | IfNode ->
    astIf node

  | WhileNode ->
    astWhile node

  | _ ->
    failwith "NEVER: nodeIsTerm bug"

let astStmt (node: NodeData) =
  assert (node.Node |> nodeIsStmt)

  match node.Node with
  | ExprNode ->
    let term =
      node
      |> nodeToFirstNode nodeIsTerm
      |> Option.map astTerm

    ATermStmt (term, node)

  | LetNode ->
    let first =
      node
      |> nodeToFirstNode ((=) ParamNode)
      |> Option.map astParam

    let second =
      node
      |> nodeToFirstNode ((=) ArgNode)
      |> Option.map astArg

    ALetStmt (first, second, node)

  | ExternFnNode ->
    let name =
      node
      |> nodeToFirstNode ((=) NameNode)
      |> Option.map astName

    let args =
      node
      |> nodeToFilterNode ((=) ParamNode)
      |> List.map astParam

    let result =
      node
      |> nodeToFirstNode ((=) ResultNode)
      |> Option.map astResult

    AExternFnStmt (name, args, result, ref None, node)

  | FnNode ->
    let name =
      node
      |> nodeToFirstNode ((=) NameNode)
      |> Option.map astName

    let args =
      node
      |> nodeToFilterNode ((=) ParamNode)
      |> List.map astParam

    let result =
      node
      |> nodeToFirstNode ((=) ResultNode)
      |> Option.map astResult

    let body =
      node
      |> nodeToFirstNode ((=) BlockNode)
      |> Option.map astTerm

    AFnStmt (name, args, result, body, ref None, node)

  | StructNode ->
    let name =
      node
      |> nodeToFirstNode ((=) NameNode)
      |> Option.map astName

    let fields =
      node
      |> nodeToFilterNode ((=) ParamNode)
      |> List.map astParam

    AStructStmt (name, fields, node)

  | SemiNode ->
    let stmts =
      node
      |> nodeToFilterNode nodeIsStmt
      |> List.map astStmt

    ASemiStmt (stmts, node)

  | _ ->
    failwith "NEVER: nodeIsStmt bug"

let astRoot (node: NodeData) =
  assert (node.Node = SemiNode)

  astStmt node
