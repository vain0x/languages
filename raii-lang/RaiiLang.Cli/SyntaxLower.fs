module rec RaiiLang.SyntaxLower

open RaiiLang.Helpers
open RaiiLang.Syntax

let lowerArg (node: NodeData) =
  assert (node.Node = ArgNode)

  let passBy =
    node
    |> nodeToFirstToken (tokenAsPassBy >> Option.isSome)
    |> Option.bind (fun token -> tokenAsPassBy token.Token)
    |> Option.defaultValue ByIn

  let term =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map lowerTerm

  AArg (passBy, term, node)

let lowerParam (node: NodeData) =
  assert (node.Node = ParamNode)

  let mode =
    node
    |> nodeToFirstToken (tokenAsMode >> Option.isSome)
    |> Option.bind (fun token -> tokenAsMode token.Token)
    |> Option.defaultValue ValMode

  let name =
    node
    |> nodeToFirstNode ((=) NameNode)
    |> Option.map lowerName

  let ty =
    node
    |> nodeToFirstNode ((=) TyNode)
    |> Option.map lowerTy

  AParam (mode, name, ty, node)

let lowerResult (node: NodeData) =
  assert (node.Node = ResultNode)

  let ty =
    node
    |> nodeToFirstNode ((=) TyNode)
    |> Option.map lowerTy

  AResult (ty, node)

let lowerTy (node: NodeData) =
  assert (node.Node = TyNode)

  let name =
    node
    |> nodeToFirstNode ((=) NameNode)
    |> Option.bind (nodeToFirstToken ((=) IdentToken))
    |> Option.map tokenToText

  ATy (name, node)

let lowerBoolLiteral (node: NodeData) =
  assert (node.Node = BoolLiteralNode)

  let value =
    node
    |> nodeToFirstToken (fun token -> token = FalseToken || token = TrueToken)
    |> Option.map (fun token -> token.Token = TrueToken)
    |> Option.defaultValue false

  ABoolLiteral (value, node)

let lowerIntLiteral (node: NodeData) =
  assert (node.Node = IntLiteralNode)

  let intToken =
    node
    |> nodeToFirstToken ((=) IntToken)
    |> Option.map tokenToText

  AIntLiteral (intToken, node)

let lowerStrLiteral (node: NodeData) =
  assert (node.Node = StrLiteralNode)

  let segments =
    node
    |> nodeToFilterToken ((=) StrVerbatimToken)
    |> List.map (tokenToText >> StrVerbatim)

  AStrLiteral (segments, node)

let lowerName (node: NodeData) =
  assert (node.Node = NameNode)

  let ident =
    node
    |> nodeToFirstToken ((=) IdentToken)
    |> Option.map tokenToText

  AName (ident, node)

let lowerGroup (node: NodeData) =
  assert (node.Node = GroupNode)

  let item =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map lowerTerm

  AGroupTerm (item, node)

let lowerBlock (node: NodeData) =
  assert (node.Node = BlockNode)

  let item =
    node
    |> nodeToFilterNode nodeIsStmt
    |> List.map lowerStmt

  ABlockTerm (item, node)

let lowerBreak (node: NodeData) =
  assert (node.Node = BreakNode)

  ABreakTerm node

let lowerContinue (node: NodeData) =
  assert (node.Node = ContinueNode)

  AContinueTerm node

let lowerLoop (node: NodeData) =
  assert (node.Node = LoopNode)

  let body =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map lowerTerm

  ALoopTerm (body, node)

let lowerCall (node: NodeData) =
  assert (node.Node = CallNode)

  let cal =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map lowerTerm

  let args =
    node
    |> nodeToFilterNode ((=) ArgNode)
    |> List.map lowerArg

  ACallTerm (cal, args, node)

let lowerIf (node: NodeData) =
  assert (node.Node = IfNode)

  let cond =
    node
    |> nodeToFirstNode nodeIsTerm
    |> Option.map lowerTerm

  let body =
    node
    |> nodeToFirstNode ((=) ThenNode)
    |> Option.bind (nodeToFirstNode nodeIsTerm)
    |> Option.map lowerTerm

  let alt =
    node
    |> nodeToFirstNode ((=) ElseNode)
    |> Option.bind (nodeToFirstNode nodeIsTerm)
    |> Option.map lowerTerm

  AIfTerm (cond, body, alt, node)

let lowerWhile (node: NodeData) =
  assert (node.Node = WhileNode)

  let terms =
    node
    |> nodeToFilterNode nodeIsTerm
    |> List.map lowerTerm

  // while キーワードの左と右からそれぞれ探す方がいい。
  let cond = terms |> List.tryItem 0
  let body = terms |> List.tryItem 1

  AWhileTerm (cond, body, node)

let lowerBin (node: NodeData) =
  assert (node.Node = BinNode)

  let asBin (t: TokenData) = tokenAsBin t.Token

  let bin =
    node
    |> nodeToFirstToken (tokenAsBin >> Option.isSome)
    |> Option.bind asBin

  let terms =
    node
    |> nodeToFilterNode nodeIsTerm
    |> List.map lowerTerm

  let first = terms |> List.tryItem 0
  let second = terms |> List.tryItem 1

  ABinTerm (bin, first, second, node)

let lowerTerm (node: NodeData) =
  assert (node.Node |> nodeIsTerm)

  match node.Node with
  | BoolLiteralNode ->
    lowerBoolLiteral node

  | IntLiteralNode ->
    lowerIntLiteral node

  | StrLiteralNode ->
    lowerStrLiteral node

  | NameNode ->
    lowerName node |> ANameTerm

  | GroupNode ->
    lowerGroup node

  | BlockNode ->
    lowerBlock node

  | BreakNode ->
    lowerBreak node

  | ContinueNode ->
    lowerContinue node

  | LoopNode ->
    lowerLoop node

  | CallNode ->
    lowerCall node

  | BinNode ->
    lowerBin node

  | IfNode ->
    lowerIf node

  | WhileNode ->
    lowerWhile node

  | _ ->
    failwith "NEVER: nodeIsTerm bug"

let lowerStmt (node: NodeData) =
  assert (node.Node |> nodeIsStmt)

  match node.Node with
  | ExprNode ->
    let term =
      node
      |> nodeToFirstNode nodeIsTerm
      |> Option.map lowerTerm

    ATermStmt (term, node)

  | LetNode ->
    let first =
      node
      |> nodeToFirstNode ((=) ParamNode)
      |> Option.map lowerParam

    let second =
      node
      |> nodeToFirstNode ((=) ArgNode)
      |> Option.map lowerArg

    ALetStmt (first, second, node)

  | ExternFnNode ->
    let name =
      node
      |> nodeToFirstNode ((=) NameNode)
      |> Option.map lowerName

    let args =
      node
      |> nodeToFilterNode ((=) ParamNode)
      |> List.map lowerParam

    let result =
      node
      |> nodeToFirstNode ((=) ResultNode)
      |> Option.map lowerResult

    AExternFnStmt (name, args, result, node)

  | FnNode ->
    let name =
      node
      |> nodeToFirstNode ((=) NameNode)
      |> Option.map lowerName

    let args =
      node
      |> nodeToFilterNode ((=) ParamNode)
      |> List.map lowerParam

    let result =
      node
      |> nodeToFirstNode ((=) ResultNode)
      |> Option.map lowerResult

    let body =
      node
      |> nodeToFirstNode ((=) BlockNode)
      |> Option.map lowerTerm

    AFnStmt (name, args, result, body, node)

  | SemiNode ->
    let stmts =
      node
      |> nodeToFilterNode nodeIsStmt
      |> List.map lowerStmt

    ASemiStmt (stmts, node)

  | _ ->
    failwith "NEVER: nodeIsStmt bug"

let lower (node: NodeData) =
  assert (node.Node = SemiNode)

  lowerStmt node
