use crate::*;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct VarDef {
    pub name: String,
    pub index: usize,
    pub kind: VarKind,
}

#[derive(Clone, Debug)]
pub enum VarKind {
    Global,
    Local,
    Param(FunId),
}

#[derive(Clone, Default)]
pub struct FunDef {
    pub name: String,
    pub body: ExpId,
    pub params: Vec<VarId>,
    pub locals: Vec<VarId>,
}

#[derive(Clone, Copy, Debug)]
pub enum FunRef {
    Fun(FunId),
    Prim(Prim),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Prim {
    AddInt,
    SubInt,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    AddStr,
    ToStr,
    ReadStr,
    ReadInt,
    Print,
    PrintLn,
    Begin,
    Cond,
    While,
}

#[derive(Clone, Debug)]
pub enum ExpKind {
    Err(String, NodeId),
    None,
    Int(i64),
    Str(String),
    Var(VarId),
    App(FunRef),
    Set(VarId),
}

#[derive(Clone, Debug)]
pub struct Exp {
    pub kind: ExpKind,
    pub children: Vec<ExpId>,
}

pub struct Sema {
    pub syntax: Rc<Syntax>,
    pub vars: Vec<VarDef>,
    pub funs: Vec<FunDef>,
    pub exps: Vec<Exp>,
}

struct SemanticAnalyzer {
    sema: Sema,
    current_fun_id: FunId,
}

const PRIMS: &'static [(&'static str, Prim, PrimArity)] = &[
    ("+", Prim::AddInt, PrimArity::Bin),
    ("-", Prim::SubInt, PrimArity::Bin),
    ("*", Prim::Mul, PrimArity::Bin),
    ("/", Prim::Div, PrimArity::Bin),
    ("%", Prim::Mod, PrimArity::Bin),
    ("==", Prim::Eq, PrimArity::Fixed(2)),
    ("!=", Prim::Ne, PrimArity::Fixed(2)),
    ("<", Prim::Lt, PrimArity::Fixed(2)),
    ("<=", Prim::Le, PrimArity::Fixed(2)),
    (">", Prim::Gt, PrimArity::Fixed(2)),
    (">=", Prim::Ge, PrimArity::Fixed(2)),
    ("++", Prim::AddStr, PrimArity::Bin),
    ("to_str", Prim::ToStr, PrimArity::Fixed(1)),
    ("read_str", Prim::ReadStr, PrimArity::Fixed(0)),
    ("read_int", Prim::ReadInt, PrimArity::Fixed(0)),
    ("print", Prim::Print, PrimArity::Infinite),
    ("println", Prim::PrintLn, PrimArity::Infinite),
    ("begin", Prim::Begin, PrimArity::Infinite),
    ("cond", Prim::Cond, PrimArity::Infinite),
    ("while", Prim::While, PrimArity::Fixed(2)),
];

impl FunDef {
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

trait AddExp {
    fn exps_mut(&mut self) -> &mut Vec<Exp>;

    fn add_exp(&mut self, kind: ExpKind, children: Vec<ExpId>) -> ExpId {
        self.exps_mut().push(Exp { kind, children });
        self.exps_mut().len() - 1
    }

    fn add_term(&mut self, kind: ExpKind) -> ExpId {
        self.add_exp(kind, vec![])
    }

    fn add_err(&mut self, message: String, node_id: NodeId) -> ExpId {
        self.add_exp(ExpKind::Err(message, node_id), vec![])
    }
}

impl AddExp for Sema {
    fn exps_mut(&mut self) -> &mut Vec<Exp> {
        &mut self.exps
    }
}

impl AddExp for SemanticAnalyzer {
    fn exps_mut(&mut self) -> &mut Vec<Exp> {
        &mut self.sema.exps
    }
}

impl Sema {
    fn add_param(&mut self, fun_id: FunId, name: String) {
        let index = self.funs[fun_id].params.len();
        let var_id = self.vars.len();

        self.vars.push(VarDef {
            name,
            index,
            kind: VarKind::Param(fun_id),
        });

        self.funs[fun_id].params.push(var_id);
    }

    fn add_local(&mut self, fun_id: FunId, name: String) -> VarId {
        let kind = if fun_id == GLOBAL_FUN_ID {
            VarKind::Global
        } else {
            VarKind::Local
        };
        let index = self.funs[fun_id].locals.len();
        let var_id = self.vars.len();

        self.vars.push(VarDef { name, index, kind });
        self.funs[fun_id].locals.push(var_id);

        var_id
    }

    fn find_var_id(&self, fun_id: FunId, name: &str) -> Option<VarId> {
        for &fun_id in &[fun_id, GLOBAL_FUN_ID] {
            for &var_id in self.funs[fun_id].locals.iter().rev() {
                if self.vars[var_id].name == name {
                    return Some(var_id);
                }
            }

            for &var_id in self.funs[fun_id].params.iter().rev() {
                if self.vars[var_id].name == name {
                    return Some(var_id);
                }
            }
        }
        None
    }

    fn find_fun_id(&self, name: &str) -> Option<FunId> {
        for fun_id in (0..self.funs.len()).rev() {
            if self.funs[fun_id].name == name {
                return Some(fun_id);
            }
        }
        None
    }
}

impl SemanticAnalyzer {
    fn tokens(&self) -> &[Token] {
        &self.sema.syntax.tokens
    }

    fn nodes(&self) -> &[Node] {
        &self.sema.syntax.nodes
    }

    fn node_as_ident(&self, node_id: NodeId) -> Option<&str> {
        match &self.nodes()[node_id] {
            &Node::Value(token_id) => match &self.tokens()[token_id] {
                Token::Id(name) => Some(name),
                _ => None,
            },
            &Node::Ann(node_id, _) => self.node_as_ident(node_id),
            _ => None,
        }
    }

    fn node_as_app(&self, node_id: NodeId) -> Option<&[NodeId]> {
        match &self.nodes()[node_id] {
            Node::App(nodes) => Some(&nodes),
            _ => None,
        }
    }

    fn find_var_id(&self, name: &str) -> Option<VarId> {
        self.sema.find_var_id(self.current_fun_id, name)
    }

    fn find_fun_id(&self, name: &str) -> Option<FunId> {
        self.sema.find_fun_id(name)
    }

    fn add_param(&mut self, name: String) {
        self.sema.add_param(self.current_fun_id, name);
    }

    fn add_local(&mut self, name: String) -> VarId {
        self.sema.add_local(self.current_fun_id, name)
    }

    fn on_token(&mut self, token_id: TokenId, node_id: NodeId) -> ExpId {
        match &self.tokens()[token_id] {
            Token::Err(err) => self.add_err(err.to_owned(), node_id),
            Token::Id(name) => {
                if let Some(var_id) = self.find_var_id(name) {
                    self.add_term(ExpKind::Var(var_id))
                } else if name == "true" {
                    self.add_term(ExpKind::Int(1))
                } else if name == "false" {
                    self.add_term(ExpKind::Int(0))
                } else {
                    self.add_err("Undefined variable".into(), node_id)
                }
            }
            &Token::Int(value) => self.add_term(ExpKind::Int(value)),
            Token::Str(value) => self.add_term(ExpKind::Str(value.to_owned())),
            Token::Pun(_) | Token::Eof => unreachable!(),
        }
    }

    fn on_children(&mut self, nodes: &[NodeId]) -> Vec<ExpId> {
        let mut children = vec![];
        for i in 0..nodes.len() {
            children.push(self.on_node(nodes[i]));
        }
        children
    }

    fn add_fun(&mut self, name: String, params: Vec<String>, body: NodeId) {
        let previous_fun_id = self.current_fun_id;

        self.sema.funs.push(FunDef {
            name: name,
            ..FunDef::default()
        });
        self.current_fun_id = self.sema.funs.len() - 1;

        for name in params {
            self.add_param(name);
        }

        let body = self.on_node(body);

        self.sema.funs[self.current_fun_id].body = body;

        self.current_fun_id = previous_fun_id;
    }

    fn on_app(&mut self, node_id: NodeId, name: &str, nodes: Vec<NodeId>) -> ExpId {
        if name == "def" {
            if nodes.len() != 2 {
                return self.add_err("Expected 2 args".into(), node_id);
            }
            let sig_nodes = match self.node_as_app(nodes[0]) {
                None => return self.add_err("Expected a list".into(), nodes[0]),
                Some(nodes) if nodes.len() == 0 => {
                    return self.add_err("Expected a non-empty list".into(), nodes[0]);
                }
                Some(nodes) => nodes.to_owned(),
            };
            let name = match self.node_as_ident(sig_nodes[0]) {
                None => return self.add_err("Expected an ident".into(), sig_nodes[0]),
                Some(name) => name.to_owned(),
            };
            let mut params = vec![];
            for i in 1..sig_nodes.len() {
                params.push(match self.node_as_ident(sig_nodes[i]) {
                    None => return self.add_err("Expected an ident".into(), sig_nodes[i]),
                    Some(name) => name.to_owned(),
                });
            }

            self.add_fun(name, params, nodes[1]);

            return self.add_term(ExpKind::None);
        }

        if name == "let" {
            if nodes.len() != 2 {
                return self.add_err("Expected 2 args".into(), node_id);
            }
            let name = match &self.node_as_ident(nodes[0]) {
                None => return self.add_err("Expected an ident".into(), nodes[0]),
                &Some(name) => name.to_owned(),
            };
            let r = self.on_node(nodes[1]);
            let var_id = self.add_local(name);
            return self.add_exp(ExpKind::Set(var_id), vec![r]);
        }

        if name == "set" {
            if nodes.len() != 2 {
                return self.add_err("Expected 2 args".into(), node_id);
            }
            let var_id = match self
                .node_as_ident(nodes[0])
                .and_then(|name| self.find_var_id(name))
            {
                None => return self.add_err("Undefined variable".into(), nodes[0]),
                Some(var_id) => var_id,
            };
            let r = self.on_node(nodes[1]);
            return self.add_exp(ExpKind::Set(var_id), vec![r]);
        }

        for i in 0..PRIMS.len() {
            if name != PRIMS[i].0 {
                continue;
            }

            let fun_ref = FunRef::Prim(PRIMS[i].1);
            match PRIMS[i].2 {
                PrimArity::Bin if nodes.len() == 0 => {
                    return self.add_err("Expected 1+ args".into(), nodes[0]);
                }
                PrimArity::Bin => {
                    let mut l = self.on_node(nodes[0]);
                    for i in 1..nodes.len() {
                        let r = self.on_node(nodes[i]);
                        let t = self.add_exp(ExpKind::App(fun_ref), vec![l, r]);
                        l = t;
                    }
                    return l;
                }
                PrimArity::Fixed(arity) if nodes.len() != arity => {
                    return self.add_err(format!("Expected {} args", arity), node_id);
                }
                PrimArity::Fixed(_) | PrimArity::Infinite => {
                    let children = self.on_children(&nodes);
                    return self.add_exp(ExpKind::App(fun_ref), children);
                }
            }
        }

        let fun_ref = match self.find_fun_id(name) {
            Some(fun_id) => FunRef::Fun(fun_id),
            None => return self.add_err("Undefined function".into(), node_id),
        };

        let children = self.on_children(&nodes);
        self.add_exp(ExpKind::App(fun_ref), children)
    }

    fn on_node(&mut self, node_id: NodeId) -> ExpId {
        match &self.nodes()[node_id] {
            Node::Err(err, _) => self.add_err(err.to_owned(), node_id),
            &Node::Value(token_id) => self.on_token(token_id, node_id),
            Node::App(nodes) if nodes.len() == 0 => {
                self.add_err("Expected a non-empty list".into(), node_id)
            }
            Node::App(nodes) => match &self.nodes()[nodes[0]] {
                &Node::Value(token_id) => match &self.tokens()[token_id] {
                    Token::Id(head) => {
                        self.on_app(nodes[0], &head.to_owned(), nodes[1..].to_owned())
                    }
                    _ => self.add_err("Expected an ident".into(), nodes[0]),
                },
                _ => self.add_err("Expected an ident".into(), nodes[0]),
            },
            &Node::Ann(value_node_id, _) => self.on_node(value_node_id),
        }
    }

    pub fn sema(&mut self) {
        let entry_syn_id = self.nodes().len() - 1;
        self.add_fun("main".to_owned(), vec![], entry_syn_id);
    }
}

pub fn sema(syntax: Rc<Syntax>) -> Sema {
    let mut analyzer = SemanticAnalyzer {
        sema: Sema {
            syntax,
            vars: vec![],
            funs: vec![],
            exps: vec![],
        },
        current_fun_id: GLOBAL_FUN_ID,
    };
    analyzer.sema();
    analyzer.sema
}
