use crate::{
    ast::a_tree::{AExpr, AFnExpr, AStmt, Ast},
    semantics::local_symbol::NLocalSymbol,
    semantics::{prim::Prim, symbol::NSymbol},
    syntax::syntax_token::SyntaxToken,
};
use std::{
    cell::RefCell, collections::HashMap, collections::HashSet, fmt::Write, mem::replace, mem::take,
    rc::Rc,
};

pub(crate) struct EClosureEnv<'a> {
    // upvar_set_opt: Option<&'a HashSet<NLocalSymbol>>,
    parent_opt: Option<Rc<EClosureEnv<'a>>>,
    fields: RefCell<HashMap<NLocalSymbol, EValue<'a>>>,
}

impl<'a> EClosureEnv<'a> {
    fn new(
        parent_opt: Option<Rc<EClosureEnv<'a>>>,
        _expr: &'a AFnExpr<'a>,
        _ast: &'a Ast<'a>,
    ) -> Self {
        Self {
            parent_opt,
            fields: RefCell::default(),
            // upvar_set_opt: ast.fn_escapes.get(&expr.id),
        }
    }
}

#[derive(Clone)]
pub(crate) enum EValue<'a> {
    Uninit,
    Bool(bool),
    Int(i64),
    Fn {
        env_opt: Option<Rc<EClosureEnv<'a>>>,
        expr: &'a AFnExpr<'a>,
    },
    Prim(Prim),
}

impl<'a> EValue<'a> {
    fn take(&mut self) -> EValue<'a> {
        replace(self, EValue::Uninit)
    }
}

pub(crate) type EResult<'a> = Result<EValue<'a>, String>;

pub(crate) struct Evaluator<'a> {
    static_vars: HashMap<usize, EValue<'a>>,
    params: Vec<EValue<'a>>,
    local_vars: HashMap<usize, EValue<'a>>,

    /// 呼び出し中のクロージャの環境
    env_opt: Option<Rc<EClosureEnv<'a>>>,

    /// upvar (エスケープするパラメータやローカル変数) の集合
    upvar_set: HashSet<NLocalSymbol>,

    output: String,
    ast: &'a Ast<'a>,
}

impl<'a> Evaluator<'a> {
    pub(crate) fn new(ast: &'a Ast<'a>) -> Self {
        Self {
            static_vars: HashMap::new(),
            params: vec![],
            local_vars: HashMap::new(),
            env_opt: None,
            output: String::new(),
            ast,
            upvar_set: ast
                .fn_escapes
                .values()
                .flat_map(|set| set.iter().copied())
                .collect::<HashSet<_>>(),
        }
    }

    /// 関数がエスケープする変数を所有しない？
    fn fn_is_static(&self, fn_id: usize) -> bool {
        self.ast
            .fn_escapes
            .get(&fn_id)
            .map_or(true, |set| set.is_empty())
    }

    fn symbol_as_upvar(&self, symbol: NSymbol) -> Option<NLocalSymbol> {
        NLocalSymbol::from_symbol(symbol).filter(|id| self.upvar_set.contains(id))
    }

    fn get_value(&mut self, token: SyntaxToken<'a>) -> EResult<'a> {
        let symbol = self.ast.name_res[&token.index];

        if let Some(symbol) = self.symbol_as_upvar(symbol) {
            let mut env = self.env_opt.as_ref().expect("illegal use of upvar");
            loop {
                if let Some(slot) = env.fields.borrow().get(&symbol) {
                    return Ok(slot.clone());
                }

                match env.parent_opt.as_ref() {
                    Some(parent) => env = parent,
                    None => panic!("use of uninit upvar"),
                }
            }
        }

        let value = match symbol {
            NSymbol::Missing => return Err(format!("unknown var {}", token.text)),
            NSymbol::Prim(prim) => EValue::Prim(prim),
            NSymbol::PrimTy(..) => unreachable!("use PrimTy as value"),
            NSymbol::StaticVar { id, .. } => self.static_vars[&id].clone(),
            NSymbol::Param { index, .. } => self.params[index].clone(),
            NSymbol::LocalVar { id, .. } => self.local_vars[&id].clone(),
        };
        Ok(value)
    }

    fn assign_value_opt(&mut self, name_opt: Option<SyntaxToken<'a>>, value: EValue<'a>) {
        let token = match name_opt {
            Some(it) => it,
            None => return,
        };

        let symbol = self.ast.name_res[&token.index];

        if let Some(symbol) = self.symbol_as_upvar(symbol) {
            let mut env = self.env_opt.as_ref().expect("illegal use of upvar");
            loop {
                if let Some(slot) = env.fields.borrow_mut().get_mut(&symbol) {
                    *slot = value;
                    return;
                }

                match env.parent_opt.as_ref() {
                    Some(parent) => env = parent,
                    None => {
                        break;
                    }
                }
            }

            self.env_opt
                .as_deref()
                .unwrap()
                .fields
                .borrow_mut()
                .insert(symbol, value);
            return;
        }

        match symbol {
            NSymbol::Missing | NSymbol::Prim(_) | NSymbol::PrimTy(..) | NSymbol::Param { .. } => {
                unreachable!()
            }
            NSymbol::StaticVar { id, .. } => {
                self.static_vars.insert(id, value);
            }
            NSymbol::LocalVar { id, .. } => {
                self.local_vars.insert(id, value);
            }
        }
    }

    fn on_expr(&mut self, expr: &'a AExpr<'a>) -> EResult<'a> {
        let value = match expr {
            AExpr::False(..) => EValue::Bool(false),
            AExpr::True(..) => EValue::Bool(true),
            AExpr::Number(token) => {
                let value = match token.text.parse::<i64>() {
                    Ok(it) => it,
                    Err(err) => return Err(format!("invalid number constant {:?}", err)),
                };
                EValue::Int(value)
            }
            AExpr::Var(token) => self.get_value(*token)?,
            AExpr::Call(expr) => match self.on_expr(&*expr.callee)? {
                EValue::Uninit => unreachable!(),
                EValue::Bool(_) => return Err("can't call bool".into()),
                EValue::Int(_) => return Err("can't call int".into()),
                EValue::Fn {
                    expr: callee,
                    env_opt: callee_env_opt,
                } => {
                    let mut args = expr
                        .args
                        .iter()
                        .map(|expr| self.on_expr(expr))
                        .collect::<Result<Vec<_>, _>>()?;
                    if args.len() != callee.params.len() {
                        return Err("arity mismatch".into());
                    }

                    let env_opt = if self.fn_is_static(callee.id) {
                        None
                    } else {
                        let env = EClosureEnv::new(callee_env_opt.clone(), callee, self.ast);
                        {
                            let mut fields = env.fields.borrow_mut();
                            for (i, (token, _)) in callee.params.iter().enumerate() {
                                if let Some(symbol) =
                                    self.symbol_as_upvar(self.ast.name_res[&token.index])
                                {
                                    fields.insert(symbol, args[i].take());
                                }
                            }
                        }
                        Some(Rc::new(env))
                    };

                    let parent_params = replace(&mut self.params, args);
                    let parent_local_vars = take(&mut self.local_vars);
                    let parent_env_opt = replace(&mut self.env_opt, env_opt);

                    let result = match &callee.body_opt {
                        Some(body) => self.on_expr(&*body)?,
                        None => return Err("body missing".into()),
                    };

                    self.params = parent_params;
                    self.local_vars = parent_local_vars;
                    self.env_opt = parent_env_opt;

                    result
                }
                EValue::Prim(prim) => match prim {
                    Prim::IntEq => todo!(),
                    Prim::IntAdd => {
                        let mut args = expr
                            .args
                            .iter()
                            .map(|expr| self.on_expr(expr))
                            .collect::<Result<Vec<_>, _>>()?;

                        match args.as_mut_slice() {
                            [left, right] => match (left.take(), right.take()) {
                                (EValue::Int(left), EValue::Int(right)) => {
                                    EValue::Int(left + right)
                                }
                                _ => return Err("type error".into()),
                            },
                            _ => return Err("arity error".into()),
                        }
                    }
                },
            },
            AExpr::Block(stmts) => {
                let (stmts, last_opt) = match stmts.split_last() {
                    Some((AStmt::Expr(last), stmts)) => (stmts, Some(last)),
                    _ => (stmts.as_slice(), None),
                };

                for stmt in stmts {
                    self.on_stmt(stmt)?;
                }

                let last = match last_opt {
                    Some(last) => self.on_expr(last)?,
                    None => EValue::Bool(false), // unit?
                };
                last
            }
            AExpr::If(expr) => {
                let cond = match &expr.cond_opt {
                    Some(it) => it,
                    None => return Err("cond missing".into()),
                };

                let cond = match self.on_expr(cond)? {
                    EValue::Bool(it) => it,
                    _ => return Err("non-bool condition".into()),
                };

                match (cond, &expr.body_opt, &expr.alt_opt) {
                    (true, Some(body), _) => self.on_expr(body)?,
                    (true, None, _) => return Err("body missing".into()),
                    (false, _, Some(alt)) => self.on_expr(alt)?,
                    (false, _, None) => return Err("alt missing".into()),
                }
            }
            AExpr::Fn(expr) => EValue::Fn {
                env_opt: self.env_opt.clone(),
                expr,
            },
        };
        Ok(value)
    }

    fn on_stmt(&mut self, stmt: &'a AStmt<'a>) -> Result<(), String> {
        match stmt {
            AStmt::Expr(expr) => {
                let _value = self.on_expr(expr)?;
            }
            AStmt::Let(stmt) => {
                let value = match &stmt.init_opt {
                    Some(init) => self.on_expr(init)?,
                    None => return Err("missing init expression".into()),
                };

                self.assign_value_opt(stmt.name_opt, value);
            }
        }
        Ok(())
    }

    fn on_root(&mut self) {
        for stmt in &self.ast.root.stmts {
            match stmt {
                AStmt::Expr(expr) => {
                    let name = "it";
                    let result = self.on_expr(expr);

                    self.emit_val(name, &result);
                    if let Ok(value) = result {
                        self.assign_value_opt(None, value);
                    }
                }
                AStmt::Let(stmt) => {
                    let name = match stmt.name_opt {
                        Some(name) => name.text,
                        None => "it",
                    };

                    let result = match &stmt.init_opt {
                        Some(init) => self.on_expr(init),
                        None => Err("missing init expression".into()),
                    };

                    self.emit_val(name, &result);
                    if let Ok(value) = result {
                        self.assign_value_opt(stmt.name_opt, value);
                    }
                }
            }
        }
    }

    fn emit_val(&mut self, name: &str, result: &EResult<'a>) {
        let value = match result {
            Ok(value) => value,
            Err(err) => {
                writeln!(
                    &mut self.output,
                    "val {} = <err>;\nERROR: \"{}\"",
                    name, err
                )
                .unwrap();
                return;
            }
        };

        match value {
            EValue::Uninit => {
                writeln!(&mut self.output, "val {} : uninit", name).unwrap();
            }
            EValue::Bool(value) => {
                writeln!(&mut self.output, "val {} : bool = {};", name, value).unwrap();
            }
            EValue::Int(value) => {
                writeln!(&mut self.output, "val {} : number = {};", name, value).unwrap();
            }
            EValue::Fn { .. } => {
                writeln!(
                    &mut self.output,
                    "val {} : fn(...) -> ... = <function>;",
                    name
                )
                .unwrap();
            }
            EValue::Prim(prim) => {
                writeln!(
                    &mut self.output,
                    "val {} : fn... = <primitive({:?})>;",
                    name, prim
                )
                .unwrap();
            }
        }
    }
}

pub(crate) fn evaluate<'a>(ast: &'a Ast<'a>) -> String {
    let mut evaluator = Evaluator::new(ast);
    evaluator.on_root();
    evaluator.output
}
