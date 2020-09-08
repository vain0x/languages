use crate::{
    ast::a_tree::AFnExpr,
    ast::a_tree::{ADecl, AExpr, Ast},
    semantics::prim::Prim,
    semantics::symbol::NSymbol,
    syntax::syntax_token::SyntaxToken,
};
use std::{collections::HashMap, fmt::Write, mem::replace, mem::take};

#[derive(Copy, Clone)]
pub(crate) enum EValue<'a> {
    Bool(bool),
    Int(i64),
    Fn(&'a AFnExpr<'a>),
    Prim(Prim),
}

pub(crate) type EResult<'a> = Result<EValue<'a>, String>;

pub(crate) struct Evaluator<'a> {
    static_vars: HashMap<usize, EValue<'a>>,
    params: Vec<EValue<'a>>,
    local_vars: HashMap<usize, EValue<'a>>,
    output: String,
    ast: &'a Ast<'a>,
}

impl<'a> Evaluator<'a> {
    pub(crate) fn new(ast: &'a Ast<'a>) -> Self {
        Self {
            static_vars: HashMap::new(),
            params: vec![],
            local_vars: HashMap::new(),
            output: String::new(),
            ast,
        }
    }

    fn assign_value_opt(&mut self, name_opt: Option<SyntaxToken<'a>>, value: EValue<'a>) {
        if let Some(symbol) = name_opt.map(|token| self.ast.name_res[&token.index]) {
            match symbol {
                NSymbol::Missing
                | NSymbol::Prim(_)
                | NSymbol::PrimTy(..)
                | NSymbol::Param { .. } => unreachable!(),
                NSymbol::StaticVar { id, .. } => {
                    self.static_vars.insert(id, value);
                }
                NSymbol::LocalVar { id, .. } => {
                    self.local_vars.insert(id, value);
                }
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
            AExpr::Var(token) => match self.ast.name_res[&token.index] {
                NSymbol::Missing => return Err(format!("unknown var {}", token.text)),
                NSymbol::Prim(prim) => EValue::Prim(prim),
                NSymbol::PrimTy(..) => unreachable!(),
                NSymbol::StaticVar { id, .. } => self.static_vars[&id],
                NSymbol::Param { index, .. } => self.params[index],
                NSymbol::LocalVar { id, .. } => self.local_vars[&id],
            },
            AExpr::Call(expr) => match self.on_expr(&*expr.callee)? {
                EValue::Bool(_) => return Err("can't call bool".into()),
                EValue::Int(_) => return Err("can't call int".into()),
                EValue::Fn(callee) => {
                    let args = expr
                        .args
                        .iter()
                        .map(|expr| self.on_expr(expr))
                        .collect::<Result<Vec<_>, _>>()?;
                    if args.len() != callee.params.len() {
                        return Err("arity mismatch".into());
                    }

                    let parent_params = replace(&mut self.params, args);
                    let parent_local_vars = take(&mut self.local_vars);

                    let result = match &callee.body_opt {
                        Some(body) => self.on_expr(&*body)?,
                        None => return Err("body missing".into()),
                    };

                    self.params = parent_params;
                    self.local_vars = parent_local_vars;

                    result
                }
                EValue::Prim(..) => todo!(),
            },
            AExpr::Block(decls) => {
                let (decls, last_opt) = match decls.split_last() {
                    Some((ADecl::Expr(last), decls)) => (decls, Some(last)),
                    _ => (decls.as_slice(), None),
                };

                for decl in decls {
                    self.on_decl(decl)?;
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
            AExpr::Fn(value) => EValue::Fn(value),
        };
        Ok(value)
    }

    fn on_decl(&mut self, decl: &'a ADecl<'a>) -> Result<(), String> {
        match decl {
            ADecl::Expr(expr) => {
                let _value = self.on_expr(expr)?;
            }
            ADecl::Let(decl) => {
                let value = match &decl.init_opt {
                    Some(init) => self.on_expr(init)?,
                    None => return Err("missing init expression".into()),
                };

                self.assign_value_opt(decl.name_opt, value);
            }
        }
        Ok(())
    }

    fn on_root(&mut self) {
        for decl in &self.ast.root.decls {
            match decl {
                ADecl::Expr(expr) => {
                    let name = "it";
                    let result = self.on_expr(expr);

                    self.emit_val(name, &result);
                    if let Ok(value) = result {
                        self.assign_value_opt(None, value);
                    }
                }
                ADecl::Let(decl) => {
                    let name = match decl.name_opt {
                        Some(name) => name.text,
                        None => "it",
                    };

                    let result = match &decl.init_opt {
                        Some(init) => self.on_expr(init),
                        None => Err("missing init expression".into()),
                    };

                    self.emit_val(name, &result);
                    if let Ok(value) = result {
                        self.assign_value_opt(decl.name_opt, value);
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
            EValue::Bool(value) => {
                writeln!(&mut self.output, "val {} : bool = {};", name, value).unwrap();
            }
            EValue::Int(value) => {
                writeln!(&mut self.output, "val {} : number = {};", name, value).unwrap();
            }
            EValue::Fn(_) => {
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
