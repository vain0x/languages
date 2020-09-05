use crate::{
    ast::a_tree::AFnExpr,
    ast::a_tree::{ADecl, AExpr, Ast},
};
use std::{collections::HashMap, fmt::Write};

#[derive(Copy, Clone)]
pub(crate) enum EValue<'a> {
    Bool(bool),
    Int(i64),
    Fn(&'a AFnExpr<'a>),
}

pub(crate) type EResult<'a> = Result<EValue<'a>, String>;

pub(crate) struct Evaluator<'a> {
    map_stack: Vec<HashMap<&'a str, EValue<'a>>>,
    output: String,
    ast: &'a Ast<'a>,
}

impl<'a> Evaluator<'a> {
    pub(crate) fn new(ast: &'a Ast<'a>) -> Self {
        Self {
            map_stack: vec![HashMap::new()],
            output: String::new(),
            ast,
        }
    }

    fn find_value(&self, name: &str) -> Option<EValue<'a>> {
        self.map_stack
            .iter()
            .rev()
            .find_map(|map| map.get(name))
            .copied()
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
            AExpr::Var(token) => self
                .find_value(token.text)
                .ok_or_else(|| format!("unknown var {}", token.text))?,
            AExpr::Call(expr) => match self.on_expr(&*expr.callee)? {
                EValue::Bool(_) => return Err("can't call bool".into()),
                EValue::Int(_) => return Err("can't call int".into()),
                EValue::Fn(callee) => {
                    let args = expr.args.iter().map(|expr| self.on_expr(expr));

                    let mut local_map = HashMap::new();
                    for (param, arg) in callee.params.iter().zip(args) {
                        local_map.insert(param.text, arg?);
                    }
                    self.map_stack.push(local_map);
                    let result = match &callee.body_opt {
                        Some(body) => self.on_expr(&*body)?,
                        None => return Err("body missing".into()),
                    };
                    self.map_stack.pop();

                    result
                }
            },
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

    // fn on_decl(&mut self, decl: ADecl<'a>) {
    //     //
    // }

    fn on_root(&mut self) {
        for decl in &self.ast.root.decls {
            match decl {
                ADecl::Expr(expr) => {
                    let name = "it";
                    let result = self.on_expr(expr);

                    self.emit_val(name, &result);
                    if let Ok(value) = result {
                        self.map_stack.last_mut().unwrap().insert(name, value);
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
                        self.map_stack.last_mut().unwrap().insert(name, value);
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
        }
    }
}

pub(crate) fn evaluate<'a>(ast: &'a Ast<'a>) -> String {
    let mut evaluator = Evaluator::new(ast);
    evaluator.on_root();
    evaluator.output
}
