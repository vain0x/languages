use crate::{
    ast::a_tree::{ADecl, AExpr, ATy, Ast},
    context::Context,
    semantics::{prim::Prim, symbol::NSymbol, ty::Ty},
    syntax::syntax_token::SyntaxToken,
    utils::*,
};
use std::collections::HashMap;

pub(crate) type TResult<T> = Result<T, String>;

#[derive(Default)]
pub(crate) struct TypeChecker<'a> {
    static_vars: HashMap<usize, Ty<'a>>,
    params: HashMap<usize, Ty<'a>>,
    local_vars: HashMap<usize, Ty<'a>>,
    errors: Vec<String>,
    context_opt: Option<&'a Context>,
    ast_opt: Option<&'a Ast<'a>>,
}

impl<'a> TypeChecker<'a> {
    fn assign_value_opt(&mut self, name_opt: Option<SyntaxToken<'a>>, ty: Ty<'a>) {
        let token = match name_opt {
            Some(token) => token,
            None => return,
        };

        let symbol = self.ast_opt.unwrap().name_res[&token.index];

        match symbol {
            NSymbol::Missing | NSymbol::Prim(_) | NSymbol::PrimTy(..) => unreachable!(),
            NSymbol::StaticVar { id, .. } => {
                self.errors
                    .push(format!("static {} : {:?}", token.text, &ty));
                self.static_vars.insert(id, ty);
            }
            NSymbol::Param { id, .. } => {
                self.errors
                    .push(format!("param {} : {:?}", token.text, &ty));
                self.params.insert(id, ty);
            }
            NSymbol::LocalVar { id, .. } => {
                self.errors.push(format!("val {} : {:?}", token.text, &ty));
                self.local_vars.insert(id, ty);
            }
        }
    }

    fn bump(&self) -> &'a bumpalo::Bump {
        &self.context_opt.unwrap().bump
    }

    fn on_ty(&mut self, ty: &'a ATy<'a>) -> TResult<Ty<'a>> {
        let ty = match ty {
            ATy::Name(token) => match self.ast_opt.unwrap().name_res[&token.index] {
                NSymbol::Missing => return Err(format!("unknown type {}", token.text)),
                NSymbol::Prim(..)
                | NSymbol::StaticVar { .. }
                | NSymbol::Param { .. }
                | NSymbol::LocalVar { .. } => unreachable!(),
                NSymbol::PrimTy(prim_ty) => prim_ty.to_ty(),
            },
            ATy::Fn(ty) => {
                let param_tys = ty
                    .param_tys
                    .iter()
                    .map(|ty| self.on_ty(ty))
                    .collect::<TResult<Vec<_>>>()?;
                let result_ty = match ty.result_ty_opt {
                    Some(ty) => self.on_ty(ty)?,
                    None => Ty::Unit,
                };
                Ty::Fn {
                    params: self.bump().alloc_slice_copy(&param_tys),
                    result: self.bump().alloc(result_ty),
                }
            }
        };
        Ok(ty)
    }

    fn on_expr(&mut self, expr: &'a AExpr<'a>) -> TResult<Ty<'a>> {
        let ty = match expr {
            AExpr::False(..) | AExpr::True(..) => Ty::Bool,
            AExpr::Number(..) => Ty::Int,
            AExpr::Var(token) => match self.ast_opt.unwrap().name_res[&token.index] {
                NSymbol::Missing => return Err(format!("unknown var {}", token.text)),
                NSymbol::Prim(prim) => match prim {
                    Prim::IntEq => Ty::Fn {
                        params: { self.bump().alloc_slice_copy(&[Ty::Int; 2]) },
                        result: self.bump().alloc(Ty::Bool),
                    },
                    Prim::IntAdd => Ty::Fn {
                        params: { self.bump().alloc_slice_copy(&[Ty::Int; 2]) },
                        result: self.bump().alloc(Ty::Int),
                    },
                },
                NSymbol::PrimTy(..) => unreachable!(),
                NSymbol::StaticVar { id, .. } => self.static_vars[&id],
                NSymbol::Param { id, .. } => self.params[&id],
                NSymbol::LocalVar { id, .. } => self.local_vars[&id],
            },
            AExpr::Call(expr) => match self.on_expr(&*expr.callee)? {
                Ty::Unit => return Err("can't call unit".into()),
                Ty::Bool => return Err("can't call bool".into()),
                Ty::Int => return Err("can't call int".into()),
                Ty::Fn { params, result } => {
                    if expr.args.len() != params.len() {
                        return Err("arity mismatch".into());
                    }

                    for (arg, param) in expr.args.iter().zip(params) {
                        let arg = self.on_expr(arg)?;
                        if arg != *param {
                            return Err("type mismatch".into());
                        }
                    }
                    *result
                }
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
                    None => Ty::Bool, // unit?
                };
                last
            }
            AExpr::If(expr) => {
                let cond = match &expr.cond_opt {
                    Some(it) => it,
                    None => return Err("cond missing".into()),
                };

                if self.on_expr(cond)? != Ty::Bool {
                    return Err("non-bool condition".into());
                }

                let body = match &expr.body_opt {
                    Some(body) => self.on_expr(body)?,
                    None => Ty::Unit,
                };
                let alt = match &expr.alt_opt {
                    Some(alt) => self.on_expr(alt)?,
                    None => Ty::Unit,
                };

                if body != alt {
                    return Err("inconsistent if type".into());
                }
                body
            }
            AExpr::Fn(expr) => {
                let mut escaping_symbols = self
                    .ast_opt
                    .unwrap()
                    .fn_escapes
                    .get(&expr.id)
                    .into_iter()
                    .flatten()
                    .map(|symbol| format!("{:?}", symbol))
                    .collect::<Vec<_>>();
                if !escaping_symbols.is_empty() {
                    escaping_symbols.sort();
                    self.errors
                        .push(format!("escaping: {}", escaping_symbols.join(", ")));
                }

                let mut param_tys = BumpaloVec::new_in(self.bump());
                for (token, ty_opt) in &expr.params {
                    let ty = ty_opt
                        .as_ref()
                        .ok_or_else(|| "missing type ascription".to_string())?;
                    let ty = self.on_ty(&ty)?;
                    param_tys.push(ty);

                    self.assign_value_opt(Some(*token), ty);
                }

                let body = expr
                    .body_opt
                    .as_ref()
                    .ok_or_else(|| "function body missing".to_string())?;
                let result_ty = self.on_expr(body)?;

                Ty::Fn {
                    params: self.bump().alloc_slice_fill_iter(param_tys),
                    result: self.bump().alloc(result_ty),
                }
            }
        };
        Ok(ty)
    }

    fn on_decl(&mut self, decl: &'a ADecl<'a>) -> Result<(), String> {
        match decl {
            ADecl::Expr(expr) => {
                self.on_expr(expr)?;
            }
            ADecl::Let(decl) => {
                let ty = match &decl.init_opt {
                    Some(init) => self.on_expr(init)?,
                    None => return Err("missing init expression".into()),
                };

                self.assign_value_opt(decl.name_opt, ty);
            }
        }
        Ok(())
    }

    fn on_root(&mut self) -> Result<(), String> {
        for decl in &self.ast_opt.unwrap().root.decls {
            self.on_decl(decl)?;
        }
        Ok(())
    }
}

pub(crate) fn type_check<'a>(context: &'a Context, ast: &'a Ast<'a>) -> Vec<String> {
    let mut type_checker = TypeChecker::default();
    type_checker.ast_opt = Some(ast);
    type_checker.context_opt = Some(context);
    type_checker.on_root().unwrap();
    type_checker.errors
}
