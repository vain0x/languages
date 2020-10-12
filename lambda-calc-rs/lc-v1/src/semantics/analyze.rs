use std::mem::take;

use crate::{
    ast::a_tree::{AExpr, AStmt, ATy, Ast},
    eval::{code_gen::*, type_check::*},
    semantics::{symbol::*, ty::*},
    syntax::syntax_token::SyntaxToken,
    utils::*,
};

pub(crate) type AfterRval<'a> = (GTerm<'a>, Ty<'a>);

#[derive(Default)]
struct Analyzer<'a> {
    // write:
    type_checker: TypeChecker<'a>,
    errors: Vec<String>,
    cg_opt: Option<&'a CodeGenerator<'a>>,

    // read:
    ast_opt: Option<&'a Ast<'a>>,
    bump_opt: Option<&'a bumpalo::Bump>,
}

impl<'a> Analyzer<'a> {
    pub(crate) fn init(&mut self, ast: &'a Ast<'a>, bump: &'a bumpalo::Bump) {
        self.ast_opt = Some(ast);
        self.bump_opt = Some(bump);

        self.type_checker.init(ast, bump);
        self.cg_opt = {
            let cg = bump.alloc(CodeGenerator::default());
            cg.init(ast);
            Some(cg)
        };
    }

    #[allow(unused)]
    fn ast(&self) -> &'a Ast<'a> {
        self.ast_opt.unwrap()
    }

    fn bump(&self) -> &'a bumpalo::Bump {
        self.bump_opt.unwrap()
    }

    fn cg(&self) -> &'a CodeGenerator<'a> {
        self.cg_opt.unwrap()
    }

    fn assign_value_opt(&mut self, name_opt: Option<SyntaxToken<'a>>, ty: Ty<'a>) {
        self.type_checker
            .assign_value_opt(name_opt, ty, &mut self.errors);
    }

    fn do_in_branch(
        &mut self,
        cond_term: GTerm<'a>,
        gen_arms: impl FnOnce(&mut Self, &mut BranchState) -> TResult<()>,
    ) -> TResult<GTerm<'a>> {
        let mut branch_state = self.cg().before_branch(cond_term, 2);

        gen_arms(self, &mut branch_state)?;

        let term = self.cg().after_branch(branch_state);
        Ok(term)
    }

    fn do_in_arm<'s, 'br>(
        &'s mut self,
        branch_state: &'br mut BranchState,
        gen_arm: impl FnOnce(&mut Self, &mut ArmState<'br>) -> TResult<AfterRval<'a>> + 's,
    ) -> TResult<()> {
        let mut arm_state = self.cg().before_arm(branch_state);

        let (arm_term, _arm_ty) = gen_arm(self, &mut arm_state)?;
        self.cg().after_arm(arm_term, arm_state);
        Ok(())
    }

    fn on_ty(&mut self, ty: &'a ATy<'a>) -> TResult<Ty<'a>> {
        let ty = match ty {
            ATy::Name(token) => match self.ast().name_res[&token.index] {
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
                Ty::Fn(FnTy {
                    params: self.bump().alloc_slice_copy(&param_tys),
                    result: self.bump().alloc(result_ty),
                })
            }
        };
        Ok(ty)
    }

    fn on_expr(&mut self, expr: &'a AExpr<'a>) -> TResult<AfterRval<'a>> {
        let ty = match expr {
            AExpr::True(..) => (GTerm::Bool(true), Ty::Bool),
            AExpr::False(..) => (GTerm::Bool(false), Ty::Bool),
            AExpr::Number(token) => {
                let value = token
                    .text
                    .parse::<i64>()
                    .map_err(|_| format!("can't parse as int: {:?}", token.text))?;
                (GTerm::Int(value), Ty::Int)
            }
            AExpr::Var(token) => {
                let term = self.cg().eval_var(*token)?;
                let ty = self.type_checker.eval_var(*token)?;
                (term, ty)
            }
            AExpr::Call(expr) => {
                // callable
                let (fn_term, fn_ty) = self.on_expr(&*expr.callee)?;
                let arity = expr.args.len();
                let fn_ty = self.type_checker.expect_callable(fn_ty, arity)?;

                // args
                let arg_terms = expr
                    .args
                    .iter()
                    .zip(fn_ty.params)
                    .map(|(arg, &param_ty)| -> TResult<_> {
                        let (arg_term, arg_ty) = self.on_expr(arg)?;
                        self.type_checker.expect_assignable(param_ty, arg_ty)?;
                        Ok(arg_term)
                    })
                    .collect::<TResult<Vec<_>>>()?;

                let result_term = self.cg().on_call_expr(fn_term, arg_terms);
                (result_term, *fn_ty.result)
            }
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
                    None => (GTerm::Unit, Ty::Unit),
                };
                last
            }
            AExpr::If(expr) => {
                let cond = match &expr.cond_opt {
                    Some(it) => it,
                    None => return Err("cond missing".into()),
                };
                let (cond_term, cond_ty) = self.on_expr(cond)?;
                self.type_checker.expect_cond_ty(cond_ty)?;

                let result_term = self.do_in_branch(cond_term, |an, branch_state| {
                    an.do_in_arm(branch_state, |an, _arm| match &expr.body_opt {
                        Some(body) => an.on_expr(body),
                        None => Ok((GTerm::Unit, Ty::Unit)),
                    })?;
                    an.do_in_arm(branch_state, |an, _arm| match &expr.alt_opt {
                        Some(alt) => an.on_expr(alt),
                        None => Ok((GTerm::Unit, Ty::Unit)),
                    })
                })?;

                // TODO: branch
                let body_ty = Ty::Bool;
                let alt_ty = Ty::Bool;
                let result_ty = self.type_checker.join_ty(body_ty, alt_ty)?;
                (result_term, result_ty)
            }
            AExpr::Fn(expr) => {
                let mut escaping_symbols = self
                    .ast()
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
                    let ty = match ty_opt {
                        Some(ty) => self.on_ty(&ty)?,
                        None => Ty::Todo,
                    };
                    // .ok_or_else(|| "missing type ascription".to_string())?;
                    param_tys.push(ty);

                    self.assign_value_opt(Some(*token), ty);
                }

                let builder = self.cg().before_fn_expr(expr.params.len());

                let body = expr
                    .body_opt
                    .as_ref()
                    .ok_or_else(|| "function body missing".to_string())?;
                let (body_term, body_ty) = self.on_expr(body)?;

                let fn_term = self.cg().after_fn_expr(body_term, builder);

                let fn_ty = Ty::Fn(FnTy {
                    params: self.bump().alloc_slice_fill_iter(param_tys),
                    result: self.bump().alloc(body_ty),
                });
                (fn_term, fn_ty)
            }
        };
        Ok(ty)
    }

    fn on_stmt(&mut self, stmt: &'a AStmt<'a>) -> Result<(), String> {
        match stmt {
            AStmt::Expr(expr) => {
                let (term, _) = self.on_expr(expr)?;
                self.cg().after_expr_stmt(term);
            }
            AStmt::Let(stmt) => {
                let (init_term, init_ty) = match &stmt.init_opt {
                    Some(init) => self.on_expr(init)?,
                    None => return Err("missing init expression".into()),
                };

                self.assign_value_opt(stmt.name_opt, init_ty);
                self.cg().after_let_stmt(stmt.name_opt, init_term);
            }
        }
        Ok(())
    }

    fn on_root(&mut self) -> Result<(), String> {
        self.cg().before_root();

        for stmt in &self.ast().root.stmts {
            self.on_stmt(stmt)?;
        }

        self.cg().after_root();
        Ok(())
    }
}

pub(crate) fn analyze<'a>(ast: &'a Ast<'a>, bump: &'a bumpalo::Bump) -> (Program<'a>, Vec<String>) {
    let mut analyzer = Analyzer::default();
    analyzer.init(ast, bump);
    analyzer.on_root().unwrap();

    let program = take(&mut *analyzer.cg().program.borrow_mut());
    let errors = analyzer.errors;
    (program, errors)
}
