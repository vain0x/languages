use crate::{
    ast::a_tree::*,
    semantics::{prim::Prim, symbol::NSymbol, ty::*},
    syntax::syntax_token::SyntaxToken,
};
use std::collections::HashMap;

pub(crate) type TResult<T> = Result<T, String>;

/// if の結果型を構成するのに使う。
pub(crate) enum Joint<'a> {
    Empty,
    Ty(Ty<'a>),
}

impl<'a> Joint<'a> {
    pub(crate) fn push(&mut self, ty: Ty<'a>, tc: &mut TypeChecker<'a>) -> TResult<()> {
        tc.add_to_joint(self, ty)
    }

    pub(crate) fn into_ty(self, tc: &mut TypeChecker<'a>) -> Ty<'a> {
        tc.consume_joint(self)
    }
}

#[derive(Default)]
pub(crate) struct TypeChecker<'a> {
    static_vars: HashMap<usize, Ty<'a>>,
    params: HashMap<usize, Ty<'a>>,
    local_vars: HashMap<usize, Ty<'a>>,
    ast_opt: Option<&'a Ast<'a>>,
    bump_opt: Option<&'a bumpalo::Bump>,
}

impl<'a> TypeChecker<'a> {
    pub(crate) fn init(&mut self, ast: &'a Ast<'a>, bump: &'a bumpalo::Bump) {
        self.ast_opt = Some(ast);
        self.bump_opt = Some(bump);
    }

    fn ast(&self) -> &'a Ast<'a> {
        self.ast_opt.unwrap()
    }

    fn bump(&self) -> &'a bumpalo::Bump {
        self.bump_opt.unwrap()
    }

    pub(crate) fn assign_value_opt(
        &mut self,
        name_opt: Option<SyntaxToken<'a>>,
        ty: Ty<'a>,
        errors: &mut Vec<String>,
    ) {
        let token = match name_opt {
            Some(token) => token,
            None => return,
        };

        let symbol = self.ast().name_res[&token.index];
        match symbol {
            NSymbol::Missing | NSymbol::Prim(_) | NSymbol::PrimTy(..) => unreachable!(),
            NSymbol::StaticVar { id, .. } => {
                errors.push(format!("static {} : {:?}", token.text, &ty));
                self.static_vars.insert(id, ty);
            }
            NSymbol::Param { id, .. } => {
                errors.push(format!("param {} : {:?}", token.text, &ty));
                self.params.insert(id, ty);
            }
            NSymbol::LocalVar { id, .. } => {
                errors.push(format!("val {} : {:?}", token.text, &ty));
                self.local_vars.insert(id, ty);
            }
        }
    }

    pub(crate) fn eval_var(&mut self, token: SyntaxToken<'a>) -> TResult<Ty<'a>> {
        let ty = match self.ast().name_res[&token.index] {
            NSymbol::Missing => return Err(format!("unknown var {}", token.text)),
            NSymbol::Prim(prim) => match prim {
                Prim::IntEq => Ty::Fn(FnTy {
                    params: self.bump().alloc_slice_copy(&[Ty::Int; 2]),
                    result: self.bump().alloc(Ty::Bool),
                }),
                Prim::IntAdd => Ty::Fn(FnTy {
                    params: self.bump().alloc_slice_copy(&[Ty::Int; 2]),
                    result: self.bump().alloc(Ty::Int),
                }),
            },
            NSymbol::PrimTy(..) => unreachable!(),
            NSymbol::StaticVar { id, .. } => self.static_vars[&id],
            NSymbol::Param { id, .. } => self.params[&id],
            NSymbol::LocalVar { id, .. } => self.local_vars[&id],
        };
        Ok(ty)
    }

    pub(crate) fn expect_assignable(&mut self, target_ty: Ty<'a>, src_ty: Ty<'a>) -> TResult<()> {
        if src_ty != target_ty {
            return Err(format!(
                "type mismatch: src={:?} target={:?}",
                src_ty, target_ty
            ));
        }

        Ok(())
    }

    pub(crate) fn expect_callable(&mut self, ty: Ty<'a>, arity: usize) -> TResult<FnTy<'a>> {
        match ty {
            Ty::Fn(ty) => {
                if arity != ty.params.len() {
                    return Err("arity mismatch".into());
                }

                Ok(ty)
            }
            _ => Err(format!("can't call {:?}", ty)),
        }
    }

    pub(crate) fn expect_cond_ty(&mut self, ty: Ty<'a>) -> TResult<()> {
        if ty != Ty::Bool {
            return Err("non-bool condition".into());
        }

        Ok(())
    }

    pub(crate) fn new_joint(&mut self) -> Joint<'a> {
        Joint::Empty
    }

    fn add_to_joint(&mut self, joint: &mut Joint<'a>, ty: Ty<'a>) -> TResult<()> {
        match *joint {
            Joint::Empty => {
                *joint = Joint::Ty(ty);
            }
            Joint::Ty(current_ty) => {
                if ty != current_ty {
                    return Err("inconsistent if type".into());
                }
            }
        }
        Ok(())
    }

    fn consume_joint(&mut self, joint: Joint<'a>) -> Ty<'a> {
        match joint {
            Joint::Empty => {
                // never or fresh meta ty?
                Ty::Unit
            }
            Joint::Ty(ty) => ty,
        }
    }
}
