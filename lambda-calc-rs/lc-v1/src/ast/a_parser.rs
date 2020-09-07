use std::collections::HashMap;
use super::a_tree::*;
use crate::{
    context::Context, eval::code_gen::Prim, eval::type_check::PrimTy,
    parse::parser::LambdaParserHost, syntax::syntax_token::SyntaxToken, utils::*,
};

#[derive(Copy, Clone, Debug)]
pub(crate) enum NSymbol {
    Missing,
    Prim(Prim),
    PrimTy(PrimTy),
    StaticVar {
        id: usize,
        depth: usize,
        index: usize,
    },
    Param {
        id: usize,
        depth: usize,
        index: usize,
    },
    LocalVar {
        id: usize,
        depth: usize,
        index: usize,
    },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum ScopeKind {
    Block,
    Fn,
    Root,
    Prim,
}

pub(crate) struct Scope<'a> {
    kind: ScopeKind,
    map: HashMap<&'a str, NSymbol>,
    local_var_count: usize,
}

impl<'a> Scope<'a> {
    pub(crate) fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            map: HashMap::new(),
            local_var_count: 0,
        }
    }
}

fn resolve_builtin_ty(name: &str) -> Option<PrimTy> {
    let ty = match name {
        "unit" => PrimTy::Unit,
        "bool" => PrimTy::Bool,
        "int" => PrimTy::Int,
        _ => return None,
    };
    Some(ty)
}

pub(crate) struct AstLambdaParserHost<'a> {
    symbol_id: usize,
    pub(crate) name_res: HashMap<usize, NSymbol>,
    pub(crate) scope_chain: Vec<Scope<'a>>,
    pub(crate) context: &'a Context,
}

impl<'a> AstLambdaParserHost<'a> {
    pub(crate) fn new(context: &'a Context) -> Self {
        let mut scope = Scope::new(ScopeKind::Prim);
        scope.map.insert("int_eq", NSymbol::Prim(Prim::IntEq));
        scope.map.insert("int_add", NSymbol::Prim(Prim::IntAdd));

        let scope_chain = vec![scope, Scope::new(ScopeKind::Root)];

        Self {
            symbol_id: 0,
            name_res: HashMap::new(),
            scope_chain,
            context,
        }
    }

    fn new_box<T>(&self, value: T) -> BumpaloBox<'a, T> {
        BumpaloBox::new_in(value, &self.context.bump)
    }

    fn new_vec<T>(&self) -> BumpaloVec<'a, T> {
        BumpaloVec::new_in(&self.context.bump)
    }

    fn fresh_symbol_id(&mut self) -> usize {
        self.symbol_id += 1;
        self.symbol_id
    }
}

impl<'a> LambdaParserHost<'a> for AstLambdaParserHost<'a> {
    type BeforeParamTyList = BumpaloVec<'a, ATy<'a>>;
    type AfterParamTyList = BumpaloVec<'a, ATy<'a>>;

    type BeforeParamList = BumpaloVec<'a, (SyntaxToken<'a>, Option<ATy<'a>>)>;
    type AfterParamList = BumpaloVec<'a, (SyntaxToken<'a>, Option<ATy<'a>>)>;

    type BeforeArgList = BumpaloVec<'a, AExpr<'a>>;
    type AfterArgList = BumpaloVec<'a, AExpr<'a>>;

    type BeforeBlockExpr = BumpaloVec<'a, ADecl<'a>>;

    type AfterTy = ATy<'a>;
    type AfterExpr = AExpr<'a>;
    type AfterDecl = ADecl<'a>;
    type AfterRoot = ARoot<'a>;

    fn before_param_ty_list(&mut self, _left_paren: SyntaxToken<'a>) -> Self::BeforeParamTyList {
        BumpaloVec::new_in(&self.context.bump)
    }

    fn after_param_ty(
        &mut self,
        param_ty: Self::AfterTy,
        _comma_opt: Option<SyntaxToken<'a>>,
        param_ty_list: &mut Self::BeforeParamTyList,
    ) {
        param_ty_list.push(param_ty);
    }

    fn after_param_ty_list(
        &mut self,
        _right_paren_opt: Option<SyntaxToken<'a>>,
        param_ty_list: Self::BeforeParamTyList,
    ) -> Self::AfterParamTyList {
        param_ty_list
    }

    fn before_param_list(&mut self, _left_paren: SyntaxToken<'a>) -> Self::BeforeParamList {
        BumpaloVec::new_in(&self.context.bump)
    }

    fn after_param(
        &mut self,
        name: SyntaxToken<'a>,
        _colon_opt: Option<SyntaxToken<'a>>,
        ty_opt: Option<Self::AfterTy>,
        _comma_opt: Option<SyntaxToken<'a>>,
        param_list: &mut Self::BeforeParamList,
    ) {
        let id = self.fresh_symbol_id();
        let depth = self.scope_chain.len() - 1;
        let index = param_list.len();
        let symbol = NSymbol::Param { id, depth, index };
        self.scope_chain
            .last_mut()
            .unwrap()
            .map
            .insert(name.text, symbol);
        self.name_res.insert(name.index, symbol);

        param_list.push((name, ty_opt));
    }

    fn after_param_list(
        &mut self,
        _right_paren_opt: Option<SyntaxToken<'a>>,
        param_list: Self::BeforeParamList,
    ) -> Self::AfterParamList {
        param_list
    }

    fn before_arg_list(&mut self, _left_paren: SyntaxToken<'a>) -> Self::BeforeArgList {
        BumpaloVec::new_in(&self.context.bump)
    }

    fn after_arg(
        &mut self,
        expr: Self::AfterExpr,
        _comma_opt: Option<SyntaxToken<'a>>,
        arg_list: &mut Self::BeforeArgList,
    ) {
        arg_list.push(expr);
    }

    fn after_arg_list(
        &mut self,
        _right_paren: Option<SyntaxToken<'a>>,
        arg_list: Self::BeforeArgList,
    ) -> Self::AfterArgList {
        arg_list
    }

    fn after_name_ty(&mut self, token: SyntaxToken<'a>) -> Self::AfterTy {
        let symbol = match resolve_builtin_ty(token.text) {
            Some(ty) => NSymbol::PrimTy(ty),
            None => NSymbol::Missing,
        };
        self.name_res.insert(token.index, symbol);

        ATy::Name(token)
    }

    fn after_fn_ty(
        &mut self,
        _keyword: SyntaxToken<'a>,
        param_ty_list_opt: Option<Self::AfterParamTyList>,
        _arrow_opt: Option<SyntaxToken<'a>>,
        result_ty_opt: Option<Self::AfterTy>,
    ) -> Self::AfterTy {
        ATy::Fn(AFnTy {
            param_tys: self
                .context
                .bump
                .alloc(param_ty_list_opt.unwrap_or_else(|| self.new_vec()))
                .as_slice(),
            result_ty_opt: result_ty_opt.map(|ty| &*self.context.bump.alloc(ty)),
        })
    }

    fn after_true_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr {
        AExpr::True(token)
    }

    fn after_false_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr {
        AExpr::False(token)
    }

    fn after_number_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr {
        AExpr::Number(token)
    }

    fn after_ident_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr {
        let symbol = self
            .scope_chain
            .iter()
            .rev()
            .find_map(|scope| scope.map.get(token.text).copied())
            .unwrap_or(NSymbol::Missing);
        self.name_res.insert(token.index, symbol);

        AExpr::Var(token)
    }

    fn after_call_expr(
        &mut self,
        callee: Self::AfterExpr,
        arg_list: Self::AfterArgList,
    ) -> Self::AfterExpr {
        AExpr::Call(ACallExpr {
            callee: self.new_box(callee),
            args: arg_list,
        })
    }

    fn before_block_expr(&mut self, _left_paren: SyntaxToken<'a>) -> Self::BeforeBlockExpr {
        self.scope_chain.push(Scope::new(ScopeKind::Block));

        BumpaloVec::new_in(&self.context.bump)
    }

    fn after_decl_in_block(
        &mut self,
        decl: Self::AfterDecl,
        block_expr: &mut Self::BeforeBlockExpr,
    ) {
        block_expr.push(decl);
    }

    fn after_block_expr(
        &mut self,
        _right_paren_opt: Option<SyntaxToken<'a>>,
        block_expr: Self::BeforeBlockExpr,
    ) -> Self::AfterExpr {
        let scope_opt = self.scope_chain.pop();
        assert_eq!(scope_opt.map(|s| s.kind), Some(ScopeKind::Block));

        AExpr::Block(block_expr)
    }

    fn after_if_expr(
        &mut self,
        _if_keyword: SyntaxToken<'a>,
        cond_opt: Option<Self::AfterExpr>,
        body_opt: Option<Self::AfterExpr>,
        _else_keyword: Option<SyntaxToken<'a>>,
        alt_opt: Option<Self::AfterExpr>,
    ) -> Self::AfterExpr {
        AExpr::If(AIfExpr {
            cond_opt: cond_opt.map(|expr| self.new_box(expr)),
            body_opt: body_opt.map(|expr| self.new_box(expr)),
            alt_opt: alt_opt.map(|expr| self.new_box(expr)),
        })
    }

    fn before_fn_expr(&mut self) {
        self.scope_chain.push(Scope::new(ScopeKind::Fn));
    }

    fn after_fn_expr(
        &mut self,
        _keyword: SyntaxToken<'a>,
        param_list_opt: Option<Self::AfterParamList>,
        _arrow_opt: Option<SyntaxToken<'a>>,
        result_ty_opt: Option<Self::AfterTy>,
        body_opt: Option<Self::AfterExpr>,
    ) -> Self::AfterExpr {
        let scope_opt = self.scope_chain.pop();
        assert_eq!(scope_opt.map(|s| s.kind), Some(ScopeKind::Fn));

        AExpr::Fn(AFnExpr {
            params: param_list_opt.unwrap_or_else(|| BumpaloVec::new_in(&self.context.bump)),
            result_ty_opt: result_ty_opt.map(|ty| self.new_box(ty)),
            body_opt: body_opt.map(|body| self.new_box(body)),
        })
    }

    fn after_expr_decl(
        &mut self,
        expr: Self::AfterExpr,
        _semi_opt: Option<SyntaxToken<'a>>,
    ) -> Self::AfterDecl {
        ADecl::Expr(expr)
    }

    fn after_let_decl(
        &mut self,
        _keyword: SyntaxToken<'a>,
        name_opt: Option<SyntaxToken<'a>>,
        _equal_opt: Option<SyntaxToken<'a>>,
        init_opt: Option<Self::AfterExpr>,
        _semi_opt: Option<SyntaxToken<'a>>,
    ) -> Self::AfterDecl {
        if let Some(name) = &name_opt {
            let id = self.fresh_symbol_id();
            let is_global = self
                .scope_chain
                .iter()
                .rev()
                .skip_while(|s| s.kind == ScopeKind::Block)
                .next()
                .unwrap()
                .kind
                == ScopeKind::Root;

            let depth = self.scope_chain.len() - 1;
            let scope = self.scope_chain.last_mut().unwrap();
            let index = scope.local_var_count;
            let symbol = if is_global {
                NSymbol::StaticVar { id, depth, index }
            } else {
                NSymbol::LocalVar { id, depth, index }
            };
            scope.map.insert(name.text, symbol);

            self.name_res.insert(name.index, symbol);
        }

        ADecl::Let(ALetDecl { name_opt, init_opt })
    }

    fn after_root(&mut self, decls: Vec<Self::AfterDecl>, eof: SyntaxToken<'a>) -> Self::AfterRoot {
        let scope_opt = self.scope_chain.pop();
        assert_eq!(scope_opt.map(|s| s.kind), Some(ScopeKind::Root));

        ARoot {
            decls: self.context.allocate_iter(decls),
            eof,
        }
    }
}
