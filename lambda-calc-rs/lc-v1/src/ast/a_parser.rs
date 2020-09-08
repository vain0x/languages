use super::a_tree::*;
use crate::{
    context::Context, parse::parser::LambdaParserHost,
    semantics::scope::name_resolution::NameResolution,
    semantics::scope::scope_resolver::ScopeResolver, syntax::syntax_token::SyntaxToken, utils::*,
};

pub(crate) struct AstLambdaParserHost<'a> {
    scope_resolver: ScopeResolver<'a>,
    pub(crate) context: &'a Context,
}

impl<'a> AstLambdaParserHost<'a> {
    pub(crate) fn new(context: &'a Context) -> Self {
        Self {
            scope_resolver: ScopeResolver::new(context),
            context,
        }
    }

    fn new_box<T>(&self, value: T) -> BumpaloBox<'a, T> {
        BumpaloBox::new_in(value, &self.context.bump)
    }

    fn new_vec<T>(&self) -> BumpaloVec<'a, T> {
        BumpaloVec::new_in(&self.context.bump)
    }

    pub(crate) fn take_output(&mut self) -> NameResolution {
        self.scope_resolver.take_output()
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
        self.scope_resolver.after_param(name, param_list.len());

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
        self.scope_resolver.after_name_ty(token);

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
        self.scope_resolver.after_ident_expr(token);

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
        self.scope_resolver.before_block_expr();

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
        self.scope_resolver.after_block_expr();

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

    fn before_fn_expr(&mut self, keyword: SyntaxToken<'a>) {
        self.scope_resolver.before_fn_expr(keyword);
    }

    fn after_fn_expr(
        &mut self,
        keyword: SyntaxToken<'a>,
        param_list_opt: Option<Self::AfterParamList>,
        _arrow_opt: Option<SyntaxToken<'a>>,
        result_ty_opt: Option<Self::AfterTy>,
        body_opt: Option<Self::AfterExpr>,
    ) -> Self::AfterExpr {
        self.scope_resolver.after_fn_expr(keyword);

        AExpr::Fn(AFnExpr {
            id: keyword.index,
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
        self.scope_resolver.after_let_decl(name_opt);

        ADecl::Let(ALetDecl { name_opt, init_opt })
    }

    fn after_root(&mut self, decls: Vec<Self::AfterDecl>, eof: SyntaxToken<'a>) -> Self::AfterRoot {
        self.scope_resolver.after_root();

        ARoot {
            decls: self.context.allocate_iter(decls),
            eof,
        }
    }
}
