use crate::syntax::syntax_token::SyntaxToken;

pub(crate) trait LambdaParserHost<'a> {
    type BeforeParamTyList;
    type AfterParamTyList;
    type BeforeParamList;
    type AfterParamList;
    type BeforeArgList;
    type AfterArgList;
    type BeforeBlockExpr;
    type AfterTy;
    type AfterExpr;
    type AfterDecl;
    type AfterRoot;

    fn before_param_ty_list(&mut self, left_paren: SyntaxToken<'a>) -> Self::BeforeParamTyList;
    fn after_param_ty(
        &mut self,
        param_ty: Self::AfterTy,
        comma_opt: Option<SyntaxToken<'a>>,
        param_ty_list: &mut Self::BeforeParamTyList,
    );
    fn after_param_ty_list(
        &mut self,
        right_paren_opt: Option<SyntaxToken<'a>>,
        param_ty_list: Self::BeforeParamTyList,
    ) -> Self::AfterParamTyList;

    fn before_param_list(&mut self, left_paren: SyntaxToken<'a>) -> Self::BeforeParamList;
    fn after_param(
        &mut self,
        name: SyntaxToken<'a>,
        colon_opt: Option<SyntaxToken<'a>>,
        ty_opt: Option<Self::AfterTy>,
        comma_opt: Option<SyntaxToken<'a>>,
        param_list: &mut Self::BeforeParamList,
    );
    fn after_param_list(
        &mut self,
        right_paren_opt: Option<SyntaxToken<'a>>,
        param_list: Self::BeforeParamList,
    ) -> Self::AfterParamList;

    fn before_arg_list(&mut self, left_paren: SyntaxToken<'a>) -> Self::BeforeArgList;
    fn after_arg(
        &mut self,
        expr: Self::AfterExpr,
        comma_opt: Option<SyntaxToken<'a>>,
        arg_list: &mut Self::BeforeArgList,
    );
    fn after_arg_list(
        &mut self,
        right_paren_opt: Option<SyntaxToken<'a>>,
        arg_list: Self::BeforeArgList,
    ) -> Self::AfterArgList;

    fn after_name_ty(&mut self, token: SyntaxToken<'a>) -> Self::AfterTy;
    fn after_fn_ty(
        &mut self,
        _keyword: SyntaxToken<'a>,
        param_ty_list_opt: Option<Self::AfterParamTyList>,
        arrow_opt: Option<SyntaxToken<'a>>,
        result_ty_opt: Option<Self::AfterTy>,
    ) -> Self::AfterTy;

    fn after_true_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr;
    fn after_false_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr;
    fn after_number_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr;
    fn after_ident_expr(&mut self, token: SyntaxToken<'a>) -> Self::AfterExpr;

    fn after_call_expr(
        &mut self,
        callee: Self::AfterExpr,
        arg_list: Self::AfterArgList,
    ) -> Self::AfterExpr;

    fn before_block_expr(&mut self, left_paren: SyntaxToken<'a>) -> Self::BeforeBlockExpr;
    fn after_decl_in_block(
        &mut self,
        decl: Self::AfterDecl,
        block_expr: &mut Self::BeforeBlockExpr,
    );
    fn after_block_expr(
        &mut self,
        right_paren_opt: Option<SyntaxToken<'a>>,
        block_expr: Self::BeforeBlockExpr,
    ) -> Self::AfterExpr;

    fn after_if_expr(
        &mut self,
        if_keyword: SyntaxToken<'a>,
        cond_opt: Option<Self::AfterExpr>,
        body_opt: Option<Self::AfterExpr>,
        else_keyword: Option<SyntaxToken<'a>>,
        alt_opt: Option<Self::AfterExpr>,
    ) -> Self::AfterExpr;

    fn before_fn_expr(&mut self, keyword: SyntaxToken<'a>);

    fn after_fn_expr(
        &mut self,
        keyword: SyntaxToken<'a>,
        param_list_opt: Option<Self::AfterParamList>,
        arrow_opt: Option<SyntaxToken<'a>>,
        result_ty_opt: Option<Self::AfterTy>,
        body_opt: Option<Self::AfterExpr>,
    ) -> Self::AfterExpr;

    fn after_expr_decl(
        &mut self,
        expr: Self::AfterExpr,
        semi_opt: Option<SyntaxToken<'a>>,
    ) -> Self::AfterDecl;

    fn after_let_decl(
        &mut self,
        keyword: SyntaxToken<'a>,
        name_opt: Option<SyntaxToken<'a>>,
        equal_opt: Option<SyntaxToken<'a>>,
        init_opt: Option<Self::AfterExpr>,
        semi_opt: Option<SyntaxToken<'a>>,
    ) -> Self::AfterDecl;

    fn after_root(&mut self, decls: Vec<Self::AfterDecl>, eof: SyntaxToken<'a>) -> Self::AfterRoot;
}
