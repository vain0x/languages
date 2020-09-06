use std::collections::HashMap;

use super::a_tree::*;
use crate::{
    context::Context, parse::parser::LambdaParserHost, syntax::syntax_token::SyntaxToken, utils::*,
};

#[derive(Debug)]
pub(crate) enum NSymbol {
    // StaticVar,
    Param,
    LocalVar,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum ScopeKind {
    Block,
    Fn,
    Root,
}

pub(crate) struct Scope<'a> {
    kind: ScopeKind,
    map: HashMap<&'a str, NSymbol>,
}

impl<'a> Scope<'a> {
    pub(crate) fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            map: HashMap::new(),
        }
    }
}

pub(crate) struct AstLambdaParserHost<'a> {
    pub(crate) name_res: HashMap<*const SyntaxToken<'a>, NSymbol>,
    pub(crate) scope_chain: Vec<Scope<'a>>,
    pub(crate) context: &'a Context,
}

impl<'a> AstLambdaParserHost<'a> {
    pub(crate) fn new(context: &'a Context) -> Self {
        Self {
            name_res: HashMap::new(),
            scope_chain: vec![Scope::new(ScopeKind::Root)],
            context,
        }
    }

    fn new_box<T>(&self, value: T) -> BumpaloBox<'a, T> {
        BumpaloBox::new_in(value, &self.context.bump)
    }
}

impl<'a> LambdaParserHost<'a> for AstLambdaParserHost<'a> {
    type BeforeParamList = BumpaloVec<'a, SyntaxToken<'a>>;
    type AfterParamList = BumpaloVec<'a, SyntaxToken<'a>>;

    type BeforeArgList = BumpaloVec<'a, AExpr<'a>>;
    type AfterArgList = BumpaloVec<'a, AExpr<'a>>;

    type BeforeBlockExpr = BumpaloVec<'a, ADecl<'a>>;

    type AfterExpr = AExpr<'a>;
    type AfterDecl = ADecl<'a>;
    type AfterRoot = ARoot<'a>;

    fn before_param_list(&mut self, _left_paren: SyntaxToken<'a>) -> Self::BeforeParamList {
        BumpaloVec::new_in(&self.context.bump)
    }

    fn after_param(
        &mut self,
        name: SyntaxToken<'a>,
        _comma_opt: Option<SyntaxToken<'a>>,
        param_list: &mut Self::BeforeParamList,
    ) {
        self.scope_chain
            .last_mut()
            .unwrap()
            .map
            .insert(name.text, NSymbol::Param);

        param_list.push(name);
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
        match self
            .scope_chain
            .iter()
            .enumerate()
            .rev()
            .find_map(|(index, scope)| scope.map.get(token.text).map(|symbol| (index, symbol)))
        {
            Some((depth, symbol)) => {
                eprintln!("{}: {:?}", token.text, (depth, symbol));
            }
            None => eprintln!("{}: undefined?", token.text),
        };

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
        body_opt: Option<Self::AfterExpr>,
    ) -> Self::AfterExpr {
        let scope_opt = self.scope_chain.pop();
        assert_eq!(scope_opt.map(|s| s.kind), Some(ScopeKind::Fn));

        AExpr::Fn(AFnExpr {
            params: param_list_opt.unwrap_or_else(|| BumpaloVec::new_in(&self.context.bump)),
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
            self.scope_chain
                .last_mut()
                .unwrap()
                .map
                .insert(name.text, NSymbol::LocalVar);
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
