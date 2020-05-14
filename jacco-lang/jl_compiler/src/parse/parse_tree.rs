//! Jacco 言語の構文木の定義

mod parse_element;

#[macro_use]
mod parse_node;

pub(crate) use parse_element::{PElementMut, PElementRef};
pub(crate) use parse_node::{try_as_element_mut, try_as_element_ref, PNode};

use super::*;

pub(crate) type PNameId = usize;

#[derive(Clone, Debug)]
pub(crate) struct PName {
    pub(crate) name_id: PNameId,
    pub(crate) token: TokenData,
}

impl PName {
    pub(crate) fn text(&self) -> &str {
        self.token.text()
    }

    pub(crate) fn location(&self) -> &Location {
        self.token.location()
    }

    pub(crate) fn decompose(self) -> (String, Location) {
        let (_, text, location) = self.token.decompose();
        (text, location)
    }
}

impl PNode for PName {
    fn len(&self) -> usize {
        1
    }

    fn get(&self, i: usize) -> Option<PElementRef> {
        assert_eq!(i, 0);
        try_as_element_ref(&self.token)
    }

    fn get_mut(&mut self, i: usize) -> Option<PElementMut> {
        assert_eq!(i, 0);
        try_as_element_mut(&mut self.token)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PNameTy(pub(crate) PName);

impl PNode for PNameTy {
    impl_node_seq! { 0 }
}

#[derive(Clone, Debug)]
pub(crate) struct PNeverTy {
    pub(crate) bang: TokenData,
}

impl PNode for PNeverTy {
    impl_node_seq! { bang }
}

#[derive(Clone, Debug)]
pub(crate) struct PUnitTy {
    pub(crate) left: TokenData,
    pub(crate) right_opt: Option<TokenData>,
}

impl PNode for PUnitTy {
    impl_node_seq! { left, right_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PPtrTy {
    pub(crate) star: TokenData,
    pub(crate) ty_opt: Option<Box<PTy>>,
}

impl PNode for PPtrTy {
    impl_node_seq! { star, ty_opt }
}

#[derive(Clone, Debug)]
pub(crate) enum PTy {
    Name(PNameTy),
    Never(PNeverTy),
    Unit(PUnitTy),
    Ptr(PPtrTy),
}

impl PNode for PTy {
    impl_node_choice! {
        PTy::Name,
        PTy::Never,
        PTy::Unit,
        PTy::Ptr,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PParam {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) comma_opt: Option<TokenData>,
}

impl PNode for PParam {
    impl_node_seq! { name, colon_opt, ty_opt, comma_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PParamList {
    pub(crate) left: TokenData,
    pub(crate) params: Vec<PParam>,
    pub(crate) right_opt: Option<TokenData>,
}

impl PNode for PParamList {
    fn len(&self) -> usize {
        self.params.len() + 2
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if i == 0 {
            return try_as_element_ref(&self.left);
        }

        i -= 1;

        if let Some(param) = self.params.get(i) {
            return try_as_element_ref(param);
        }

        i -= self.params.len();

        if i == 0 {
            return try_as_element_ref(&self.right_opt);
        }

        unreachable!();
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        if i == 0 {
            return try_as_element_mut(&mut self.left);
        }

        i -= 1;

        let param_count = self.params.len();
        if let Some(param) = self.params.get_mut(i) {
            return try_as_element_mut(param);
        }

        i -= param_count;

        if i == 0 {
            return try_as_element_mut(&mut self.right_opt);
        }

        unreachable!();
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PArg {
    pub(crate) expr: PExpr,
    pub(crate) comma_opt: Option<TokenData>,
}

impl PNode for PArg {
    impl_node_seq! { expr, comma_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PArgList {
    pub(crate) left: TokenData,
    pub(crate) args: Vec<PArg>,
    pub(crate) right_opt: Option<TokenData>,
}

impl PArgList {
    pub(crate) fn is_tuple(&self) -> bool {
        match self.args.as_slice() {
            []
            | [PArg {
                comma_opt: Some(_), ..
            }]
            | [_, _, ..] => true,
            _ => false,
        }
    }
}

impl PNode for PArgList {
    fn len(&self) -> usize {
        self.args.len() + 2
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if i == 0 {
            return try_as_element_ref(&self.left);
        }

        i -= 1;

        if let Some(arg) = self.args.get(i) {
            return try_as_element_ref(arg);
        }

        i -= self.args.len();

        if i == 0 {
            return try_as_element_ref(&self.right_opt);
        }

        unreachable!();
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        if i == 0 {
            return try_as_element_mut(&mut self.left);
        }

        i -= 1;

        let arg_count = self.args.len();
        if let Some(arg) = self.args.get_mut(i) {
            return try_as_element_mut(arg);
        }

        i -= arg_count;

        if i == 0 {
            return try_as_element_mut(&mut self.right_opt);
        }

        unreachable!();
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PBlock {
    pub(crate) left: TokenData,
    pub(crate) decls: Vec<PDecl>,
    pub(crate) last_opt: Option<Box<PExpr>>,
    pub(crate) right_opt: Option<TokenData>,
}

impl PNode for PBlock {
    fn len(&self) -> usize {
        self.decls.len() + 3
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if i == 0 {
            return try_as_element_ref(&self.left);
        }

        i -= 1;

        if let Some(decl) = self.decls.get(i) {
            return try_as_element_ref(decl);
        }

        i -= self.decls.len();

        match i {
            0 => try_as_element_ref(&self.last_opt),
            1 => try_as_element_ref(&self.right_opt),
            _ => unreachable!(),
        }
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        if i == 0 {
            return try_as_element_mut(&mut self.left);
        }

        i -= 1;

        let decl_count = self.decls.len();
        if let Some(decl) = self.decls.get_mut(i) {
            return try_as_element_mut(decl);
        }

        i -= decl_count;

        match i {
            0 => try_as_element_mut(&mut self.last_opt),
            1 => try_as_element_mut(&mut self.right_opt),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PIntExpr {
    pub(crate) token: TokenData,
}

impl PNode for PIntExpr {
    impl_node_seq! { token }
}

#[derive(Clone, Debug)]
pub(crate) struct PStrExpr {
    pub(crate) token: TokenData,
}

impl PNode for PStrExpr {
    impl_node_seq! { token }
}

#[derive(Clone, Debug)]
pub(crate) struct PNameExpr(pub(crate) PName);

impl PNode for PNameExpr {
    impl_node_seq! { 0 }
}

#[derive(Clone, Debug)]
pub(crate) struct PTupleExpr {
    pub(crate) arg_list: PArgList,
}

impl PNode for PTupleExpr {
    impl_node_seq! { arg_list }
}

#[derive(Clone, Debug)]
pub(crate) struct PCallExpr {
    pub(crate) callee: Box<PExpr>,
    pub(crate) arg_list: PArgList,
}

impl PNode for PCallExpr {
    impl_node_seq! { callee, arg_list }
}

#[derive(Clone, Debug)]
pub(crate) struct PUnaryOpExpr {
    pub(crate) op: PUnaryOp,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) location: Location,
}

impl PNode for PUnaryOpExpr {
    impl_node_seq! { arg_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PBinaryOpExpr {
    pub(crate) op: PBinaryOp,
    pub(crate) left: Box<PExpr>,
    pub(crate) right_opt: Option<Box<PExpr>>,
    pub(crate) location: Location,
}

impl PNode for PBinaryOpExpr {
    impl_node_seq! { left, right_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PBlockExpr(pub(crate) PBlock);

impl PNode for PBlockExpr {
    impl_node_seq! { 0 }
}

#[derive(Clone, Debug)]
pub(crate) struct PBreakExpr {
    pub(crate) keyword: TokenData,
    pub(crate) arg_opt: Option<Box<PExpr>>,
}

impl PNode for PBreakExpr {
    impl_node_seq! { keyword, arg_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PContinueExpr {
    pub(crate) keyword: TokenData,
}

impl PNode for PContinueExpr {
    impl_node_seq! { keyword }
}

#[derive(Clone, Debug)]
pub(crate) struct PReturnExpr {
    pub(crate) keyword: TokenData,
    pub(crate) arg_opt: Option<Box<PExpr>>,
}

impl PNode for PReturnExpr {
    impl_node_seq! { keyword, arg_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PIfExpr {
    pub(crate) keyword: TokenData,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) else_opt: Option<TokenData>,
    pub(crate) alt_opt: Option<Box<PExpr>>,
}

impl PNode for PIfExpr {
    impl_node_seq! { keyword, cond_opt, body_opt, else_opt, alt_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PWhileExpr {
    pub(crate) keyword: TokenData,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) body_opt: Option<PBlock>,
}

impl PNode for PWhileExpr {
    impl_node_seq! { keyword, cond_opt, body_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PLoopExpr {
    pub(crate) keyword: TokenData,
    pub(crate) body_opt: Option<PBlock>,
}

impl PNode for PLoopExpr {
    impl_node_seq! { keyword, body_opt }
}

#[derive(Clone, Debug)]
pub(crate) enum PExpr {
    Int(PIntExpr),
    Str(PStrExpr),
    Name(PNameExpr),
    Tuple(PTupleExpr),
    Call(PCallExpr),
    UnaryOp(PUnaryOpExpr),
    BinaryOp(PBinaryOpExpr),
    Block(PBlockExpr),
    Break(PBreakExpr),
    Continue(PContinueExpr),
    Return(PReturnExpr),
    If(PIfExpr),
    While(PWhileExpr),
    Loop(PLoopExpr),
}

impl Default for PExpr {
    fn default() -> Self {
        PExpr::Str(PStrExpr {
            token: TokenData::default(),
        })
    }
}

impl PNode for PExpr {
    impl_node_choice! {
        PExpr::Int,
        PExpr::Str,
        PExpr::Name,
        PExpr::Tuple,
        PExpr::Call,
        PExpr::UnaryOp,
        PExpr::BinaryOp,
        PExpr::Block,
        PExpr::Break,
        PExpr::Continue,
        PExpr::Return,
        PExpr::If,
        PExpr::While,
        PExpr::Loop,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PExprDecl {
    pub(crate) expr: PExpr,
    pub(crate) semi_opt: Option<TokenData>,
}

impl PNode for PExprDecl {
    impl_node_seq! { expr, semi_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PLetDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) equal_opt: Option<TokenData>,
    pub(crate) init_opt: Option<PExpr>,
    pub(crate) semi_opt: Option<TokenData>,
}

impl PNode for PLetDecl {
    impl_node_seq! { keyword, name_opt, colon_opt, ty_opt, equal_opt, init_opt, semi_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PFnDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) param_list_opt: Option<PParamList>,
    pub(crate) arrow_opt: Option<TokenData>,
    pub(crate) result_opt: Option<PTy>,
    pub(crate) block_opt: Option<PBlock>,
}

impl PNode for PFnDecl {
    impl_node_seq! { keyword, name_opt, param_list_opt, arrow_opt, result_opt, block_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PExternFnDecl {
    pub(crate) extern_keyword: TokenData,
    pub(crate) fn_keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) param_list_opt: Option<PParamList>,
    pub(crate) arrow_opt: Option<TokenData>,
    pub(crate) result_opt: Option<PTy>,
    pub(crate) semi_opt: Option<TokenData>,
}

impl PNode for PExternFnDecl {
    impl_node_seq! { extern_keyword, fn_keyword, name_opt, param_list_opt, arrow_opt, result_opt, semi_opt }
}

#[derive(Clone, Debug)]
pub(crate) struct PStructDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) semi_opt: Option<TokenData>,
}

impl PNode for PStructDecl {
    impl_node_seq! { keyword, name_opt, semi_opt }
}

#[derive(Clone, Debug)]
pub(crate) enum PDecl {
    Expr(PExprDecl),
    Let(PLetDecl),
    Fn(PFnDecl),
    ExternFn(PExternFnDecl),
    Struct(PStructDecl),
}

impl PNode for PDecl {
    impl_node_choice! {
        PDecl::Expr,
        PDecl::Let,
        PDecl::Fn,
        PDecl::ExternFn,
        PDecl::Struct,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) decls: Vec<PDecl>,
    pub(crate) eof: TokenData,
}

impl PNode for PRoot {
    fn len(&self) -> usize {
        self.decls.len() + 1
    }

    fn get(&self, mut i: usize) -> Option<PElementRef> {
        if let Some(decl) = self.decls.get(i) {
            return try_as_element_ref(decl);
        }

        i -= self.decls.len();
        if i == 0 {
            return try_as_element_ref(&self.eof);
        }

        unreachable!()
    }

    fn get_mut(&mut self, mut i: usize) -> Option<PElementMut> {
        let decl_count = self.decls.len();
        if let Some(decl) = self.decls.get_mut(i) {
            return try_as_element_mut(decl);
        }

        i -= decl_count;

        if i == 0 {
            return try_as_element_mut(&mut self.eof);
        }

        unreachable!()
    }
}
