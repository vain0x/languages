use crate::{
    ast::a_tree::*,
    eval::*,
    semantics::{prim::Prim, symbol::*},
    syntax::syntax_token::SyntaxToken,
};
use std::{cell::RefCell, marker::PhantomData, mem::take};

type GResult<T> = Result<T, String>;

pub(crate) type BlockId = usize;

/// 仮想レジスタ
#[derive(Copy, Clone)]
pub(crate) struct Reg(usize);

impl Debug for Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "v_{:x}", self.0)
    }
}

#[derive(Clone)]
pub(crate) enum GTerm<'a> {
    #[allow(unused)]
    Todo,

    Unit,
    Bool(bool),
    Int(i64),
    StaticVar(StaticVarId),
    Param(ParamId),
    LocalVar(LocalVarId),
    Fn(FnId),
    Prim(Prim),
    Reg(Reg),

    #[allow(unused)]
    Unused(PhantomData<&'a ()>),
}

impl<'a> Debug for GTerm<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GTerm::Todo => write!(f, "{{todo}}"),
            GTerm::Unit => write!(f, "unit"),
            GTerm::Bool(value) => Debug::fmt(value, f),
            GTerm::Int(value) => write!(f, "{}_int", value),
            GTerm::StaticVar(id) => write!(f, "s_{:x}", id),
            GTerm::Param(id) => write!(f, "p_{:x}", id),
            GTerm::LocalVar(id) => write!(f, "l_{:x}", id),
            GTerm::Fn(id) => write!(f, "f_{:x}", id),
            GTerm::Prim(prim) => Debug::fmt(prim, f),
            GTerm::Reg(reg) => Debug::fmt(reg, f),
            GTerm::Unused(_) => unreachable!(),
        }
    }
}

/// Immediate value.
pub(crate) enum Imm {
    Bool(bool),
    Int(i64),
    Fn(FnId),
    Prim(Prim),
}

impl Debug for Imm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Imm::Bool(value) => write!(f, "{:?}", value),
            Imm::Int(value) => write!(f, "{}_int", value),
            Imm::Fn(value) => write!(f, "fn#{}", value),
            Imm::Prim(value) => Debug::fmt(value, f),
        }
    }
}

pub(crate) enum Code<'a> {
    Mov(Reg, Reg),
    MovImm(Reg, Imm),
    LoadStaticVar(Reg, StaticVarId),
    LoadParam(Reg, ParamId),
    LoadLocalVar(Reg, LocalVarId),
    StoreStaticVar(StaticVarId, GTerm<'a>),
    StoreLocalVar(LocalVarId, GTerm<'a>),
    PrintVal(&'a str, GTerm<'a>),
    Call {
        fn_term: GTerm<'a>,
        arg_terms: Vec<GTerm<'a>>,
    },
}

impl<'a> Debug for Code<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Code::Mov(dest, src) => write!(f, "mov {:?} <- {:?}", dest, src),
            Code::MovImm(dest, value) => write!(f, "im {:?} <- {:?}", dest, value),
            Code::LoadStaticVar(dest, id) => write!(f, "load {:?} <- s_{:x}", dest, id),
            Code::LoadParam(dest, id) => write!(f, "load {:?} <- p_{:x}", dest, id),
            Code::LoadLocalVar(dest, id) => write!(f, "load {:?} <- l_{:x}", dest, id),
            Code::StoreStaticVar(id, src) => write!(f, "store s_{:x} <- {:?}", id, src),
            Code::StoreLocalVar(id, src) => write!(f, "store l_{:x} <- {:?}", id, src),
            Code::PrintVal(name, value) => write!(f, "print({:?}, {:?})", name, value),
            Code::Call { fn_term, arg_terms } => {
                write!(f, "call ")?;
                Debug::fmt(fn_term, f)?;
                write!(f, "(")?;
                for (i, arg_term) in arg_terms.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    Debug::fmt(arg_term, f)?;
                }
                write!(f, ")")
            }
        }
    }
}

pub(crate) enum Cont<'a> {
    Jump(BlockId),
    If {
        cond_term: GTerm<'a>,
        then_block: BlockId,
        else_block: BlockId,
    },
    Return,
    Exit,
}

impl<'a> Debug for Cont<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Cont::Jump(block_id) => write!(f, "jump b_{:x}", block_id),
            Cont::If {
                cond_term,
                then_block,
                else_block,
            } => write!(
                f,
                "jump (if {:?} then b_{:x} else b_{:x})",
                cond_term, then_block, else_block
            ),
            Cont::Return => write!(f, "ret"),
            Cont::Exit => write!(f, "exit"),
        }
    }
}

impl<'a> Default for Cont<'a> {
    fn default() -> Self {
        Cont::Exit
    }
}

#[derive(Debug, Default)]
pub(crate) struct BlockInfo<'a> {
    codes: Vec<Code<'a>>,
    cont: Cont<'a>,
}

#[derive(Debug, Default)]
pub(crate) struct FnInfo {
    local_var_count: usize,
    entry_block: BlockId,
}

#[derive(Debug, Default)]
pub(crate) struct Program<'a> {
    reg_count: usize,
    blocks: Vec<BlockInfo<'a>>,
    fns: Vec<FnInfo>,
    codes: Vec<Code<'a>>,
}

type StaticVarId = usize;
type ParamId = usize;
type LocalVarId = usize;
type FnId = usize;

#[derive(Default)]
struct BlockBuilder<'a> {
    codes: Vec<Code<'a>>,
    cont_opt: Option<Cont<'a>>,
}

struct FnBuilder {
    local_var_count: usize,
    entry_block: usize,
}

pub(crate) struct BranchState {
    arm_index: usize,
    arm_blocks: Vec<BlockId>,
    target_block: BlockId,
    target_reg: Reg,
    parent_break: Option<(BlockId, Reg)>,
}

pub(crate) struct ArmState<'br> {
    branch: &'br mut BranchState,
}

pub(crate) struct FnExprBuilder {
    fn_id: FnId,
    parent_block: Option<BlockId>,
    parent_fn: Option<FnId>,
}

#[derive(Default)]
pub(crate) struct CodeGenerator<'a> {
    current_block: RefCell<Option<BlockId>>,
    current_break: RefCell<Option<(BlockId, Reg)>>,
    current_fn: RefCell<Option<FnId>>,
    blocks: RefCell<Vec<BlockBuilder<'a>>>,
    fns: RefCell<Vec<FnBuilder>>,
    pub(crate) program: RefCell<Program<'a>>,

    // read
    ast_opt: Option<&'a Ast<'a>>,
}

impl<'a> CodeGenerator<'a> {
    pub(crate) fn init(&mut self, ast: &'a Ast<'a>) {
        self.ast_opt = Some(ast);
    }

    fn ast(&self) -> &'a Ast<'a> {
        self.ast_opt.unwrap()
    }

    fn in_global(&self) -> bool {
        self.current_fn.borrow().is_none()
    }

    fn set_to_reg(&self, reg: Reg, term: GTerm<'a>) {
        match term {
            GTerm::Todo => todo!(),
            GTerm::Unit => {}
            GTerm::Bool(value) => self.emit(Code::MovImm(reg, Imm::Bool(value))),
            GTerm::Int(value) => self.emit(Code::MovImm(reg, Imm::Int(value))),
            GTerm::StaticVar(id) => self.emit(Code::LoadStaticVar(reg, id)),
            GTerm::Param(id) => self.emit(Code::LoadParam(reg, id)),
            GTerm::LocalVar(id) => self.emit(Code::LoadLocalVar(reg, id)),
            GTerm::Fn(id) => self.emit(Code::MovImm(reg, Imm::Fn(id))),
            GTerm::Reg(src) => self.emit(Code::Mov(reg, src)),
            GTerm::Prim(prim) => self.emit(Code::MovImm(reg, Imm::Prim(prim))),
            GTerm::Unused(..) => unreachable!(),
        }
    }

    fn fresh_reg(&self) -> Reg {
        let mut program = self.program.borrow_mut();
        let reg = Reg(program.reg_count);
        program.reg_count += 1;
        reg
    }

    fn new_block(&self) -> BlockId {
        let mut blocks = self.blocks.borrow_mut();
        let block_id = blocks.len();
        blocks.push(BlockBuilder::default());
        block_id
    }

    fn emit(&self, code: Code<'a>) {
        let block_id = self.current_block.borrow().unwrap();
        let block = &mut self.blocks.borrow_mut()[block_id];
        assert!(block.cont_opt.is_none());
        block.codes.push(code);
    }

    fn begin_block(&self, block_id: BlockId) {
        if let Some(block_id) = *self.current_block.borrow() {
            assert!(self.blocks.borrow()[block_id].cont_opt.is_none());
        }

        self.current_block.borrow_mut().replace(block_id);
    }

    fn end_block(&self, cont: Cont<'a>) {
        let block_id = self.current_block.borrow_mut().take().unwrap();
        let old = self.blocks.borrow_mut()[block_id].cont_opt.replace(cont);
        assert!(old.is_none());
    }

    pub(crate) fn eval_var(&self, name: SyntaxToken<'a>) -> GResult<GTerm<'a>> {
        let ty = match self.ast().name_res[&name.index] {
            NSymbol::Missing => return Err(format!("unknown var {}", name.text)),
            NSymbol::Prim(prim) => GTerm::Prim(prim),
            NSymbol::PrimTy(..) => unreachable!(),
            NSymbol::StaticVar { id, .. } => GTerm::StaticVar(id),
            NSymbol::Param { id, .. } => GTerm::Param(id),
            NSymbol::LocalVar { id, .. } => GTerm::LocalVar(id),
        };
        Ok(ty)
    }

    pub(crate) fn before_branch(&self, cond_term: GTerm<'a>, arm_count: usize) -> BranchState {
        let target_reg = self.fresh_reg();

        let arm_blocks = (0..arm_count).map(|_| self.new_block()).collect::<Vec<_>>();
        let target_block = self.new_block();

        let cont = match arm_blocks.as_slice() {
            &[then_block, else_block] => Cont::If {
                cond_term,
                then_block,
                else_block,
            },
            _ => unimplemented!(),
        };
        self.end_block(cont);

        let parent_break = self
            .current_break
            .borrow_mut()
            .replace((target_block, target_reg));

        BranchState {
            arm_index: 0,
            arm_blocks,
            target_block,
            target_reg,
            parent_break,
        }
    }

    pub(crate) fn after_branch(&self, branch: BranchState) -> GTerm<'a> {
        let BranchState {
            arm_index,
            arm_blocks,
            target_block,
            target_reg,
            parent_break,
        } = branch;
        assert_eq!(arm_index, arm_blocks.len());
        assert!(arm_blocks
            .iter()
            .all(|&arm_block| self.blocks.borrow()[arm_block].cont_opt.is_some()));

        *self.current_break.borrow_mut() = parent_break;
        self.begin_block(target_block);
        GTerm::Reg(target_reg)
    }

    pub(crate) fn before_arm<'br>(&self, branch: &'br mut BranchState) -> ArmState<'br> {
        let arm_block = branch.arm_blocks[branch.arm_index];
        branch.arm_index += 1;

        self.begin_block(arm_block);
        ArmState { branch }
    }

    pub(crate) fn after_arm<'br>(&self, term: GTerm<'a>, arm: ArmState<'br>) {
        let BranchState {
            target_reg,
            target_block,
            ..
        } = *arm.branch;

        self.set_to_reg(target_reg, term);
        self.end_block(Cont::Jump(target_block));
    }

    pub(crate) fn on_call_expr(&self, fn_term: GTerm<'a>, arg_terms: Vec<GTerm<'a>>) -> GTerm<'a> {
        self.emit(Code::Call { fn_term, arg_terms });
        // 関数の結果は 0 番レジスタに入る。
        GTerm::Reg(Reg(0))
    }

    pub(crate) fn before_fn_expr(&self, param_count: usize) -> FnExprBuilder {
        let entry_block = self.new_block();

        let fn_id = {
            let mut fns = self.fns.borrow_mut();
            let fn_id = fns.len();
            fns.push(FnBuilder {
                local_var_count: param_count,
                entry_block,
            });
            // TODO: プロローグを挿入する？
            fn_id
        };

        let parent_fn = self.current_fn.borrow_mut().replace(fn_id);
        let parent_block = self.current_block.borrow_mut().replace(entry_block);

        self.begin_block(entry_block);
        FnExprBuilder {
            fn_id,
            parent_block,
            parent_fn,
        }
    }

    pub(crate) fn after_fn_expr(&self, body_term: GTerm<'a>, builder: FnExprBuilder) -> GTerm<'a> {
        let FnExprBuilder {
            fn_id,
            parent_block,
            parent_fn,
        } = builder;

        self.set_to_reg(Reg(0), body_term);
        self.end_block(Cont::Return);

        *self.current_block.borrow_mut() = parent_block;
        *self.current_fn.borrow_mut() = parent_fn;
        GTerm::Fn(fn_id)
    }

    pub(crate) fn after_expr_stmt(&self, term: GTerm<'a>) {
        let name = "it";

        if self.in_global() {
            self.emit(Code::PrintVal(name, term));
        }
    }

    pub(crate) fn after_let_stmt(&self, name_opt: Option<SyntaxToken<'a>>, term: GTerm<'a>) {
        let name = match name_opt {
            Some(it) => it,
            None => return,
        };

        let code = {
            let term = term.clone();
            match self.ast().name_res[&name.index] {
                NSymbol::StaticVar { id, .. } => Code::StoreStaticVar(id, term),
                NSymbol::LocalVar { id, .. } => Code::StoreLocalVar(id, term),
                _ => unreachable!(),
            }
        };
        self.emit(code);

        if self.in_global() {
            self.emit(Code::PrintVal(name.text, term));
        }
    }

    pub(crate) fn before_root(&self) {
        // 関数の結果を入れるレジスタを確保する。
        self.program.borrow_mut().reg_count += 1;

        // トップレベルのブロックを開始する。
        let toplevel_block = self.new_block();
        assert_eq!(toplevel_block, 0);
        *self.current_block.borrow_mut() = Some(toplevel_block);
    }

    pub(crate) fn after_root(&self) {
        self.end_block(Cont::Exit);

        let mut program = self.program.borrow_mut();
        program.blocks = take(&mut *self.blocks.borrow_mut())
            .into_iter()
            .map(|block_builder| {
                let BlockBuilder { cont_opt, codes } = block_builder;
                BlockInfo {
                    codes,
                    cont: cont_opt.unwrap(),
                }
            })
            .collect();

        program.fns = take(&mut *self.fns.borrow_mut())
            .into_iter()
            .map(|fn_builder| {
                let FnBuilder {
                    local_var_count,
                    entry_block,
                } = fn_builder;

                FnInfo {
                    local_var_count,
                    entry_block,
                }
            })
            .collect();
    }
}
