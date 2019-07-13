//! Instructions.

use al_aux::il::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum InsKind {
    // 文
    Exit,
    Assert,
    CellSet,

    // 式
    Bool(bool),
    Int(i64),
    GlobalGet,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,
}

fn gen_ins(il: usize, t: &IlTree, inss: &mut Vec<InsKind>) {
    for ci in 0..t.child_len(il) {
        gen_ins(t.child(il, ci), t, inss);
    }

    match t.kind(il) {
        // 宣言:
        IlKind::Root => {}
        IlKind::CodeSection => {}

        // 文:
        IlKind::Semi => {}
        IlKind::Assert => inss.push(InsKind::Assert),
        IlKind::CellSet => inss.push(InsKind::CellSet),

        // 式:
        IlKind::Bool(value) => inss.push(InsKind::Bool(value)),
        IlKind::Int(value) => inss.push(InsKind::Int(value)),
        IlKind::GlobalGet => inss.push(InsKind::GlobalGet),
        IlKind::OpAdd => inss.push(InsKind::OpAdd),
        IlKind::OpSub => inss.push(InsKind::OpSub),
        IlKind::OpMul => inss.push(InsKind::OpMul),
        IlKind::OpDiv => inss.push(InsKind::OpDiv),
        IlKind::OpEq => inss.push(InsKind::OpEq),
    }

    inss.push(InsKind::Exit);
}

pub(crate) fn gen(t: &IlTree) -> Vec<InsKind> {
    let mut inss = vec![];
    gen_ins(t.root(), t, &mut inss);
    inss
}
