//! Instructions.

use al_aux::il::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum InsKind {
    Exit,
    Assert,
    Bool(bool),
    Int(i64),
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
        IlKind::Exit => inss.push(InsKind::Exit),
        IlKind::Semi => {}
        IlKind::Assert => inss.push(InsKind::Assert),
        IlKind::Bool(value) => inss.push(InsKind::Bool(value)),
        IlKind::Int(value) => inss.push(InsKind::Int(value)),
        IlKind::OpAdd => inss.push(InsKind::OpAdd),
        IlKind::OpSub => inss.push(InsKind::OpSub),
        IlKind::OpMul => inss.push(InsKind::OpMul),
        IlKind::OpDiv => inss.push(InsKind::OpDiv),
        IlKind::OpEq => inss.push(InsKind::OpEq),
    }
}

pub(crate) fn gen(t: &IlTree) -> Vec<InsKind> {
    let mut inss = vec![];
    gen_ins(t.root(), t, &mut inss);
    inss
}
