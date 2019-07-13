//! Instructions.

use std::collections::HashMap;
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

fn gen_globals(il: usize, t: &IlTree, globals: &mut HashMap<String, usize>) {
    for ci in 0..t.child_len(il) {
        let child = t.child(il, ci);
        match t.kind(child) {
            IlKind::Ident(string_id) => {
                let ident = t.get_string(string_id).to_owned();
                let global_id = globals.len();
                globals.insert(ident, global_id);
            }
            kind => unreachable!(
                "globals の子ノードはすべて識別子でなければいけない {:?}",
                kind
            ),
        }
    }
}

fn gen_ins(il: usize, t: &IlTree, inss: &mut Vec<InsKind>, globals: &mut HashMap<String, usize>) {
    match t.kind(il) {
        IlKind::Globals => return gen_globals(il, t, globals),
        _ => {}
    }

    for ci in 0..t.child_len(il) {
        gen_ins(t.child(il, ci), t, inss, globals);
    }

    match t.kind(il) {
        // 宣言:

        IlKind::Root | IlKind::CodeSection | IlKind::Globals => {}

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

        // その他:

        IlKind::Ident(string_id) => {
            let ident = t.get_string(string_id);
            match globals.get(ident) {
                Some(global_id) => inss.push(InsKind::Int(*global_id as i64)),
                None => panic!("Unknown ident {}", ident),
            }
        }
    }

}

pub(crate) fn gen(t: &IlTree) -> (Vec<InsKind>, usize) {
    let mut inss = vec![];
    let mut globals = HashMap::new();

    gen_ins(t.root(), t, &mut inss, &mut globals);
    inss.push(InsKind::Exit);

    (inss, globals.len())
}
