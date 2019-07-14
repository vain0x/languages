//! 中間言語からアセンブリへの変換

use super::*;
use al_aux::il::*;

fn gen_globals(il: usize, t: &IlTree, a: &mut AlAsm) {
    for ci in 0..t.child_len(il) {
        let child = t.child(il, ci);
        match t.kind(child) {
            IlKind::Ident(string_id) => {
                let ident = t.get_string(string_id).to_owned();
                a.globals.new_global(ident);
            }
            kind => unreachable!(
                "globals の子ノードはすべて識別子でなければいけない {:?}",
                kind
            ),
        }
    }
}

fn gen_il(il: usize, t: &IlTree, a: &mut AlAsm) {
    match t.kind(il) {
        IlKind::Globals => return gen_globals(il, t, a),
        _ => {}
    }

    for ci in 0..t.child_len(il) {
        gen_il(t.child(il, ci), t, a);
    }

    match t.kind(il) {
        // 宣言:

        IlKind::Root | IlKind::CodeSection | IlKind::Globals => {}

        // 文:

        IlKind::Semi => {}
        IlKind::Assert => a.new_instr(InstrKind::Assert),
        IlKind::CellSet => a.new_instr(InstrKind::CellSet),

        // 式:

        IlKind::Bool(value) => a.new_instr(InstrKind::Bool(value)),
        IlKind::Int(value) => a.new_instr(InstrKind::Int(value)),
        IlKind::GlobalGet => a.new_instr(InstrKind::GlobalGet),
        IlKind::OpAdd => a.new_instr(InstrKind::OpAdd),
        IlKind::OpSub => a.new_instr(InstrKind::OpSub),
        IlKind::OpMul => a.new_instr(InstrKind::OpMul),
        IlKind::OpDiv => a.new_instr(InstrKind::OpDiv),
        IlKind::OpEq => a.new_instr(InstrKind::OpEq),

        // その他:

        IlKind::Ident(string_id) => {
            let ident = t.get_string(string_id);
            match a.globals.find(ident) {
                Some(global_id) => a.new_instr(InstrKind::Int(global_id as i64)),
                None => panic!("Unknown ident {}", ident),
            }
        }
    }
}

pub(crate) fn assemble(t: &IlTree) -> AlAsm {
    let mut a = AlAsm::new();

    gen_il(t.root(), t, &mut a);
    a.instrs.push(InstrKind::Exit);

    a
}
