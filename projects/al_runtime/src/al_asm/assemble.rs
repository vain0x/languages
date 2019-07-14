//! 中間言語からアセンブリへの変換

use super::*;
use al_aux::il::*;

fn gen_globals(il: usize, a: &mut AlAsm<'_>) {
    for ci in 0..a.il().child_len(il) {
        let child = a.il().child(il, ci);
        match a.il().kind(child) {
            IlKind::Ident(string_id) => {
                let ident = a.il().get_string(string_id).to_owned();
                a.globals.new_global(ident);
            }
            kind => unreachable!(
                "globals の子ノードはすべて識別子でなければいけない {:?}",
                kind
            ),
        }
    }
}

fn gen_il(il: usize, a: &mut AlAsm<'_>) {
    match a.il().kind(il) {
        IlKind::Globals => return gen_globals(il, a),
        _ => {}
    }

    for ci in 0..a.il().child_len(il) {
        gen_il(a.il().child(il, ci), a);
    }

    match a.il().kind(il) {
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
            let ident = a.il().get_string(string_id);
            match a.globals.find(ident) {
                Some(global_id) => a.new_instr(InstrKind::Int(global_id as i64)),
                None => panic!("Unknown ident {}", ident),
            }
        }
    }
}

pub(crate) fn assemble(t: &IlTree) -> AlAsm<'_> {
    let mut a = AlAsm::new(t);

    gen_il(a.il().root(), &mut a);
    a.instrs.push(InstrKind::Exit);

    a
}
