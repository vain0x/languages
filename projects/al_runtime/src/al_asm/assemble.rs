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

fn gen_ins(il: usize, a: &mut AlAsm<'_>) {
    match a.il().kind(il) {
        IlKind::Globals => return gen_globals(il, a),
        _ => {}
    }

    for ci in 0..a.il().child_len(il) {
        gen_ins(a.il().child(il, ci), a);
    }

    match a.il().kind(il) {
        // 宣言:

        IlKind::Root | IlKind::CodeSection | IlKind::Globals => {}

        // 文:

        IlKind::Semi => {}
        IlKind::Assert => a.new_ins(InsKind::Assert),
        IlKind::CellSet => a.new_ins(InsKind::CellSet),

        // 式:

        IlKind::Bool(value) => a.new_ins(InsKind::Bool(value)),
        IlKind::Int(value) => a.new_ins(InsKind::Int(value)),
        IlKind::GlobalGet => a.new_ins(InsKind::GlobalGet),
        IlKind::OpAdd => a.new_ins(InsKind::OpAdd),
        IlKind::OpSub => a.new_ins(InsKind::OpSub),
        IlKind::OpMul => a.new_ins(InsKind::OpMul),
        IlKind::OpDiv => a.new_ins(InsKind::OpDiv),
        IlKind::OpEq => a.new_ins(InsKind::OpEq),

        // その他:

        IlKind::Ident(string_id) => {
            let ident = a.il().get_string(string_id);
            match a.globals.find(ident) {
                Some(global_id) => a.new_ins(InsKind::Int(global_id as i64)),
                None => panic!("Unknown ident {}", ident),
            }
        }
    }
}

pub(crate) fn assemble(t: &IlTree) -> AlAsm<'_> {
    let mut a = AlAsm::new(t);

    gen_ins(a.il().root(), &mut a);
    a.inss.push(InsKind::Exit);

    a
}
