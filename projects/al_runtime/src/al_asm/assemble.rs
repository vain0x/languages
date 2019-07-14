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

fn gen_labels(il: usize, t: &IlTree, a: &mut AlAsm) {
    for ci in 0..t.child_len(il) {
        let child = t.child(il, ci);
        match t.kind(child) {
            IlKind::Ident(string_id) => {
                let ident = t.get_string(string_id).to_owned();
                a.labels.new_label(ident);
            }
            kind => unreachable!(
                "labels の子ノードはすべて識別子でなければいけない {:?}",
                kind
            ),
        }
    }
}

fn gen_label_def(il: usize, t: &IlTree, a: &mut AlAsm) {
    assert_eq!(t.child_len(il), 1);
    match t.kind(t.child(il, 0)) {
        IlKind::Ident(string_id) => {
            let ident = t.get_string(string_id);
            let label = a.labels.find(ident).expect("ラベルは定義済みのはず");
            a.labels.def_label(label, a.instrs.len());
        }
        kind => unreachable!(
            "label_def の子ノードは識別子でなければいけない {:?}",
            kind
        ),
    }
}

fn gen_global_get(il: usize, t: &IlTree, a: &mut AlAsm) {
    assert_eq!(t.child_len(il), 1);
    match t.kind(t.child(il, 0)) {
        IlKind::Ident(string_id) => {
            let ident = t.get_string(string_id);
            let global_id = a
                .globals
                .find(ident)
                .expect("グローバル変数は定義済みのはず");
            a.new_instr(InstrKind::Int(global_id as i64));
            a.new_instr(InstrKind::GlobalGet);
        }
        kind => unreachable!(
            "global_get の子ノードは識別子でなければいけない {:?}",
            kind
        ),
    }
}

fn gen_label_get(il: usize, t: &IlTree, a: &mut AlAsm) {
    assert_eq!(t.child_len(il), 1);
    match t.kind(t.child(il, 0)) {
        IlKind::Ident(string_id) => {
            let ident = t.get_string(string_id);
            let label = a.labels.find(ident).expect("ラベルは定義済みのはず");
            a.labels.use_label(label, a.instrs.len());
            a.new_instr(InstrKind::Label(label));
        }
        kind => unreachable!(
            "label_get の子ノードは識別子でなければいけない {:?}",
            kind
        ),
    }
}

fn gen_il(il: usize, t: &IlTree, a: &mut AlAsm) {
    match t.kind(il) {
        IlKind::Globals => return gen_globals(il, t, a),
        IlKind::Labels => return gen_labels(il, t, a),
        IlKind::LabelDef => return gen_label_def(il, t, a),
        IlKind::GlobalGet => return gen_global_get(il, t, a),
        IlKind::LabelGet => return gen_label_get(il, t, a),
        _ => {}
    }

    for ci in 0..t.child_len(il) {
        gen_il(t.child(il, ci), t, a);
    }

    match t.kind(il) {
        // 文:
        IlKind::Semi => {}
        IlKind::Jump => a.new_instr(InstrKind::Jump),
        IlKind::JumpUnless => a.new_instr(InstrKind::JumpUnless),
        IlKind::Pop => a.new_instr(InstrKind::Pop),
        IlKind::Assert => a.new_instr(InstrKind::Assert),
        IlKind::CellSet => a.new_instr(InstrKind::CellSet),

        // 式:
        IlKind::Bool(value) => a.new_instr(InstrKind::Bool(value)),
        IlKind::Int(value) => a.new_instr(InstrKind::Int(value)),
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

        IlKind::Root
        | IlKind::CodeSection
        | IlKind::Globals
        | IlKind::Labels
        | IlKind::LabelDef
        | IlKind::GlobalGet
        | IlKind::LabelGet => {}
    }
}

pub(crate) fn assemble(t: &IlTree) -> AlAsm {
    let mut a = AlAsm::new();

    gen_il(t.root(), t, &mut a);
    a.instrs.push(InstrKind::Exit);

    a.resolve_labels();
    a
}
