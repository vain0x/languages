//! 中間言語からアセンブリへの変換

use super::*;
use al_aux::il::*;

fn gen_il(il: usize, t: &IlTree, a: &mut AlAsm, idents: &mut Vec<usize>) {
    for ci in 0..t.child_len(il) {
        gen_il(t.child(il, ci), t, a, idents);
    }

    match t.kind(il) {
        // 宣言:
        IlKind::Globals => {
            // (globals $a ..) の中身がすべてスタックに積まれているので、それらをグローバル変数として宣言する。
            for &ident in idents.iter() {
                let ident = t.get_string(ident).to_owned();
                a.globals.new_global(ident);
            }
            idents.clear();
        }
        IlKind::Labels => {
            // globals と同様
            for &ident in idents.iter() {
                let ident = t.get_string(ident).to_owned();
                a.labels.new_label(ident);
            }
            idents.clear();
        }
        IlKind::LabelDef => {
            let ident = idents.pop().expect("label_def は識別子を1つ持つはず");
            let ident = t.get_string(ident);
            let label = a.labels.find(ident).unwrap_or_else(|| {
                panic!(
                    "labels に列挙されていないラベルがあります {}",
                    ident
                )
            });
            a.labels.def_label(label, a.instrs.len());
        }

        // 文:
        IlKind::Jump => a.new_instr(InstrKind::Jump),
        IlKind::JumpUnless => a.new_instr(InstrKind::JumpUnless),
        IlKind::Pop => a.new_instr(InstrKind::Pop),
        IlKind::Assert => a.new_instr(InstrKind::Assert),
        IlKind::CellSet => a.new_instr(InstrKind::CellSet),

        // 式:
        IlKind::Bool(value) => a.new_instr(InstrKind::Bool(value)),
        IlKind::Int(value) => a.new_instr(InstrKind::Int(value)),

        IlKind::GlobalGet => {
            let ident = idents.pop().expect("global_get は識別子を1つ持つはず");
            let ident = t.get_string(ident);
            let global_id = a.globals.find(ident).unwrap_or_else(|| {
                panic!(
                    "globals に列挙されていないグローバル変数があります {}",
                    ident
                )
            });
            a.new_instr(InstrKind::Int(global_id as i64));
            a.new_instr(InstrKind::GlobalGet);
        }
        IlKind::LabelGet => {
            let ident = idents.pop().expect("label_get は識別子を1つ持つはず");
            let ident = t.get_string(ident);
            let label = a.labels.find(ident).unwrap_or_else(|| {
                panic!(
                    "labels に列挙されていないラベルがあります {}",
                    ident
                )
            });
            a.labels.use_label(label, a.instrs.len());
            a.new_instr(InstrKind::Label(label));
        }

        IlKind::OpAdd => a.new_instr(InstrKind::OpAdd),
        IlKind::OpSub => a.new_instr(InstrKind::OpSub),
        IlKind::OpMul => a.new_instr(InstrKind::OpMul),
        IlKind::OpDiv => a.new_instr(InstrKind::OpDiv),
        IlKind::OpEq => a.new_instr(InstrKind::OpEq),

        // その他:
        IlKind::Ident(ident_id) => idents.push(ident_id),

        IlKind::Root | IlKind::CodeSection | IlKind::Semi => {}
    }
}

pub(crate) fn assemble(t: &IlTree) -> AlAsm {
    let mut a = AlAsm::new();
    let mut idents = vec![];

    gen_il(t.root(), t, &mut a, &mut idents);
    a.instrs.push(InstrKind::Exit);

    a.resolve_labels();
    a
}
