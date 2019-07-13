//! 中間言語から文字列への変換

use al_aux::il::*;
use std::io::{self, Write};

fn print_atom(kind: IlKind, t: &IlTree, out: &mut Vec<u8>) -> io::Result<()> {
    match kind {
        IlKind::Root => write!(out, "root")?,
        IlKind::CodeSection => write!(out, "code_section")?,
        IlKind::Globals => write!(out, "globals")?,

        IlKind::Semi => write!(out, "semi")?,
        IlKind::Assert => write!(out, "assert")?,
        IlKind::CellSet => write!(out, "cell_set")?,

        IlKind::Bool(value) => write!(out, "{}", value)?,
        IlKind::Int(value) => write!(out, "{}", value)?,
        IlKind::GlobalGet => write!(out, "global_get")?,
        IlKind::OpAdd => write!(out, "+")?,
        IlKind::OpSub => write!(out, "-")?,
        IlKind::OpMul => write!(out, "*")?,
        IlKind::OpDiv => write!(out, "/")?,
        IlKind::OpEq => write!(out, "==")?,

        IlKind::Ident(ident) => write!(out, "${}", t.get_string(ident))?,
    };
    Ok(())
}

fn print_node(il: usize, depth: usize, t: &IlTree, out: &mut Vec<u8>) -> io::Result<()> {
    let has_paren = match t.kind(il) {
        IlKind::Bool(_) | IlKind::Int(_) | IlKind::Ident(_) => false,
        _ => true,
    };

    if let Some(comment) = t.comment(il) {
        for line in comment.split("\n") {
            // Indent.
            for _ in 0..depth {
                write!(out, "  ")?;
            }

            write!(out, "// {}\n", line.trim_end())?;
        }
    }

    // Indent.
    for _ in 0..depth {
        write!(out, "  ")?;
    }

    if has_paren {
        write!(out, "(")?;
    }

    print_atom(t.kind(il), t, out)?;

    for ci in 0..t.child_len(il) {
        write!(out, "\n")?;
        print_node(t.child(il, ci), depth + 1, t, out)?;
    }

    if has_paren {
        write!(out, ")")?;
    }

    Ok(())
}

pub(crate) fn print(t: &IlTree) -> io::Result<Vec<u8>> {
    let mut out = Vec::new();

    print_node(t.root(), 0, t, &mut out)?;
    write!(out, "\n")?;

    Ok(out)
}
