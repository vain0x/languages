//! 中間言語から文字列への変換

use al_aux::il::*;
use std::io::{self, Write};

fn print_atom(kind: IlKind, t: &IlTree, out: &mut Vec<u8>) -> io::Result<()> {
    match kind {
        IlKind::Int(value) => write!(out, "{}", value)?,
        IlKind::Ident(ident) => write!(out, "${}", t.get_string(ident))?,
        _ => {
            if let Some(text) = IlKind::texts()
                .into_iter()
                .filter_map(|&(text, k)| if k == kind { Some(text) } else { None })
                .next()
            {
                return write!(out, "{}", text);
            }
            unreachable!("Unknown kind {:?}", kind)
        }
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
