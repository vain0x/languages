use al_aux::il::*;
use std::io::{self, Write};

fn print_atom(kind: IlKind, out: &mut Vec<u8>) -> io::Result<()> {
    match kind {
        IlKind::Bool(value) => write!(out, "{}", value)?,
        IlKind::Int(value) => write!(out, "{}", value)?,
        IlKind::Exit => write!(out, "exit")?,
        IlKind::Semi => write!(out, "semi")?,
        IlKind::Assert => write!(out, "assert")?,
        IlKind::OpAdd => write!(out, "+")?,
        IlKind::OpSub => write!(out, "-")?,
        IlKind::OpMul => write!(out, "*")?,
        IlKind::OpDiv => write!(out, "/")?,
        IlKind::OpEq => write!(out, "==")?,
    };
    Ok(())
}

fn print_node(il: usize, depth: usize, t: &IlTree, out: &mut Vec<u8>) -> io::Result<()> {
    let has_paren = match t.kind(il) {
        IlKind::Bool(_) | IlKind::Int(_) => false,
        _ => true,
    };

    for _ in 0..depth {
        write!(out, "  ")?;
    }

    if has_paren {
        write!(out, "(")?;
    }

    print_atom(t.kind(il), out)?;

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
