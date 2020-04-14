use super::*;
use crate::cps::KRoot;
use std::io::{self, Write};

fn write_indent(indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    for _ in 0..indent * 4 {
        write!(out, " ")?;
    }
    Ok(())
}

fn write_ty(ty: &CTy, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    match ty {
        CTy::Void => write!(out, "void"),
        CTy::Bool => write!(out, "bool"),
        CTy::Int => write!(out, "int"),
        CTy::Enum { ident, variants } => {
            write!(out, "enum {}", ident)?;
            if let Some(variants) = variants {
                write!(out, " {{\n")?;
                for (ident, ty) in variants {
                    let indent = indent + 1;
                    write_indent(indent, out)?;
                    write!(out, "{} ", ident)?;
                    write_ty(ty, indent, out)?;
                    write!(out, ",\n")?;
                }
                write_indent(indent, out)?;
                write!(out, "}}")?;
            }
            Ok(())
        }
    }
}

fn write_expr(expr: &CExpr, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    match expr {
        CExpr::BoolLit(false) => write!(out, "false"),
        CExpr::BoolLit(true) => write!(out, "true"),
        CExpr::IntLit(value) => write!(out, "{}", value),
        CExpr::Name(name) => write!(out, "{}", name),
        CExpr::Call { cal, args } => {
            write_expr(cal, indent, out)?;
            write!(out, "(")?;

            let mut first = true;
            for arg in args {
                if !first {
                    write!(out, ", ")?;
                }
                first = false;

                write_expr(arg, indent, out)?;
            }
            write!(out, ")")
        }
        CExpr::Neg(arg) => {
            write!(out, "!")?;
            write_expr(arg, indent, out)
        }
        CExpr::Add(left, right) => {
            write!(out, "(")?;
            write_expr(left, indent, out)?;
            write!(out, " + ")?;
            write_expr(right, indent, out)?;
            write!(out, ")")
        }
    }
}

fn write_stmt(stmt: &CStmt, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    match stmt {
        CStmt::Expr(body) => {
            write_expr(body, indent, out)?;
            write!(out, ";")
        }
        CStmt::Block(body) => write_block(body, indent, out),
        CStmt::Return(None) => write!(out, "return;"),
        CStmt::Return(Some(arg)) => {
            write!(out, "return ")?;
            write_expr(arg, indent, out)?;
            write!(out, ";")
        }
        CStmt::If { cond, body } => {
            write!(out, "if (")?;
            write_expr(cond, indent, out)?;
            write!(out, ") ")?;
            write_stmt(body, indent, out)
        }
        CStmt::VarDecl { name, ty, init_opt } => {
            write_ty(ty, indent, out)?;

            match init_opt {
                Some(init) => {
                    write!(out, " {} = ", name)?;
                    write_expr(init, indent, out)?;
                    write!(out, ";")
                }
                None => write!(out, " {};", name),
            }
        }
        CStmt::FnDecl {
            ident,
            params,
            result_ty,
            body,
        } => {
            write_ty(result_ty, indent, out)?;
            write!(out, " {}(", ident)?;

            let mut first = true;
            for (param, ty) in params {
                if !first {
                    write!(out, ", ")?;
                }
                first = false;

                write_ty(ty, indent, out)?;
                write!(out, " {}", param)?;
            }

            write!(out, ") ")?;
            write_block(body, indent, out)
        }
    }
}

fn write_block(block: &CBlock, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    let body = &block.body;

    if body.is_empty() {
        write!(out, "{{}}")
    } else {
        write!(out, "{{\n")?;
        for stmt in body.iter() {
            let indent = indent + 1;
            write_indent(indent, out)?;
            write_stmt(stmt, indent, out)?;
            write!(out, "\n")?;
        }
        write_indent(indent, out)?;
        write!(out, "}}")
    }
}

fn write_root(root: &CRoot, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
    let mut first = false;
    for decl in &root.body {
        if !first {
            write!(out, "\n")?;
        }
        first = false;

        write_indent(indent, out)?;
        write_stmt(decl, indent, out)?;
        write!(out, "\n")?;
    }
    Ok(())
}

pub(crate) fn clang_dump(k_root: KRoot) -> String {
    let c_root = clang_gen::gen(k_root);

    let text = {
        let mut out = vec![];
        super::clang_dump::write_root(&c_root, 0, &mut out).unwrap();
        String::from_utf8_lossy(&out).to_string()
    };

    text
}