use crate::il::*;
use crate::syntax::*;

fn gen_expr(ast: &Ast, codes: &mut Vec<Code>) {
    match ast.kind() {
        AstKind::True => codes.push(Code::PushTrue),
        AstKind::Ident(_) => unimplemented!(),
        AstKind::Call => match ast.children() {
            [cal, arg] => match cal.kind() {
                AstKind::Ident(cal_ident) if cal_ident == "assert" => {
                    gen_expr(&arg, codes);
                    codes.push(Code::Assert);
                }
                _ => unimplemented!("{:?}", cal.kind()),
            },
            _ => unreachable!(),
        },
        AstKind::Semi => {
            for child in ast.children() {
                gen_expr(child, codes);
            }
        }
    }
}

pub(crate) fn gen(ast: &Ast) -> Vec<Code> {
    let mut codes = vec![];
    gen_expr(ast, &mut codes);
    codes.push(Code::Exit);
    codes
}
