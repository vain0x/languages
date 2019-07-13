use std::fs;
use std::io;
use std::path::PathBuf;

pub(crate) mod il;
pub(crate) mod semantics;
pub(crate) mod syntax;

pub struct Output {
    pub il: String,
    pub msg: String,
}

pub fn build(entry_path: &str) -> io::Result<Output> {
    let file = 0;
    let text = fs::read_to_string(PathBuf::from(entry_path))?;

    let ast = crate::syntax::parse::parse(file, &text);
    let expr = crate::semantics::from_ast::from_ast(&ast);
    let codes = crate::il::from_expr::from_expr(&expr);
    let il = crate::il::print::print(&codes)?;

    Ok(Output {
        il: String::from_utf8(il).unwrap(),
        msg: String::new(),
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
