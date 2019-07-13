use std::fs;
use std::io;
use std::path::PathBuf;

pub(crate) mod il;
pub(crate) mod semantics;
pub(crate) mod syntax;

use crate::syntax::*;

pub struct Output {
    pub il: String,
    pub msg: String,
}

pub fn build(entry_path: &str) -> io::Result<Output> {
    let entry_path = PathBuf::from(entry_path);
    let entry_text = fs::read_to_string(&entry_path)?;
    let mut sources = SourceFileSystem::new();
    let file = sources.add_file(SourceFile::new(entry_path, entry_text));

    let ast = crate::syntax::parse::parse(file, sources.file_text(file));
    let expr = crate::semantics::from_ast::from_ast(&ast);
    let codes = crate::il::from_expr::from_expr(&expr, &sources);
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
