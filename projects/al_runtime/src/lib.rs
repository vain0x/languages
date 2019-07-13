//! ランタイム

pub(crate) mod il;
pub(crate) mod ins;
pub(crate) mod object;
pub(crate) mod runtime;

use ins::*;
use object::*;
use runtime::*;

// 中間言語を実行する。
pub fn run(il_text: &str) -> ! {
    let il_tree = il::il_parse::parse(il_text);
    let (inss, global_count) = ins::gen(&il_tree);

    let mut runtime = Runtime::new();
    runtime.inss = inss;
    runtime.heap_alloc(global_count);
    runtime.run()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
