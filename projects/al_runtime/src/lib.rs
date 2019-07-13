//! ランタイム

pub(crate) mod al_asm;
pub(crate) mod il;
pub(crate) mod object;
pub(crate) mod vm;

use al_asm::*;
use object::*;
use vm::*;

// 中間言語を実行する。
pub fn run(il_text: &str) -> ! {
    let il_tree = il::il_parse::parse(il_text);
    let (inss, global_count) = al_asm::assemble::assemble(&il_tree);

    let mut vm = VM::new();
    vm.inss = inss;
    vm.heap_alloc(global_count);
    vm.run()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
