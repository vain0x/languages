pub(crate) mod xir_eval;
pub(crate) mod xir_gen;

#[allow(unused)]
pub(crate) mod internals {
    pub(crate) use bumpalo::{
        boxed::Box as BumpBox, collections::Vec as BumpVec,
        core_alloc::string::String as BumpString, Bump,
    };
    pub(crate) use std::cell::RefCell;
    pub(crate) use std::collections::HashMap;
    pub(crate) use std::fmt::{self, Debug, Display};
    pub(crate) use std::mem::{replace, swap, take};
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{internals::*, xir_gen::XProgram};
    use expect_test::{expect, Expect};
    use joy_syntax::{parse_from_string, AModule};

    #[test]
    fn eval_test() {
        let bump = &Bump::new();
        let source_code = "assert(\"0\")";

        let ast = parse_from_string(source_code, bump).unwrap();
        let mir = bump.alloc(XProgram::new_in(bump));
        // mir.push(AModule {
        //     name: "eval_test",
        //     path: "./eval_test",
        //     source_code,
        //     root: ast,
        // });

        let mut gen = xir_gen::XirGen::new(mir, bump);
        gen.push_module("eval_test", source_code, bump.alloc(ast));

        let mut eval = xir_eval::XirEval::new(mir);
        eval.run();

        expect![[r#"
            debug: Local(XLocal { name: "_", alive: false })
        "#]]
        .assert_eq(&eval.output);
        // let actual = mir
        //     .bodies
        //     .iter()
        //     .map(|body| format!("{}", body.last))
        //     .collect::<Vec<_>>()
        //     .join("\n");
        // expect![[r#""#]].assert_eq(&actual);
    }
}
