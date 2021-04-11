pub(crate) mod xir_gen;

#[allow(unused)]
pub(crate) mod internals {
    pub(crate) use bumpalo::{boxed::Box as BumpBox, collections::Vec as BumpVec, Bump};
    pub(crate) use std::collections::HashMap;
    pub(crate) use std::fmt::{self, Debug, Display};
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
        let source_code = "assert(0)";

        let ast = parse_from_string(source_code, bump).unwrap();
        let mut mir = XProgram::new_in(bump);
        mir.push(AModule {
            name: "eval_test",
            path: "./eval_test",
            source_code,
            root: ast,
        });

        let actual = mir
            .bodies
            .iter()
            .map(|body| format!("{}", body.last))
            .collect::<Vec<_>>()
            .join("\n");
        expect![[r#""#]]
            .assert_eq(&actual);
    }
}
