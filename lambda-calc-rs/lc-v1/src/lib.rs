mod ast {
    pub(crate) mod a_parser;
    pub(crate) mod a_tree;
}

mod parse {
    pub(crate) mod parse_decls;
    pub(crate) mod parse_exprs;
    pub(crate) mod parse_root;
    pub(crate) mod parser;
}

mod syntax {
    pub(crate) mod syntax_token;
    pub(crate) mod syntax_tree;
}

mod token {
    pub(crate) mod token_data;
    pub(crate) mod token_kind;
    pub(crate) mod tokenize_rules;
}

mod utils {
    pub(crate) use bumpalo::{boxed::Box as BumpaloBox, collections::Vec as BumpaloVec};
}

mod context {
    use crate::utils::*;

    #[derive(Copy, Clone)]
    pub(crate) struct ContextRef<'a> {
        context: &'a Context,
    }

    pub(crate) struct Context {
        pub(crate) bump: bumpalo::Bump,
    }

    impl Context {
        pub(crate) fn new() -> Self {
            Self {
                bump: bumpalo::Bump::new(),
            }
        }

        pub(crate) fn allocate_iter<T>(
            &self,
            iter: impl IntoIterator<Item = T>,
        ) -> BumpaloVec<'_, T> {
            BumpaloVec::from_iter_in(iter, &self.bump)
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn snapshot_test() {
        let actual = "OK!";
        let expect = expect![[r#"OK!"#]];
        expect.assert_eq(actual);
    }
}
