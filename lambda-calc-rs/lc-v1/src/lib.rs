// mod ast {
//     pub(crate) mod a_tree;
// }

// mod parse {
//     pub(crate) mod parse_decls;
//     pub(crate) mod parse_exprs;
//     pub(crate) mod parse_root;
//     pub(crate) mod parser;
// }

mod syntax {
    pub(crate) mod syntax_token;
}

mod token {
    pub(crate) mod token_data;
    pub(crate) mod token_kind;
    pub(crate) mod tokenize_rules;
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
