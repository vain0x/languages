#![cfg(test)]

use atlisp;

#[test]
fn test() {
    assert_eq!(atlisp::eval("(println (read_int))", "42".into()), "42\n");
}
