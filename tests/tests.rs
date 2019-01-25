#![cfg(test)]

use atlisp::eval;

#[test]
fn test_read_int() {
    assert_eq!(eval("(println (to_str (read_int)))", "42"), "42\n");
}

#[test]
fn test_bin_op() {
    assert_eq!(eval("(println (to_str (+ 2 3)))", ""), "5\n");
    assert_eq!(eval("(println (to_str (- 7 2 1)))", ""), "4\n");
}

#[test]
fn test_app_eval_order() {
    assert_eq!(
        eval("(println (to_str (- (read_int) (read_int))))", "42 3"),
        "39\n"
    );
}

#[test]
fn test_let() {
    assert_eq!(eval("(+ (let a 7) (println (to_str (* a a))))", ""), "49\n");
}

#[test]
fn test_string_primitives() {
    assert_eq!(
        eval(r#"(println (to_str 42) " is answer")"#, ""),
        "42 is answer\n"
    );
    assert_eq!(eval(r#"(println (++ "a" "b"))"#, ""), "ab\n");
}
