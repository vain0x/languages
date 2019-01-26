#![cfg(test)]

extern crate atlisp;

use std::io;

fn eval(src: &str, stdin: &str) -> String {
    let program = atlisp::compile(src);
    let mut stdout = Vec::new();
    atlisp::exec(program, io::Cursor::new(stdin), &mut stdout);
    String::from_utf8(stdout).unwrap()
}

fn eval_tests(src: &str, ios: &[(&str, &str)]) {
    let program = atlisp::compile(src);

    for &(input, expected) in ios {
        let mut stdout = Vec::new();
        atlisp::exec(program.clone(), io::Cursor::new(input), &mut stdout);
        let actual = String::from_utf8(stdout).unwrap();

        assert_eq!(actual, expected, "src={}\ninput={}", src, input);
    }
}

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
fn test_set() {
    assert_eq!(
        eval(r#"(+ (let a 2) (set a (+ a 3)) (println (to_str a)))"#, ""),
        "5\n"
    )
}

#[test]
fn test_string_primitives() {
    assert_eq!(
        eval(r#"(println (to_str 42) " is answer")"#, ""),
        "42 is answer\n"
    );
    assert_eq!(eval(r#"(println (++ "a" "b"))"#, ""), "ab\n");
}

#[test]
fn test_cond() {
    eval_tests(
        r#"(cond (== (read_int) 1) (println "YES") (println "NO"))"#,
        &[("1", "YES\n"), ("0", "NO\n")],
    );
}

/// <https://atcoder.jp/contests/abs/tasks/practice_1?lang=en>
#[test]
fn test_welcome_to_atcoder() {
    eval_tests(
        r#"(+
            (let a (read_int))
            (let b (read_int))
            (let c (read_int))
            (let s (read_str))
            (println (to_str (+ a b c)) " " s)
        )"#,
        &[
            ("1\n2 3\ntest\n", "6 test\n"),
            ("72\n128 256\nmyonmyon\n", "456 myonmyon\n"),
        ],
    )
}

/// https://atcoder.jp/contests/abs/tasks/abc086_a?lang=en
#[test]
fn test_abc086_a_product() {
    eval_tests(
        r#"(+
            (let a (read_int))
            (let b (read_int))
            (println (cond (== (% (* a b) 2) 0) "Even" "Odd"))
        )"#,
        &[("3 4\n", "Even\n"), ("1 21\n", "Odd\n")],
    )
}

/// https://atcoder.jp/contests/abs/tasks/abc081_a?lang=en
#[test]
fn test_abc081_a_placing_marbles() {
    eval_tests(
        r#"(+
            (let n (read_int))
            (let k 0)
            (cond (== (% n 10) 1) (set k (+ k 1)))
            (set n (/ n 10))
            (cond (== (% n 10) 1) (set k (+ k 1)))
            (set n (/ n 10))
            (cond (== (% n 10) 1) (set k (+ k 1)))
            (println (to_str k))
        )"#,
        &[
            ("101\n", "2\n"),
            ("000\n", "0\n"),
            ("111\n", "3\n"),
            ("010\n", "1\n"),
        ],
    )
}
