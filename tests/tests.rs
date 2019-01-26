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
fn test_shadowing() {
    eval_tests(
        r#"(begin
            (let a 1)
            (begin
                (println (to_str a))
                (let a 2)
                (set a 3)
                (println (to_str a)))
            (println (to_str a))
        )"#,
        &[("", "1\n3\n1\n")],
    )
}

#[test]
fn test_cond() {
    eval_tests(
        r#"(cond (== (read_int) 1) (println "YES") (println "NO"))"#,
        &[("1", "YES\n"), ("0", "NO\n")],
    );
}

#[test]
fn test_while() {
    eval_tests(
        r#"(+
            (let N (read_int))
            (let i 0)
            (while (< i N) (+
                (println (to_str i))
                (set i (+ i 1))
            ))"#,
        &[("0\n", ""), ("1\n", "0\n"), ("3\n", "0\n1\n2\n")],
    )
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

/// <https://atcoder.jp/contests/abs/tasks/abc086_a?lang=en>
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

/// <https://atcoder.jp/contests/abs/tasks/abc081_a?lang=en>
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

/// <https://atcoder.jp/contests/abs/tasks/abc081_b?lang=en>
#[test]
fn test_abc081_b_shift_only() {
    eval_tests(
        r#"(+
            (let N (read_int))
            (let min_count 1000000000)
            (let i 0)
            (while (< i N) (+
                (let A (read_int))
                (let count 0)
                (while (== (% A 2) 0) (+
                    (set A (/ A 2))
                    (set count (+ count 1))
                ))
                (cond (> min_count count) (+
                    (set min_count count)
                ))
                (set i (+ i 1))
            ))
            (println (to_str min_count))
        )"#,
        &[("3\n8 12 40\n", "2\n"), ("4\n5 6 8 10\n", "0\n")],
    )
}

/// <https://atcoder.jp/contests/abs/tasks/abc087_b?lang=en>
#[test]
fn test_abc087_b_coins() {
    eval_tests(
        r#"(+
            (let A (read_int))
            (let B (read_int))
            (let C (read_int))
            (let X (read_int))
            (let k 0)
            (let a 0)
            (while (<= a A) (+
                (let b 0)
                (while (<= b B) (+
                    (let Y (- X (* a 500)))
                    (set Y (- Y (* b 100)))
                    (cond (<= 0 Y) (+
                        (let c (/ Y 50))
                        (cond (<= c C) (set k (+ k 1)))
                    ))
                    (set b (+ b 1))
                ))
                (set a (+ a 1))
            ))
            (println (to_str k))
        )"#,
        &[
            ("2\n2\n2\n100\n", "2\n"),
            ("5\n1\n0\n150\n", "0\n"),
            ("30\n40\n50\n6000\n", "213\n"),
        ],
    )
}
