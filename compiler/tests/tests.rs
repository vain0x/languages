#![cfg(test)]

extern crate picomet_lang_compiler;
extern crate picomet_lang_runtime;

use picomet_lang_compiler::CompilationResult;
use std::io;

fn eval_tests(src: &str, ios: &[(&str, &str)]) {
    let CompilationResult {
        success,
        program,
        stderr,
    } = picomet_lang_compiler::compile(src);
    if !stderr.is_empty() {
        eprintln!("{}", stderr);
    }
    assert!(success, "src={} program={}", src, program);

    for &(input, expected) in ios {
        let mut stdout = Vec::new();
        picomet_lang_runtime::eval(&program, io::Cursor::new(input), &mut stdout);
        let actual = String::from_utf8(stdout).unwrap();

        assert_eq!(
            actual, expected,
            "src={}\ninput={}\nprogram={}",
            src, input, program
        );
    }
}

fn test_err(src: &str, expected: &str) {
    let compress = |s: &str| s.replace(|c: char| c.is_ascii_whitespace(), "");

    let CompilationResult {
        success, stderr, ..
    } = picomet_lang_compiler::compile(src);

    assert_eq!(
        compress(&stderr),
        compress(expected),
        "stderr = {}\nexpected = {}\n",
        stderr,
        expected
    );
    assert!(!success);
}

#[test]
fn test_hello() {
    eval_tests(
        r#"
            println_int(42)
        "#,
        &[("", "42\n")],
    )
}

#[test]
fn test_arithmetic() {
    eval_tests(
        r#"
            println_int(1 + 2 * 3)
        "#,
        &[("", "7\n")],
    )
}

#[test]
fn test_prefix_parsing() {
    eval_tests(
        r#"
            println_int(-1)
        "#,
        &[("", "-1\n")],
    )
}

#[test]
fn test_comparison() {
    eval_tests(
        r#"
            let l = read_int();
            let r = read_int();
            println_int(if l == r { 1 } else { 0 });
            println_int(if l != r { 1 } else { 0 });
            println_int(if l < r { 1 } else { 0 });
            println_int(if l <= r { 1 } else { 0 });
            println_int(if l > r { 1 } else { 0 });
            println_int(if l >= r { 1 } else { 0 });
        "#,
        &[
            ("1\n1\n", "1\n0\n0\n1\n0\n1\n"),
            ("1\n2\n", "0\n1\n1\n1\n0\n0\n"),
        ],
    )
}

#[test]
fn test_arithmetic_type_error() {
    test_err(
        "1 + (while 0 == 0 {})",
        r#"
            At 1:6..1:21 Type Error
            At 1:3..1:22 Type Error
        "#,
    )
}

#[test]
fn test_str() {
    eval_tests(
        r#"
            let i = read_int();
            println_int(byte_to_int("Hello, world!"[i]))
        "#,
        &[("0", "72\n"), ("1", "101\n")],
    )
}

#[test]
fn test_write() {
    eval_tests(
        r#"
            let p = mem_alloc(2);
            p[0] = "A"[0];
            p[1] = int_to_byte(10);
            print(p, 2);
        "#,
        &[("", "A\n")],
    )
}

#[test]
fn test_read_int() {
    eval_tests(
        r#"
            println_int(read_int() + 1)
        "#,
        &[("41", "42\n"), ("-1", "0\n")],
    );

    test_err(r#"println_int()"#, "At 1:1..1:12 Type Error");
}

#[test]
fn test_local_var() {
    eval_tests(
        r#"
            let a = 1
            println_int(a)
        "#,
        &[("", "1\n")],
    )
}

#[test]
fn test_local_var_two() {
    eval_tests(
        r#"
            let a = 1;
            let b = 2;
            println_int(a);
            println_int(b);
        "#,
        &[("", "1\n2\n")],
    )
}

#[test]
fn test_local_var_set() {
    eval_tests(
        r#"
            let a = 1;
            a = a + 1;
            println_int(a);
        "#,
        &[("", "2\n")],
    )
}

#[test]
fn test_local_var_set_add() {
    eval_tests(
        r#"
            let a = 1;
            a += 1;
            println_int(a);
        "#,
        &[("", "2\n")],
    )
}

#[test]
fn test_if() {
    eval_tests(
        r#"
            println_int(if read_int() == 0 {
                42
            } else {
                -1
            })
        "#,
        &[("0", "42\n"), ("1", "-1\n")],
    )
}

#[test]
fn test_if_else_if_chain() {
    eval_tests(
        r#"
            let x = read_int();
            println_int(if x == 1 {
                -1
            } else if x == 2 {
                -2
            } else {
                x
            })
        "#,
        &[("1", "-1\n"), ("2", "-2\n"), ("3", "3\n")],
    )
}

#[test]
fn test_while() {
    eval_tests(
        r#"
            while read_int() == 0 {
                println_int(1)
            }
        "#,
        &[("0\n0\n0\n4\n0\n", "1\n1\n1\n"), ("1\n", "")],
    )
}

#[test]
fn test_fun_with_no_args() {
    eval_tests(
        r#"
            let f = fun() 1;
            println_int(f());
        "#,
        &[("", "1\n")],
    )
}

#[test]
fn test_fun_with_arg() {
    eval_tests(
        r#"
            let half = fun(x) x / 2;
            println_int(half(84));
        "#,
        &[("", "42\n")],
    )
}

#[test]
fn test_fun_with_args() {
    eval_tests(
        r#"
            let div = fun(x, y) x / y;
            println_int(div(126, 3));
        "#,
        &[("", "42\n")],
    )
}

#[test]
fn test_fun_with_args_and_locals() {
    eval_tests(
        r#"
            let digit_sum = fun(x) {
                let s = 0;
                while x > 0 {
                    s += x % 10;
                    x = x / 10;
                }
                s
            };
            println_int(digit_sum(314159));
        "#,
        &[("", "23\n")],
    )
}

#[test]
fn test_fun_to_modify_globals() {
    eval_tests(
        r#"
            let x = 0;
            let f = fun() { x += 1 };
            f();
            println_int(x);
        "#,
        &[("", "1\n")],
    )
}
