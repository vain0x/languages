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

    eprintln!("{}", program);

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
fn test_read_int() {
    eval_tests(
        r#"
            println_int(read_int() + 1)
        "#,
        &[("41", "42\n"), ("-1", "0\n")],
    )
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
