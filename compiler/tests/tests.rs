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
