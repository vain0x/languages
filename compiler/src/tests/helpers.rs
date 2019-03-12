use crate::mir::gen_mir::compile;
use crate::mir::CompilationResult;
use crate::msg::DocMsg;
use std::io;

pub fn eval_tests(src: &str, ios: &[(&str, &str)]) {
    let CompilationResult {
        success,
        program,
        msgs,
        ..
    } = compile(src);

    if !success {
        let stderr = DocMsg::to_text(&msgs);
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

pub fn test_err(src: &str, expected: &str) {
    let compress = |s: &str| s.replace(|c: char| c.is_ascii_whitespace(), "");

    let CompilationResult { success, msgs, .. } = compile(src);

    let stderr = DocMsg::to_text(&msgs);
    assert_eq!(
        compress(&stderr),
        compress(expected),
        "stderr = {}\nexpected = {}\n",
        stderr,
        expected
    );
    assert!(!success);
}
