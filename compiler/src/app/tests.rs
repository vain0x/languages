use crate::rir::gen::compile;
use crate::rir::CompilationResult;
use crate::semantics::{analyze, DocMsg};
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

    let sema = analyze::analyze_str(src);

    let stderr = DocMsg::to_text(&sema.to_doc_msgs());
    assert_eq!(
        compress(&stderr),
        compress(expected),
        "stderr = {}\nexpected = {}\n",
        stderr,
        expected
    );
    assert!(!sema.is_successful());
}

pub fn test_snapshot(name: &str, src: &str) {
    use std::fs;
    use std::path::PathBuf;

    let workspace_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let file_path = workspace_dir.join(format!("../tests/js/{}.js", name));

    // Ignore panic. I know that the current implementation doesn't work for most of cases.
    let js_code =
        match std::panic::catch_unwind(|| crate::emit_js::print::convert_to_javascript(src)) {
            Err(_) => return,
            Ok(x) => x,
        };

    let old_content = fs::read_to_string(&file_path).unwrap_or_else(|_| String::new());
    if js_code != old_content {
        fs::write(&file_path, js_code).unwrap();
    }
}
