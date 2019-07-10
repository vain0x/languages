//! Original test runner.

use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

const TIMEOUT_MILLIS: u64 = 2000;
const INTERVAL_MILLIS: u64 = 50;

struct TestCase {
    project_name: String,
    project_path: PathBuf,
    entry_path: PathBuf,
    il_path: PathBuf,
    compile_success: bool,
    test_success: bool,
}

struct TestResult {
    pass_count: usize,
    test_count: usize,
}

fn compile_tests(compiler_path: &Path, cases: &mut [TestCase]) -> Result<(), io::Error> {
    use std::process::{Command, Stdio};

    let mut children = vec![];

    for case in cases {
        let child = Command::new(compiler_path)
            .args(vec![case.entry_path.to_owned()])
            .current_dir(&case.project_path)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()?;
        children.push(Some((child, case)));
    }

    for _ in 0..TIMEOUT_MILLIS / INTERVAL_MILLIS + 1 {
        std::thread::sleep(std::time::Duration::from_millis(INTERVAL_MILLIS));

        for child_opt in &mut children {
            if let Some((child, case)) = child_opt {
                match child.try_wait()? {
                    None => continue,
                    Some(exit_status) => {
                        if exit_status.code() == Some(0) {
                            let stdout = child.stdout.as_mut().unwrap();
                            let mut buffer = vec![];
                            stdout.read_to_end(&mut buffer).unwrap();
                            std::fs::write(&case.il_path, buffer).unwrap();
                            case.compile_success = true;
                        }
                        child_opt.take();
                    }
                }
            }
        }

        if children.iter().all(|child| child.is_none()) {
            break;
        }
    }

    Ok(())
}

fn run_tests(runtime_path: &Path, cases: &mut [TestCase]) -> Result<(), io::Error> {
    use std::process::{Command, Stdio};

    let mut children = vec![];

    for case in cases {
        if !case.compile_success {
            continue;
        }

        let child = Command::new(runtime_path)
            .args(vec![case.il_path.to_owned()])
            .current_dir(&case.project_path)
            .stdin(Stdio::null())
            .stderr(Stdio::inherit())
            .spawn()?;
        children.push(Some((child, case)));
    }

    for _ in 0..TIMEOUT_MILLIS / INTERVAL_MILLIS + 1 {
        std::thread::sleep(std::time::Duration::from_millis(INTERVAL_MILLIS));

        for child_opt in &mut children {
            if let Some((child, case)) = child_opt {
                match child.try_wait()? {
                    None => continue,
                    Some(exit_status) => {
                        if exit_status.code() == Some(0) {
                            case.test_success = true;
                        }
                        child_opt.take();
                    }
                }
            }
        }

        if children.iter().all(|child| child.is_none()) {
            break;
        }
    }

    Ok(())
}

fn find_files(tests_dir: &Path) -> Result<Vec<TestCase>, io::Error> {
    let mut cases = vec![];

    for dir in fs::read_dir(&tests_dir)? {
        let project_path = dir?.path();
        let project_name = project_path
            .file_stem()
            .and_then(|name| name.to_str())
            .unwrap()
            .to_owned();
        let entry_path = project_path.join(format!("{}{}", project_name, ".altery"));
        let il_path = project_path.join(format!("{}{}", project_name, ".altery_il"));

        cases.push(TestCase {
            project_name,
            project_path,
            entry_path,
            il_path,
            compile_success: false,
            test_success: false,
        });
    }

    Ok(cases)
}

fn print_result(cases: &[TestCase]) -> i32 {
    let mut pass_count = 0;

    for case in cases {
        if !case.compile_success {
            eprintln!("{}..compile error", case.project_name);
            continue;
        }
        if !case.test_success {
            eprintln!("{}..test failure", case.project_name);
            continue;
        }
        pass_count += 1;
    }

    let result = TestResult {
        pass_count,
        test_count: cases.len(),
    };

    if result.test_count == 0 {
        eprintln!("No test found");
        1
    } else if result.pass_count == result.test_count {
        eprintln!("All {} tests passed. Congratulations!", result.pass_count);
        0
    } else {
        let fail_count = result.test_count - result.pass_count;
        eprintln!(
            "{} passed / {} failed / {} total",
            result.pass_count, fail_count, result.test_count
        );
        1
    }
}

pub fn main() {
    let project_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let tests_dir = project_dir.join("../../tests");
    let compiler_path = project_dir.join("../../target/debug/alteryc");
    let runtime_path = project_dir.join("../../target/debug/altery");

    let mut cases = find_files(&tests_dir).unwrap();
    compile_tests(&compiler_path, &mut cases).unwrap();
    run_tests(&runtime_path, &mut cases).unwrap();
    print_result(&cases);
}
