//! Original test runner.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

const TIMEOUT_MILLIS: u64 = 2000;
const INTERVAL_MILLIS: u64 = 50;

struct TestCase {
    project_path: PathBuf,
    entry_path: PathBuf,
}

struct TestResult {
    pass_count: usize,
    test_count: usize,
}

fn run_test(runtime_path: &Path, cases: &[TestCase]) -> Result<TestResult, io::Error> {
    use std::process::{Command, Stdio};
    let mut children = vec![];
    let mut pass_count = 0;
    let test_count = cases.len();

    for case in cases {
        let child = Command::new(runtime_path)
            .args(vec![case.entry_path.to_owned()])
            .current_dir(&case.project_path)
            .stdin(Stdio::null())
            .stderr(Stdio::inherit())
            .spawn()?;
        children.push(Some(child));
    }

    for _ in 0..TIMEOUT_MILLIS / INTERVAL_MILLIS + 1 {
        std::thread::sleep(std::time::Duration::from_millis(INTERVAL_MILLIS));

        for child_opt in &mut children {
            if let Some(child) = child_opt {
                match child.try_wait()? {
                    None => continue,
                    Some(exit_status) => {
                        if exit_status.code() == Some(0) {
                            pass_count += 1;
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

    Ok(TestResult {
        test_count,
        pass_count,
    })
}

fn find_files(tests_dir: &Path) -> Result<Vec<TestCase>, io::Error> {
    let mut cases = vec![];

    for dir in fs::read_dir(&tests_dir)? {
        let project_path = dir?.path();
        let project_name = project_path.file_stem().unwrap();
        let entry_path =
            project_path.join(format!("{}{}", project_name.to_str().unwrap(), ".altery"));

        cases.push(TestCase {
            project_path,
            entry_path,
        });
    }

    Ok(cases)
}

fn print_result(result: TestResult) -> i32 {
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
    let runtime_path = project_dir.join("../../target/debug/altery");

    let cases = find_files(&tests_dir).unwrap();
    let result = run_test(&runtime_path, &cases).unwrap();
    print_result(result);
}
