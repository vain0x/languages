use super::*;

pub(crate) fn parse_pil(text: &'static str, name: String, full_name: String) -> PilModData {
    let mut m = PilModData::new(name, full_name);
    let mut f = PilFnData::new("<root>");

    let mut stdout_check = String::new();

    for (_row, line) in text.lines().enumerate() {
        // コメント
        if line.starts_with("#") {
            if line[1..].starts_with("?") {
                let line = line[2..].trim();

                if line.starts_with("stdout:") {
                    let value = line["stdout:".len()..].trim();
                    stdout_check += value;
                    assert_ne!(value, "");
                    continue;
                }

                if line.starts_with("$?") {
                    let code = line
                        .split_whitespace()
                        .skip_while(|&s| s == "$?" || s == "=")
                        .next()
                        .expect("exit code")
                        .trim()
                        .parse::<i32>()
                        .expect("exit code parse as int");
                    let old_check = m.check.exit_code.replace(code);
                    assert!(old_check.is_none());
                    continue;
                }

                panic!("illegal check: #? {}", line);
            }
        }

        let stmt = line.split_whitespace().collect::<Vec<_>>();
        if !stmt.is_empty() {
            f.codes.push(stmt);
        }
    }

    m.fns.insert(f.name, f);

    m.check.stdout = if !stdout_check.is_empty() {
        Some(Cell::new(
            Box::leak(stdout_check.replace("\\n", "\n").into_boxed_str()) as &str,
        ))
    } else {
        None
    };

    m
}
