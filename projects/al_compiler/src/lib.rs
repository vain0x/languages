use std::fs;
use std::io;

pub struct Output {
    pub il: String,
    pub msg: String,
}

pub fn build(entry_path: &str) -> io::Result<Output> {
    let source = fs::read_to_string(entry_path)?;
    Ok(Output {
        il: source,
        msg: String::new(),
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
