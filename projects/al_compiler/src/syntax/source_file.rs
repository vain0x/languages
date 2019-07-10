use std::path::PathBuf;

#[derive(Clone, Debug)]
pub(crate) struct SourceFile {
    path: PathBuf,
    text: String,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct SourceLocation {
    file: usize,
    start: usize,
    end: usize,
}

impl SourceLocation {
    pub(crate) fn new(file: usize, start: usize, end: usize) -> Self {
        assert!(start <= end);
        SourceLocation { file, start, end }
    }
}
