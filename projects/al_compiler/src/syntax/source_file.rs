use std::path::PathBuf;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct SourceLocation {
    file: usize,
    start: usize,
    end: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct SourceFile {
    path: PathBuf,
    text: String,
}

pub(crate) struct SourceFileSystem {
    files: Vec<SourceFile>,
}

impl SourceLocation {
    pub(crate) fn new(file: usize, start: usize, end: usize) -> Self {
        assert!(start <= end);
        SourceLocation { file, start, end }
    }

    pub(crate) fn file(&self) -> usize {
        self.file
    }

    pub(crate) fn start(&self) -> usize {
        self.start
    }

    pub(crate) fn end(&self) -> usize {
        self.end
    }
}

impl SourceFile {
    pub(crate) fn new(path: PathBuf, text: String) -> SourceFile {
        SourceFile { path, text }
    }

    pub(crate) fn text(&self) -> &str {
        &self.text
    }
}

impl SourceFileSystem {
    pub(crate) fn new() -> SourceFileSystem {
        SourceFileSystem { files: vec![] }
    }

    pub(crate) fn add_file(&mut self, file: SourceFile) -> usize {
        let file_id = self.files.len();
        self.files.push(file);
        file_id
    }

    pub(crate) fn file_text(&self, file: usize) -> &str {
        self.files[file].text()
    }

    pub(crate) fn loc_text(&self, loc: &SourceLocation) -> &str {
        &self.file_text(loc.file())[loc.start()..loc.end()]
    }
}
