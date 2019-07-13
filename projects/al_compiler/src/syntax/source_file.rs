//! ソースファイル
//!
//! ソースファイルは ID で管理する。

use std::cmp::{max, min};
use std::path::PathBuf;

/// ソースファイル上の範囲
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct SourceLocation {
    file: usize,

    /// 開始位置 (バイト単位)
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

    pub(crate) fn union(&self, other: &SourceLocation) -> SourceLocation {
        assert_eq!(self.file(), other.file());
        SourceLocation {
            file: self.file(),
            start: min(self.start(), other.start()),
            end: max(self.end(), other.end()),
        }
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
