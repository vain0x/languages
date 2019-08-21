//! ソースファイル
//!
//! ソースファイルは ID で管理する。

use std::cmp::{max, min};
use std::path::PathBuf;

/// ソースファイル上の範囲
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct SourceLocation {
    file: usize,

    /// 開始位置 (バイト単位)
    start: usize,

    end: usize,
}

/// ソースファイル上の位置情報
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct SourceExtent {
    // ノードの中心的な意味を持つ範囲。例えば `2 + 3` の `+` の部分。
    main: SourceLocation,

    // ノードの全体の範囲。例えば `(2 + 3)` のカッコも含めた部分。
    total: SourceLocation,
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

impl SourceExtent {
    pub(crate) fn new(main: SourceLocation, total: SourceLocation) -> SourceExtent {
        SourceExtent {
            main,
            total,
        }
    }

    pub(crate) fn main(&self) -> &SourceLocation {
        &self.main
    }

    pub(crate) fn total(&self) -> &SourceLocation {
        &self.total
    }

    pub(crate) fn shrink(&self) -> SourceExtent {
        (*self.main()).into()
    }

    pub(crate) fn extend(&mut self, other: &SourceLocation) {
        self.total = self.total().union(other);
    }
}

impl From<SourceLocation> for SourceExtent {
    fn from(loc: SourceLocation) -> SourceExtent {
        SourceExtent::new(loc, loc)
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
