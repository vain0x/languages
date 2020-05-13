use super::*;
use std::fmt;

/// テキスト上の範囲
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub(crate) struct Range {
    /// 開始位置
    pub(crate) start: Position,

    /// 終了位置 (終端は範囲外)
    pub(crate) end: Position,
}

impl Range {
    pub(crate) fn new(start: Position, end: Position) -> Range {
        Range { start, end }
    }

    #[allow(dead_code)]
    pub(crate) fn start(&self) -> Position {
        self.start
    }

    #[allow(dead_code)]
    pub(crate) fn end(&self) -> Position {
        self.end
    }

    #[allow(dead_code)]
    pub(crate) fn contains_loosely(self, position: Position) -> bool {
        self.start <= position && position <= self.end
    }

    pub(crate) fn unite(self, other: Range) -> Range {
        Range {
            start: self.start.min(other.start),
            end: self.start.max(other.end),
        }
    }

    pub(crate) fn behind(self) -> Range {
        Range {
            start: self.end,
            end: self.end + Position::new(0, 1),
        }
    }
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // <https://www.gnu.org/prep/standards/html_node/Errors.html>
        write!(
            f,
            "{}.{}-{}.{}",
            self.start.line + 1,
            self.start.character + 1,
            self.end.line + 1,
            self.end.character + 1
        )
    }
}
