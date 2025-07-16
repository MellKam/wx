use std::fmt;
use std::ops::Range;

use serde::Serialize;

#[derive(Clone, Copy, Default, PartialEq, Serialize)]
pub struct ByteIndex(pub u32);

impl ByteIndex {
    #[inline]
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Debug for ByteIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for ByteIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub struct TextSpan {
    start: ByteIndex,
    end: ByteIndex,
}

impl TextSpan {
    pub fn new(start: u32, end: u32) -> TextSpan {
        assert!(end >= start);
        TextSpan {
            start: ByteIndex(start),
            end: ByteIndex(end),
        }
    }

    /// Combine two spans by taking the start of the earlier span
    /// and the end of the later span.
    pub fn merge(self, other: TextSpan) -> TextSpan {
        use core::cmp::{max, min};

        let start = min(self.start.0, other.start.0);
        let end = max(self.end.0, other.end.0);
        TextSpan::new(start, end)
    }

    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start.to_usize()..self.end.to_usize()]
    }

    pub fn start_position(&self) -> TextSpan {
        TextSpan::new(self.start.0, self.start.0)
    }

    pub fn end_position(&self) -> TextSpan {
        TextSpan::new(self.end.0, self.end.0)
    }

    #[inline]
    pub const fn start(&self) -> ByteIndex {
        self.start
    }

    #[inline]
    pub fn end(&self) -> ByteIndex {
        self.end
    }
}

impl fmt::Display for TextSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{start}, {end})", start = self.start, end = self.end)
    }
}

impl Into<Range<usize>> for TextSpan {
    fn into(self) -> Range<usize> {
        self.start.to_usize()..self.end.to_usize()
    }
}
