use std::fmt;
use std::ops::Range;

#[derive(Clone, Copy, Default, PartialEq)]
pub struct ByteIndex(pub u32);

impl ByteIndex {
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Debug for ByteIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ByteIndex(")?;
        self.0.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for ByteIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    start: ByteIndex,
    end: ByteIndex,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        assert!(end >= start);
        Span {
            start: ByteIndex(start),
            end: ByteIndex(end),
        }
    }

    /// Combine two spans by taking the start of the earlier span
    /// and the end of the later span.
    pub fn merge(self, other: Span) -> Span {
        use core::cmp::{max, min};

        let start = min(self.start.0, other.start.0);
        let end = max(self.end.0, other.end.0);
        Span::new(start, end)
    }

    /// A helper function to tell whether two spans do not overlap.
    pub fn disjoint(self, other: Span) -> bool {
        let (first, last) = if self.end.0 < other.end.0 {
            (self, other)
        } else {
            (other, self)
        };
        first.end.0 <= last.start.0
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

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{start}, {end})", start = self.start, end = self.end)
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.start.to_usize()..self.end.to_usize()
    }
}
