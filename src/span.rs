use crate::SourceRef;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub src: SourceRef,
}

/// start is inclusive, end is exclusive
impl Span {
    /// # Panics
    /// `start >= end`
    pub fn new(src: SourceRef, start: usize, end: usize) -> Self {
        assert!(start < end, "{} .. {}", start, end);
        Span { start, end, src }
    }

    /// # Panics
    /// `self.start >= end`
    pub fn to(mut self, end: Self) -> Self {
        self.end = end.end;
        assert!(self.start < self.end, "{} .. {}", self.start, self.end);
        self
    }

    pub fn move_by(mut self, n: usize) -> Self {
        self.start += n;
        self.end += n;
        self
    }
}
