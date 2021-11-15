#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// start is inclusive, end is exclusive
impl Span {
    /// # Panics
    /// `start >= end`
    #[must_use]
    pub fn new(start: usize, end: usize) -> Self {
        assert!(start < end, "{} .. {}", start, end);
        Span { start, end }
    }

    /// # Panics
    /// `self.start >= end`
    #[must_use]
    pub fn to(mut self, end: Self) -> Self {
        self.end = end.end;
        assert!(self.start < self.end, "{} .. {}", self.start, self.end);
        self
    }

    #[must_use]
    pub fn move_by(mut self, n: usize) -> Self {
        self.start += n;
        self.end += n;
        self
    }
}
