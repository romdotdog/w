use crate::SourceRef;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub src: SourceRef,
}

impl Span {
    pub fn new(src: SourceRef, start: usize, end: usize) -> Self {
        Span { src, start, end }
    }

    pub fn to(mut self, end: Self) -> Self {
        self.end = end.end;
        self
    }
}
