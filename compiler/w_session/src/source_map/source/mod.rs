mod reader;
pub use reader::SourceReader;
use w_utils::{LineCol, LineColResult};

#[derive(Clone)]
pub struct Source {
    pub name: String,
    lines: LineCol,
}

impl Source {
    pub fn new(name: String, src: String) -> Self {
        let mut line_pos: Vec<usize> = vec![0];
        for (i, c) in src.char_indices() {
            if c == '\n' {
                line_pos.push(i + 1);
            }
        }

        Source {
            name,
            lines: LineCol::new(src),
        }
    }

    pub fn src(&self) -> &str {
        &self.lines.src
    }

    /// # Panics
    /// if i >= size of source
    pub fn line_col(&self, i: usize) -> LineColResult {
        self.lines.line_col(i)
    }
}
