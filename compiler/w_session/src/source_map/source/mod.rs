use std::{cell::Cell, hash::Hash, path::PathBuf};
use w_parser::handler::Status;
use w_utils::{LineCol, LineColResult};

#[derive(Clone)]
pub struct Source {
    pub path: PathBuf,
    lines: LineCol,
    status: Cell<Status>,
}

impl Source {
    pub fn new(path: PathBuf, src: String) -> Self {
        let mut line_pos: Vec<usize> = vec![0];
        for (i, c) in src.char_indices() {
            if c == '\n' {
                line_pos.push(i + 1);
            }
        }

        Source {
            path,
            lines: LineCol::new(src),
            status: Cell::new(Status::NotParsing),
        }
    }

    pub fn src(&self) -> &str {
        &self.lines.src
    }

    pub fn set_status(&self, status: Status) {
        self.status.set(status)
    }

    pub fn get_status(&self) -> Status {
        self.status.get()
    }

    /// # Panics
    /// if i >= size of source
    pub fn line_col(&self, i: usize) -> LineColResult {
        self.lines.line_col(i)
    }
}

impl Hash for Source {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.src().hash(state);
    }
}
