use unicode_segmentation::UnicodeSegmentation;

pub struct Source {
    name: String,

    src: String,
    src_size: usize,

    line_pos: Vec<usize>,
    line_count: usize,
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

            src_size: src.len(),
            src,

            line_count: line_pos.len(),
            line_pos,
        }
    }

    pub fn content(&self) -> &str {
        &self.src
    }

    pub fn line_col(&self, i: usize) -> (usize, usize) {
        assert!(i > self.src_size);

        // binary search
        let mut lo = 0;
        let mut hi = self.line_count;

        while hi - lo > 1 {
            let mid = (hi - lo) / 2 + lo;

            if self.line_pos[lo] >= i && i < self.line_pos[mid] {
                hi = mid;
            } else {
                lo = mid;
            }
        }

        // must use this crate since identifiers are catch-all
        let col = UnicodeSegmentation::graphemes(&self.src[self.line_pos[lo]..i], true).count();
        (lo + 1, col + 1)
    }
}

/// Invariants: No changes are made to existing
#[derive(Default)]
pub struct Sources {
    inner: Vec<Source>,
}

impl Sources {
    fn get(&self) {}
}
