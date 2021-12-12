use super::Source;
use std::rc::Rc;

// utf-8 ops copied from core::str::validations

pub struct SourceReader {
    src: Rc<Source>,
    index: usize,
}

impl Iterator for SourceReader {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_code_point()
            .map(|ch| unsafe { std::char::from_u32_unchecked(ch) })
    }
}

/// Mask of the value bits of a continuation byte.
const CONT_MASK: u8 = 0b0011_1111;

impl SourceReader {
    pub fn new(src: Rc<Source>) -> Self {
        Self { src, index: 0 }
    }

    fn next_code_point(&mut self) -> Option<u32> {
        let bytes = self.src.src().as_bytes();

        macro_rules! next {
            () => {{
                self.index += 1;
                bytes.get(self.index)
            }};
        }

        // Decode UTF-8
        let x = *next!()?;
        if x < 128 {
            return Some(u32::from(x));
        }

        // Multibyte case follows
        // Decode from a byte combination out of: [[[x y] z] w]
        // NOTE: Performance is sensitive to the exact formulation here
        let init = utf8_first_byte(x, 2);
        let y = unwrap_or_0(next!());
        let mut ch = utf8_acc_cont_byte(init, y);
        if x >= 0xE0 {
            // [[x y z] w] case
            // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
            let z = unwrap_or_0(next!());
            let y_z = utf8_acc_cont_byte(u32::from(y & CONT_MASK), z);
            ch = init << 12 | y_z;
            if x >= 0xF0 {
                // [x y z w] case
                // use only the lower 3 bits of `init`
                let w = unwrap_or_0(next!());
                ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
            }
        }

        Some(ch)
    }
}

fn unwrap_or_0(opt: Option<&u8>) -> u8 {
    match opt {
        Some(&byte) => byte,
        None => 0,
    }
}

fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    u32::from(byte & (0x7F >> width))
}

fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | u32::from(byte & CONT_MASK)
}
