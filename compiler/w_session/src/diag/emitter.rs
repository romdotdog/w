use std::fmt::Write;

use w_utils::linecol::LineColResult;

use super::Diagnostic;

pub trait Emitter {
    fn emit_error(&mut self, error: Diagnostic);
}

#[allow(clippy::module_name_repetitions)]
pub struct DefaultEmitter<O: Write> {
    pub stream: O,
    pub color: bool,
}

impl<O: Write> Emitter for DefaultEmitter<O> {
    fn emit_error(&mut self, error: Diagnostic) {
        let src = error.source;
        let content = src.src();
        let start_pos = error.span.start;
        let end_pos = error.span.end;

        assert!(
            start_pos < end_pos,
            "{} - {} / {}",
            start_pos,
            end_pos,
            error.msg
        );

        let LineColResult {
            line,
            col,
            start_of_line,
            end_of_line,
        } = src.line_col(start_pos);
        let spaces = content[start_of_line..start_pos]
            .chars()
            .map(|c| if c.is_whitespace() { c } else { ' ' })
            .collect::<String>();

        let r = if self.color {
            writeln!(
                self.stream,
                "\x1b[1m{}:{}:{}: \x1b[91merror:\x1b[0m {}\n{}\n{}\x1b[91m\x1b[1m{}\x1b[0m",
                src.path.display(),
                line,
                col,
                error.msg,
                content[start_of_line..end_of_line].trim_end(),
                spaces,
                "^".repeat(end_pos - start_pos),
            )
        } else {
            writeln!(
                self.stream,
                "{}:{}:{}: error: {}\n{}\n{}{}",
                src.path.display(),
                line,
                col,
                error.msg,
                content[start_of_line..end_of_line].trim_end(),
                spaces,
                "^".repeat(end_pos - start_pos),
            )
        };

        if r.is_err() {
            println!("emitter could not write errors: {:?}", r);
        }
    }
}

pub struct StdoutWriter;
impl Write for StdoutWriter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        print!("{}", s);
        Ok(())
    }
}

pub struct DummyWriter;
impl Write for DummyWriter {
    fn write_str(&mut self, _s: &str) -> std::fmt::Result {
        Ok(())
    }
}
