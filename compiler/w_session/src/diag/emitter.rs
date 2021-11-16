use super::Diagnostic;

pub trait Emitter {
    fn emit_errors(self, errors: Vec<Diagnostic>);
}

#[allow(clippy::module_name_repetitions)]
pub struct DefaultEmitter;

impl Emitter for DefaultEmitter {
    fn emit_errors(self, errors: Vec<Diagnostic>) {
        for error in errors {
            let src = &error.source;
            let start_pos = error.span.start;
            let end_pos = error.span.end;

            assert!(
                start_pos < end_pos,
                "{} - {} / {}",
                start_pos,
                end_pos,
                error.msg
            );

            let (line, column, (start_of_line, end_of_line)) = src.line_col(start_pos);
            let spaces = src.src[start_of_line..start_pos]
                .chars()
                .map(|c| if c.is_whitespace() { c } else { ' ' })
                .collect::<String>();

            println!(
                "\x1b[1m{}:{}:{}: \x1b[91merror:\x1b[0m {}\n{}\n{}\x1b[91m\x1b[1m{}\x1b[0m",
                src.name,
                line,
                column,
                error.msg,
                src.src[start_of_line..end_of_line].trim_end(),
                spaces,
                "^".repeat(end_pos - start_pos),
            );
        }
    }
}
