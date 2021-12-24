use similar::{ChangeTag, TextDiff};
use std::fmt::Write;
use std::{cell::RefCell, fs, str::Chars};

use w_ast::Span;
use w_errors::Message;
use w_parser::{Handler, Parser};
use w_utils::{LineCol, LineColResult};

struct ErrorHandler<'a> {
    src: &'a LineCol,
    errors: RefCell<Vec<(Message, Span)>>,
}

impl ErrorHandler<'_> {
    fn serialize_errors(&self) -> String {
        let mut res = String::new();
        let errors = self.errors.borrow();
        for (msg, span) in errors.iter() {
            let LineColResult { line, col, .. } = self.src.line_col(span.start);

            writeln!(
                res,
                "// \"{}\" - {}:{}+{}",
                msg,
                line,
                col,
                span.end - span.start
            )
            .unwrap();
        }
        res
    }
}

impl<'a> Handler for ErrorHandler<'a> {
    type SourceRef = ();
    type LexerInput = Chars<'a>;

    fn error(&self, _src_ref: &Self::SourceRef, msg: Message, span: Span) {
        self.errors.borrow_mut().push((msg, span));
    }

    fn load_source(&self, _name: String) -> Option<Self::SourceRef> {
        panic!("imports are not allowed in parser tests.");
    }

    fn get_source(&self, _src_ref: &Self::SourceRef) -> Self::LexerInput {
        self.src.src.chars()
    }
}

macro_rules! test {
    ($f: ident) => {
        #[test]
        fn $f() {
            let filename = concat!("tests/", stringify!($f), ".w");
            let fixture = concat!("tests/", stringify!($f), ".fixture.w");
            let src = LineCol::new(fs::read_to_string(filename).unwrap());
            let handler = ErrorHandler {
                src: &src,
                errors: RefCell::new(Vec::new()),
            };

            let parser = Parser::new(&handler, ());
            parser.parse();

            let t = handler.serialize_errors();
            if let Ok(fixture_src) = fs::read_to_string(fixture) {
                let mut failed = false;
                let diff = TextDiff::from_lines(&fixture_src, &t);
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        ChangeTag::Delete => {
                            failed = true;
                            "-"
                        }
                        ChangeTag::Insert => {
                            failed = true;
                            "+"
                        }
                        ChangeTag::Equal => " ",
                    };
                    print!("{}{}", sign, change);
                }

                if failed {
                    panic!("FAIL");
                }
            } else {
                fs::write(fixture, t).unwrap();
            }
        }
    };
}

test!(literals);
test!(operations);
test!(types);
test!(controlflow);
test!(structs);
test!(postfix);
test!(functions);
