use similar::{ChangeTag, TextDiff};
use std::fmt::Write;
use std::{cell::RefCell, fs};

use w_ast::Span;
use w_errors::Message;
use w_parser::{
    handler::{ImportlessHandler, ImportlessHandlerHandler},
    Parser,
};
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

impl<'ast> ImportlessHandler<'ast> for ErrorHandler<'ast> {
    fn error(&self, msg: Message, span: Span) {
        self.errors.borrow_mut().push((msg, span));
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

            let session = ImportlessHandlerHandler { handler: &handler };
            let ast = Parser::partial_parse(&session, &handler.src.src).parse();

            let t = format!("{}{}", ast, handler.serialize_errors());
            if let Ok(fixture_src) = fs::read_to_string(fixture) {
                let mut success = true;
                let diff = TextDiff::from_lines(&fixture_src, &t);
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        ChangeTag::Delete => {
                            success = false;
                            "-"
                        }
                        ChangeTag::Insert => {
                            success = false;
                            "+"
                        }
                        ChangeTag::Equal => " ",
                    };
                    print!("{}{}", sign, change);
                }

                assert!(success, "FAIL");
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
test!(statics);
