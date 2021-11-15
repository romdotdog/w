//use similar::{ChangeTag, TextDiff};
use std::{cell::RefCell, fs};

use w_errors::Message;
use w_lexer::Lexer;
use w_parser::{Handler, Parser};

struct ErrorHandler {
    pub errors: RefCell<Vec<Message>>,
    pub expected_errors: &'static [Message],
}

impl ErrorHandler {
    fn check_errors(&self) {
        let errors = self.errors.borrow();
        for i in 0..self.expected_errors.len() {
            match errors.get(i) {
                Some(got) => {
                    let expected = &self.expected_errors[i];
                    if got != expected {
                        panic!("expected {}, got {}", expected, got);
                    }
                }
                None => panic!("missing error {}", self.expected_errors[i]),
            }
        }
    }
}

impl Handler for ErrorHandler {
    type SourceRef = u8;

    fn error(&self, src_ref: &Self::SourceRef, msg: Message, span: w_lexer::Span) {
        self.errors.borrow_mut().push(msg);
    }

    fn load_source(&self, name: String) -> Option<Self::SourceRef> {
        panic!("imports are not allowed in parser tests.");
    }

    fn get_source<'a>(&'a self, src_ref: &'a Self::SourceRef) -> &'a str {
        todo!()
    }
}

macro_rules! test {
    ($f: ident $(, $e: ident)*) => {
        #[test]
        fn $f() {
			let filename = concat!("tests/", stringify!($f), ".w");
            //let fixture = concat!("tests/", stringify!($f), ".fixture.w");

			let handler = ErrorHandler {
				errors: RefCell::new(Vec::new()),
				expected_errors: &[$(Message::$e), *]
			};

			let parser = Parser::new(&handler, 0, Lexer::new(fs::read_to_string(filename).unwrap()));
			parser.parse();

			handler.check_errors();
			/*
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
			*/
        }
    };
}

test!(literals);
test!(operations);
test!(types, TooMuchIndirection);
test!(controlflow);
test!(
    errors,
    MissingSemicolon,
    MissingClosingParen,
    MissingType,
    MissingClosingAngleBracket,
    InvalidTopLevel,
    MalformedIdentifier,
    MalformedType,
    MissingIdentifier,
    MissingClosingBracket
);
test!(
    postfix,
    MissingIdentifier,
    MissingClosingSqBracket,
    UnexpectedToken,
    MissingClosingParen
);
