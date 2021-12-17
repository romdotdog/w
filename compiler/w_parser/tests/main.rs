//use similar::{ChangeTag, TextDiff};
use std::{cell::RefCell, cmp::max, fs, str::Chars};

use w_errors::Message;
use w_lexer::Span;
use w_parser::{Handler, Parser};
use w_utils::{LineCol, LineColResult};

struct ErrorHandler<'a> {
    src: &'a LineCol,
    errors: RefCell<Vec<(Message, Span)>>,
    expected_errors: &'static [Message],
}

impl ErrorHandler<'_> {
    fn check_errors(&self) {
        let errors = self.errors.borrow();
        let expected_len = self.expected_errors.len();
        let got_len = errors.len();
        for i in 0..max(expected_len, got_len) {
            match errors.get(i) {
                Some((got, span)) => {
                    let LineColResult { line, col, .. } = self.src.line_col(span.start);

                    match self.expected_errors.get(i) {
                        Some(expected) => {
                            if got != expected {
                                panic!("{}:{}: expected '{}', got '{}'", line, col, expected, got,);
                            }
                        }
                        None => panic!("{}:{}: got extra error '{}'", line, col, got,),
                    }
                }
                None => panic!("missing error '{}'", self.expected_errors[i]),
            }
        }
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
    ($f: ident $(, $e: ident)*) => {
        #[test]
        fn $f() {
			let filename = concat!("tests/", stringify!($f), ".w");
            //let fixture = concat!("tests/", stringify!($f), ".fixture.w");
			let src = LineCol::new(fs::read_to_string(filename).unwrap());
			let handler = ErrorHandler {
				src: &src,
				errors: RefCell::new(Vec::new()),
				expected_errors: &[$(Message::$e), *]
			};

			let parser = Parser::new(&handler, ());
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
test!(operations, LabelIsNotIdentifier);
test!(types, TooMuchIndirection);
test!(controlflow, LoopBodyBlock, MissingSemicolon);
test!(structs);
test!(
    postfix,
    MissingIdentifier,
    MissingClosingSqBracket,
    UnexpectedToken,
    MissingClosingParen
);
