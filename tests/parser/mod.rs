use similar::{ChangeTag, TextDiff};
use std::fs;

use w::diag::{Diagnostic, Message};
use w::source_map::FileLoader;
use w::Session;

macro_rules! test {
    ($f: ident $(, $e: ident)*) => {
        #[test]
        fn $f() {
            let mut sess = Session::new(Box::new(FileLoader));

            let filename = concat!("tests/parser/", stringify!($f), ".w");
            let fixture = concat!("tests/parser/", stringify!($f), ".fixture.w");
            let entry =
                sess.source_map().register_source(filename.to_owned(), fs::read_to_string(filename).unwrap());
            let t = format!("{}", sess.parse(entry).parse());

			// check errors
			#[allow(unused_mut)]
			let mut n = 0;

			$({
				match sess.diagnostics().errors().get(n) {
					Some(Diagnostic { message: Message::$e, .. }) => {}
					Some(t) => panic!("expected '{}', got '{}'", Message::$e, t.message),
					None => panic!("expected '{}', got no error", Message::$e)
				};
				n += 1;
			})*

			match sess.diagnostics().errors().get(n) {
				Some(_) => panic!("found additional errors"),
				None => {},
			}

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
