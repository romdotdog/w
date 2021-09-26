use similar::{ChangeTag, TextDiff};
use std::fs;
use w::Session;

macro_rules! test {
    ($f: ident) => {
        #[test]
        fn $f() {
            let sess = Session::new();

            let filename = concat!("tests/parser/", stringify!($f), ".w");
            let fixture = concat!("tests/parser/", stringify!($f), ".fixture.w");
            let entry =
                sess.register_source(filename.to_owned(), fs::read_to_string(filename).unwrap());
            let t = format!("{}", sess.parse(entry).parse().unwrap());

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
test!(ifs);

test!(returns);
