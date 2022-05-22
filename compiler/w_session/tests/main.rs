use similar::{ChangeTag, TextDiff};
use std::{fs, path::PathBuf};
use w_session::{diag::emitter::DefaultEmitter, source_map::loader::FileLoader, Session};

#[test]
fn error_tests() {
    let paths = fs::read_dir("tests/errors").unwrap();
    for path in paths {
        let path = path.unwrap().path();
        if let Some(ext) = path.extension() {
            if ext == "w" {
                println!();
                println!("testing {}", path.file_name().unwrap().to_str().unwrap());
                test_file(path);
            }
        }
    }
}

fn test_file(mut path: PathBuf) {
    let mut stderr = String::new();
    let sess = Session::new(
        FileLoader,
        DefaultEmitter {
            stream: &mut stderr,
            color: false,
        },
    );

    let src = sess.source_map().load_source(path.clone()).unwrap();
    sess.compile(src);

    path.set_extension("stderr");
    if let Ok(fixture) = fs::read_to_string(&path) {
        let mut success = true;
        let diff = TextDiff::from_lines(&fixture, &stderr);
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
        fs::write(path, stderr).unwrap();
    }
}

#[test]
fn include_already_parsed() {
    let mut s = String::new();
    let sess = Session::new(
        FileLoader,
        DefaultEmitter {
            stream: &mut s,
            color: false,
        },
    );
    let src = sess
        .source_map()
        .load_source(PathBuf::from("tests/import/main.w"))
        .unwrap();
    sess.compile(src);
    assert!(s.is_empty());
}

#[test]
fn recursive_error() {
    let mut s = String::new();
    let sess = Session::new(
        FileLoader,
        DefaultEmitter {
            stream: &mut s,
            color: false,
        },
    );
    let src = sess
        .source_map()
        .load_source(PathBuf::from("tests/recursion/main.w"))
        .unwrap();
    sess.compile(src);
    assert_eq!(
        s,
        "tests/recursion/main.w:1:11: error: file not found
#include <bar.w>
          ^^^^^^
"
    );
}
