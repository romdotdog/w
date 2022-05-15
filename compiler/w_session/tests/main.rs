use std::path::PathBuf;

use w_session::{diag::emitter::DefaultEmitter, source_map::loader::FileLoader, Session};

#[test]
fn include_already_parsed() {
    let sess = Session::new(FileLoader, DefaultEmitter);
    let src = sess
        .source_map()
        .load_source(PathBuf::from("tests/main.w"))
        .unwrap();
    sess.compile(src);
}
