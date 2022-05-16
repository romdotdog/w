use std::path::PathBuf;

use w_session::{
    diag::emitter::{DefaultEmitter, DummyWriter},
    source_map::loader::FileLoader,
    Session,
};

#[test]
fn include_already_parsed() {
    let sess = Session::new(
        FileLoader,
        DefaultEmitter {
            stream: DummyWriter,
            color: false,
        },
    );
    let src = sess
        .source_map()
        .load_source(PathBuf::from("tests/import/main.w"))
        .unwrap();
    sess.compile(src);
}
