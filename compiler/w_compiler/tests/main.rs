use std::{
    fs::{read_dir, read_to_string},
    path::Path,
};

use w_codegen::nop::NopSerializer;
use w_compiler::{
    handler::{ImportlessHandler, ImportlessHandlerHandler},
    Compiler,
};
use w_errors::Message;
use w_utils::span::Span;

#[test]
fn compiler() {
    let paths = read_dir("tests").unwrap();
    for path in paths {
        let path = path.unwrap().path();
        if let Some(ext) = path.extension() {
            if ext == "w" {
                test_file(&path);
            }
        }
    }
}

struct Session;
impl<'ast> ImportlessHandler<'ast> for Session {
    fn error(&self, msg: Message, _span: Span) {
        panic!("{}", msg);
    }
}

fn test_file(path: &Path) {
    println!("testing {}", path.display());
    let src = read_to_string(path).unwrap();
    let session = Session;
    let handler = ImportlessHandlerHandler { handler: &session };
    Compiler::compile_string(&handler, NopSerializer, &src);
}
