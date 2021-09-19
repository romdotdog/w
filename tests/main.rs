use w::Session;

#[test]
fn basic_fn() {
    let file = std::fs::read_to_string("benches/fn.w").unwrap();

    let sess = Session::new();
    let entry = sess.register_source("fn.w".to_owned(), file);
    let t = sess.parse(entry).parse();
    //println!("{:#?}", t);
    sess.diagnostics();
}
