use w::Session;

#[test]
fn basic_fn() {
    let file = std::fs::read_to_string("tests/fn.w").unwrap();
    let sess = Session::new();
    let t = sess.lexer(&file);
    for i in t {
        println!("{:?}", i)
    }
}
