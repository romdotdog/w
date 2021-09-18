use w::Session;

#[test]
fn basic_fn() {
    let file = std::fs::read_to_string("tests/fn.w").unwrap();
    let mut t = Session::new().parse(&file).parse();
    println!("{:#?}", t);
}
