use w::Session;

#[test]
fn basic_fn() {
    let file = std::fs::read_to_string("benches/fn.w").unwrap();
    let t = Session::new().parse(&file).parse();
    println!("{:#?}", t);
}
