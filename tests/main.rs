use w::{lexer::Lexer, parser::Parser};

#[test]
fn basic_fn() {
    let file = std::fs::read_to_string("tests/fn.w").unwrap();
    let mut t = Parser::new(&file);
    println!("{:#?}", t.expr());
}
