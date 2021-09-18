use w::lexer::Lexer;

#[test]
fn basic_fn() {
	let file = std::fs::read_to_string("tests/fn.w").unwrap();
	let lex = Lexer::new(&file);
	for t in lex {
		println!("{:?}", t);
	}
}