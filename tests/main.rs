use w::Session;

macro_rules! test {
	{$name: ident, $code: expr} => {
		#[test]
		fn $name() {
			let sess = Session::new();
			let entry = sess.register_source(stringify!($name).to_string(), $code.to_string());
			let t = sess.parse(entry).parse();
			sess.diagnostics();
		}
	}
}

#[test]
fn basic_fn() {
    let file = std::fs::read_to_string("benches/fn.w").unwrap();

    let sess = Session::new();
    let entry = sess.register_source("fn.w".to_owned(), file);
    let t = sess.parse(entry).parse();
    println!("{}", t);
    sess.diagnostics();
}

test! {
    missing_semicolon,
    r#"
fn main(): u64 {
	i = 1
	i = 2
}
"#
}
