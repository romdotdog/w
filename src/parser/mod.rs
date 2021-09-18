use crate::lexer::Lexer;

mod ast;
pub use ast::Atom;

pub struct Parser<'a> {
	lex: Lexer<'a>
}

impl Parser<'_> {
	pub fn new(src: &str) -> Self {
		Parser {
			lex: Lexer::new(src)
		}
	}

	pub fn from_lexer(lex: Lexer) -> Self {
		Parser {
			lex
		}
	}

	--

	fn primaryexpr(&mut self) {
		
	}

	fn subexpr(&mut self, lhs: Atom, min_prec: u8) {
		let l = self.lex.next();
		
	}

	fn expr() {
		
	}
}



