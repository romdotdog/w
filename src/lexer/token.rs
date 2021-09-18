use crate::span::Span;

#[derive(Debug)]
pub struct Token {
	t: TokenType,
	span: Span
}

impl Token {
	pub fn new(t: TokenType, span: Span) -> Self {
		Token {
			t, span
		}
	}
}

#[derive(Debug)]
pub enum TokenType {
	Fn,
	
	Semicolon,
	Colon,
	Period,
	LeftParen,
	RightParen,
	LeftBracket,
	RightBracket,
	
	I32,

	// TBD at parse-time
	Plus,
	Minus,

	Op {
		t: Op,
		is_assignment: bool
	},

	Integer(i64),
	Float(f64),
	
	String(String),
	Char(char),
	Identifier(String)
}

#[derive(Debug)]
pub enum Op {
	/// Identity: used for the vanilla equals assignment
	/// a = 1
	Id,

	Lt,
	Le,
	Gt,
	Ge,
	/// ==
	EqC,
	Neq,

	Add,
	Sub,

	/// can be used as unary as deref
	Mul,

	Div,
	Mod,

	Xor,

	/// can be used as unary as deref
	And,
	Or,
	Rsh,
	Lsh,

	// -- UnOp -- 

	Inc,
	Dec,

	/// shorthand for -1 - x
	BNot,

	/// shorthand for x ^ 1
	LNot,
}