#[derive(Debug, PartialEq)]
pub enum Token {
    Fn,
    Return,
    If,
    Else,
	Let,
	Mut,

    Semicolon,
    Colon,
    Comma,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

    Op(Op),

    Integer(i64),
    Float(f64),

    String(String),
    Char(char),
    Ident(String),
}

#[derive(Debug, PartialEq)]
pub enum Op {
	Binary(BinOp),
	Unary(UnOp),
	Ambiguous(AmbiguousOp)
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
	Compound(BinOpVariant),
	Regular(BinOpVariant),
}

#[derive(Debug, PartialEq)]
pub enum BinOpVariant {
	/// Identity: used for the vanilla equals assignment
    ///
    /// `a = 1`
    Id,

    /// .
    Acs,

    Lt,
    Le,
    Gt,
    Ge,

    /// ==
    EqC,
    Neq,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Xor,
    And,
    Or,
    Rsh,
    Lsh,
}

impl BinOpVariant {
	pub fn prec(&self) -> u8 {
		match self {
			Acs => 9,
            Mul | Div | Mod => 10,
            Add | Sub => 11,
            Lsh | Rsh => 12,
            Gt | Ge | Lt | Le => 13,
            EqC | Neq => 14,
            And => 15,
            Xor => 16,
            Or => 17,
		}
	}
}

#[derive(Debug)]
pub enum UnOp {
	Deref,
	AddrOf,
	Minus,
	Plus,

	Inc,
	Dec,
	Reinterpret,
	Cast,
	
	/// shorthand for -1 - x
    BNot,

    /// shorthand for x ^ 1
    LNot,
}

macro_rules! ambiguous {
	{$($n: ident => ($b: ident, $u: ident)), *} => {
		#[derive(Debug)]
		pub enum AmbiguousOp {
			$($n), *
		}

		impl AmbiguousOp {
			pub fn to_binary(&self) -> BinOpVariant {
				match self {
					$(
						$n => BinOpVariant::$b
					), *
				}
			}

			pub fn to_unary(&self) -> UnOp {
				match self {
					$(
						$n => UnOp::$u
					), *
				}
			}
		}
	}
}

ambiguous! {
	Minus => (Sub, Minus),
	Plus => (Add, Plus),
	Ampersand => (And, AddrOf),
	Asterisk => (Mul, Deref)
}

impl Op {
    pub fn is_unary(&self) -> bool {
        match self {
            Op::Binary(_) => false,
            _ => true,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            Op::Unary(_) => false,
            _ => true,
        }
    }

    pub fn prec(&self, is_assignment: bool) -> u8 {
		match self {
			Op::Unary(_) => panic!("attempt to get precedence of a unary operator"),
			Op::Binary(BinOp::Compound(_)) => return 20;
			Op::Binary(BinOp::Regular(t)) => t,
			Op::Ambiguous(t) => t.to_binary()
		}.prec()
    }
}
