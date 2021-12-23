#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Fn,
    Return,
    If,
    Loop,
    Let,
    Struct,
    Union,

    Else,
    Mut,
    Br,

    Semicolon,
    Colon,
    Comma,
    Period,
    Arrow,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftSqBracket,
    RightSqBracket,

    BinOp(BinOp),
    UnOp(UnOp),
    AmbiguousOp(AmbiguousOp),

    Integer(i64),
    Float(f64),

    String(String),
    Char(char),
    Ident(String),
    Label(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    Compound(BinOpVariant),
    Regular(BinOpVariant),
}

impl BinOp {
    pub fn prec(self) -> u8 {
        match self {
            BinOp::Compound(_) => return 20,
            BinOp::Regular(t) => t,
        }
        .prec()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOpVariant {
    /// Identity: used for the vanilla equals assignment
    ///
    /// `a = 1`
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
    pub fn prec(self) -> u8 {
        match self {
            BinOpVariant::Mul | BinOpVariant::Div | BinOpVariant::Mod => 10,
            BinOpVariant::Add | BinOpVariant::Sub => 11,
            BinOpVariant::Lsh | BinOpVariant::Rsh => 12,
            BinOpVariant::Gt | BinOpVariant::Ge | BinOpVariant::Lt | BinOpVariant::Le => 13,
            BinOpVariant::EqC | BinOpVariant::Neq => 14,
            BinOpVariant::And => 15,
            BinOpVariant::Xor => 16,
            BinOpVariant::Or => 17,
            BinOpVariant::Id => unreachable!(), // always compound
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
		#[derive(Debug, PartialEq, Clone, Copy)]
		pub enum AmbiguousOp {
			$($n), *
		}

		impl AmbiguousOp {
			pub fn binary(&self) -> BinOpVariant {
				match self {
					$(
						AmbiguousOp::$n => BinOpVariant::$b
					), *
				}
			}

			pub fn unary(&self) -> UnOp {
				match self {
					$(
						AmbiguousOp::$n => UnOp::$u
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
