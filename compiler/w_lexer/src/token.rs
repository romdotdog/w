use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Fn,
    Return,
    If,
    Loop,
    Let,
    Struct,
    Union,
    Enum,

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

    UInteger(u64),
    Integer(i64),
    Float(f64),
    Overflown,

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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Compound(BinOpVariant::Id) => write!(f, "="),
            BinOp::Compound(v) => write!(f, "{}=", v),
            BinOp::Regular(v) => write!(f, "{}", v),
        }
    }
}

impl Display for BinOpVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOpVariant::Id => panic!(),
            BinOpVariant::Lt => write!(f, "<"),
            BinOpVariant::Le => write!(f, "<="),
            BinOpVariant::Gt => write!(f, ">"),
            BinOpVariant::Ge => write!(f, ">="),
            BinOpVariant::EqC => write!(f, "=="),
            BinOpVariant::Neq => write!(f, "!="),
            BinOpVariant::Add => write!(f, "+"),
            BinOpVariant::Sub => write!(f, "-"),
            BinOpVariant::Mul => write!(f, "*"),
            BinOpVariant::Div => write!(f, "/"),
            BinOpVariant::Mod => write!(f, "%"),
            BinOpVariant::Xor => write!(f, "^"),
            BinOpVariant::And => write!(f, "&"),
            BinOpVariant::Or => write!(f, "|"),
            BinOpVariant::Rsh => write!(f, ">>"),
            BinOpVariant::Lsh => write!(f, "<<"),
        }
    }
}
