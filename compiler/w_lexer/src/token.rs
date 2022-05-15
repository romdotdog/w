use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'ast> {
    Fn,
    Export,
    Return,
    Sizeof,
    If,
    Loop,
    Let,
    Static,
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

    Integer(i64),
    Uinteger(u64),
    Float(f64),
    Overflown,

    Char(char),
    String(&'ast str),
    Ident(&'ast str),
    Label(&'ast str),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BinOp(pub bool, pub BinOpVariant);

impl BinOp {
    pub fn prec(self) -> u8 {
        if self.0 {
            return 20;
        }
        self.1.prec()
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

    And,
    Or,
    Xor,
    Shl,
    Shr,
}

impl BinOpVariant {
    pub fn prec(self) -> u8 {
        match self {
            BinOpVariant::Mul | BinOpVariant::Div | BinOpVariant::Mod => 10,
            BinOpVariant::Add | BinOpVariant::Sub => 11,
            BinOpVariant::Shl | BinOpVariant::Shr => 12,
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

    /// shorthand for x ^ -1
    BNot,

    /// shorthand for x == 0
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
            BinOp(true, BinOpVariant::Id) => write!(f, "="),
            BinOp(true, v) => write!(f, "{}=", v),
            BinOp(false, v) => write!(f, "{}", v),
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
            BinOpVariant::Shr => write!(f, ">>"),
            BinOpVariant::Shl => write!(f, "<<"),
        }
    }
}
