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

    Op { t: Op, is_assignment: bool },

    Integer(i64),
    Float(f64),

    String(String),
    Char(char),
    Ident(String),
}

#[derive(Debug, PartialEq)]
pub enum Op {
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

    /// can be used as unary as deref
    Mul,

    Div,
    Mod,

    Xor,

    /// can be used as unary as adrof
    And,
    Or,
    Rsh,
    Lsh,

    // -- UnOp --
    Inc,
    Dec,

    Reinterpret,
    Cast,

    /// shorthand for -1 - x
    BNot,

    /// shorthand for x ^ 1
    LNot,
}

impl Op {
    pub fn is_unary(&self) -> bool {
        match self {
            Op::LNot
            | Op::BNot
            | Op::And
            | Op::Mul
            | Op::Sub
            | Op::Add
            | Op::Reinterpret
            | Op::Cast => true,
            _ => false,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            Op::LNot | Op::BNot | Op::Reinterpret | Op::Cast => false,
            _ => true,
        }
    }

    pub fn prec(&self, is_assignment: bool) -> u8 {
        if is_assignment {
            return 20;
        }

        match self {
            Op::Acs => 9,
            Op::Mul | Op::Div | Op::Mod => 10,
            Op::Add | Op::Sub => 11,
            Op::Lsh | Op::Rsh => 12,
            Op::Gt | Op::Ge | Op::Lt | Op::Le => 13,
            Op::EqC | Op::Neq => 14,
            Op::And => 15,
            Op::Xor => 16,
            Op::Or => 17,

            // is_assignment is always on
            Op::Id => unreachable!(),

            // complete if this will ever be hit
            Op::Inc => todo!(),
            Op::Dec => todo!(),
            Op::BNot => todo!(),
            Op::LNot => todo!(),
            Op::Reinterpret => todo!(),
            Op::Cast => todo!(),
        }
    }
}
