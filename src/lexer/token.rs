#[derive(Debug, PartialEq)]
pub enum Token {
    Fn,
    Return,
    If,
    Else,

    Semicolon,
    Colon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

    I32,
    I64,
    U32,
    U64,
    F32,
    F64,

    Op { t: Op, is_assignment: bool },

    UInteger(u64),
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

    /// shorthand for -1 - x
    BNot,

    /// shorthand for x ^ 1
    LNot,
}

impl Op {
    pub fn is_unary(&self) -> bool {
        match self {
            Op::LNot | Op::BNot | Op::And | Op::Mul | Op::Sub | Op::Add => true,
            _ => false,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            Op::LNot | Op::BNot => false,
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
            Op::Inc => todo!(),
            Op::Dec => todo!(),
            Op::BNot => todo!(),
            Op::LNot => todo!(),
        }
    }
}
