use crate::lexer::Op;

#[derive(Debug)]
pub enum Atom {
    Integer(i64),
    Float(f64),
    Ident(String),

    Paren(Box<Atom>),
    BinOp(Box<Atom>, Op, Box<Atom>),
    UnOp(Op, Box<Atom>),
}
