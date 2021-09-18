use crate::lexer::Op;

#[derive(Debug)]
pub enum TopLevel {
    Fn(String, Atom),
}

#[derive(Debug)]
pub enum Atom {
    Integer(i64),
    Float(f64),
    Ident(String),
    Block(Vec<Atom>, Box<Atom>),
    Null,

    Paren(Box<Atom>),
    BinOp(Box<Atom>, Op, Box<Atom>),
    UnOp(Op, Box<Atom>),
}
