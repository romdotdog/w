use crate::lexer::Op;

#[derive(Debug)]
pub enum TopLevel {
    Fn(String, Atom),
}

type BAtom = Box<Atom>;

#[derive(Debug)]
pub enum Atom {
    Integer(i64),
    Float(f64),
    Ident(String),
    Null,

    Paren(BAtom),
    BinOp(BAtom, Op, BAtom),
    UnOp(Op, BAtom),

    Block(Vec<Atom>, BAtom),
    If(BAtom, BAtom, Option<BAtom>),
}
