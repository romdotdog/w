use crate::{lexer::Op, span::Span};

#[derive(Debug)]
pub enum TopLevel {
    Fn(String, Atom, Type),
}

type BAtom = Box<Atom>;

#[derive(Debug)]
pub enum AtomVariant {
    Integer(i64),
    Float(f64),
    Ident(String),
    Null,

    Paren(BAtom),
    BinOp(BAtom, Op, BAtom),
    UnOp(Op, BAtom),

    Reinterpret(BAtom),
    Cast(BAtom),

    Block(Vec<Atom>, BAtom),
    If(BAtom, BAtom, Option<BAtom>),
}

#[derive(Debug)]
pub struct Atom {
    pub v: AtomVariant,
    pub span: Span,
    pub t: Type,
}

impl Atom {
    pub fn new(v: AtomVariant, span: Span, t: Type) -> Self {
        Atom { v, span, t }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Auto,
    Null,
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
}

impl From<String> for Type {
    fn from(s: String) -> Self {
        match s.as_str() {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => todo!(),
        }
    }
}
