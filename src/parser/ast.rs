use crate::{lexer::Op, span::Span};

/*
    TODO:
    * Remove Debug derives
*/

#[derive(Debug)]
pub enum TopLevel {
    Fn(String, Vec<(String, Type)>, Atom, TypeVariant),
}

type BAtom = Box<Atom>;

#[derive(Debug)]
pub enum AtomVariant {
    Integer(i64),
	UInteger(u64),
    Float(f64),
    Ident(String),
    Null,

    Paren(BAtom),
    BinOp(BAtom, Op, BAtom),
    UnOp(Op, BAtom),

    Block(Vec<Atom>, BAtom),
    If(BAtom, BAtom, Option<BAtom>),
	Return(BAtom)
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
pub struct Type {
    v: TypeVariant,
    indir: u8,
}

impl Type {
    pub fn new(v: TypeVariant, indir: u8) -> Self {
        Type { v, indir }
    }

    pub fn auto() -> Self {
        Type {
            v: TypeVariant::Auto,
            indir: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TypeVariant {
    Auto,
    Null,
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
}

impl From<String> for TypeVariant {
    fn from(s: String) -> Self {
        match s.as_str() {
            "i32" => TypeVariant::I32,
            "i64" => TypeVariant::I64,
            "u32" => TypeVariant::U32,
            "u64" => TypeVariant::U64,
            "f32" => TypeVariant::F32,
            "f64" => TypeVariant::F64,
            _ => todo!(),
        }
    }
}
