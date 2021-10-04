use super::indir::Indir;
use crate::{
    lexer::{BinOp, UnOp},
    span::Span,
};

pub struct Program {
    pub fns: Vec<WFn>,
}

pub struct WFn {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub atom: Atom,
    pub t: Type,
}

type BAtom = Box<Atom>;

pub enum IncDec {
    Inc,
    Dec,
}

pub enum AtomVariant {
    String(String),
    Char(char),
    Integer(i64),
    Float(f64),
    Ident(String),

    Paren(BAtom),
    BinOp(BAtom, BinOp, BAtom),
    UnOp(UnOp, BAtom),
    PostIncDec(BAtom, IncDec),

    Call(BAtom, Vec<Atom>),
    Access(BAtom, String),
    Index(BAtom, BAtom),

    Block(Vec<Atom>, Option<BAtom>),
    Let(bool, Vec<Declaration>),
    If(BAtom, BAtom, Option<BAtom>),
    Loop(BAtom, BAtom),
    Return(BAtom),
    Br(Option<BAtom>),
}

pub struct Declaration {
    pub lvalue: Atom,
    pub rvalue: Atom,
    pub t: Type,
}

pub struct Atom {
    pub v: AtomVariant,
    pub span: Span,
    pub t: Type,
}

#[derive(Clone, Copy)]
pub struct Type {
    pub v: TypeVariant,
    pub indir: Indir,
}

impl Type {
    pub fn new(v: TypeVariant) -> Self {
        Type {
            v,
            indir: Indir::none(),
        }
    }

    pub fn with_indir(v: TypeVariant, indir: Indir) -> Self {
        Type { v, indir }
    }

    pub fn void() -> Self {
        Self::new(TypeVariant::Void)
    }

    pub fn auto() -> Self {
        Self::new(TypeVariant::Auto)
    }
}

#[derive(Clone, Copy)]
pub enum TypeVariant {
    Auto,
    Void,
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
            "void" => TypeVariant::Void,
            _ => todo!(),
        }
    }
}
