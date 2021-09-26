use super::indir::Indir;
use crate::{
    lexer::{BinOp, UnOp},
    span::Span,
};

/*
    TODO:
    * Remove Debug derives
*/

#[derive(Debug)]
pub struct Program {
    pub fns: Vec<WFn>,
}

#[derive(Debug)]
pub struct WFn {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub atom: Atom,
    pub t: Type,
}

type BAtom = Box<Atom>;

#[derive(Debug)]
pub enum AtomVariant {
    String(String),
    Char(char),
    Integer(i64),
    Float(f64),
    Ident(String),

    Paren(BAtom),
    BinOp(BAtom, BinOp, BAtom),
    UnOp(UnOp, BAtom),

    Block(Vec<Atom>, Option<BAtom>),
    Let(bool, Vec<Declaration>),
    If(BAtom, BAtom, Option<BAtom>),
    Return(BAtom),
}

#[derive(Debug)]
pub struct Declaration {
    pub lvalue: Atom,
    pub rvalue: Atom,
    pub t: Type,
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

#[derive(Debug, Clone, Copy)]
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
