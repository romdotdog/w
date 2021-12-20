use std::collections::HashMap;
use w_lexer::{BinOp, Span, UnOp};

mod ast_type;
pub use ast_type::{Type, TypeVariant};

mod indir;
pub use indir::Indir;

pub struct Program {
    pub fns: Vec<WFn>,
    pub structs: HashMap<String, Vec<IdentPair>>,
    pub unions: HashMap<String, Vec<IdentPair>>,
}

pub struct WFn {
    pub name: String,
    pub params: Vec<IdentPair>,
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

    Block(Option<String>, Vec<Atom>, Option<BAtom>),
    Loop(Option<String>, Option<BAtom>, BAtom),

    Let(IdentPair, Option<BAtom>),
    If(BAtom, BAtom, Option<BAtom>),

    Return(BAtom),
    Br(Option<BAtom>, String, Option<BAtom>),
}

#[derive(Clone)]
pub struct IdentPair {
    pub mutable: bool,
    pub ident: String,
    pub t: Type,
}

pub struct Atom {
    pub v: AtomVariant,
    pub span: Span,
    pub t: Type,
}
