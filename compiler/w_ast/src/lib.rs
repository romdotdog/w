use w_lexer::{BinOp, Span, UnOp};

mod ast_type;
pub use ast_type::{Type, TypeVariant};

mod indir;
pub use indir::Indir;

pub struct Program {
    pub fns: Vec<WFn>,
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

    Block(Vec<Atom>, Option<BAtom>),
    Let(IdentPair, Option<BAtom>),
    If(BAtom, BAtom, Option<BAtom>),
    Loop(BAtom, BAtom),
    Return(BAtom),
    Br(Option<BAtom>),
}

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
