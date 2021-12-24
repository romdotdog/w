use w_lexer::{BinOp, UnOp};

mod types;
pub use types::{Type, TypeVariant};

mod indir;
pub use indir::Indir;

mod span;
pub use span::Span;

pub struct Program {
    pub fns: Vec<Spanned<WFn>>,
    pub structs: Vec<Spanned<WStruct>>,
    pub unions: Vec<Spanned<WUnion>>,
}

type BAtom = Box<Spanned<Atom>>;

pub struct WFn {
    pub name: Spanned<String>,
    pub params: Vec<Spanned<IdentPair>>,
    pub atom: Spanned<Atom>,
    pub t: Option<Spanned<Type>>,
}

pub struct WStruct {
    pub name: Spanned<String>,
    pub fields: Spanned<Vec<Spanned<IdentPair>>>,
}

pub struct WUnion {
    pub name: Spanned<String>,
    pub fields: Spanned<Vec<Spanned<IdentPair>>>,
}

pub enum IncDec {
    Inc,
    Dec,
}

pub enum Atom {
    String(String),
    Ident(String),
    Char(char),
    Integer(i64),
    Float(f64),

    Paren(BAtom),
    BinOp(BAtom, BinOp, BAtom),
    UnOp(UnOp, BAtom),
    Cast(Spanned<Type>, BAtom, bool),
    PostIncDec(BAtom, IncDec),

    Call(BAtom, Vec<Spanned<Atom>>),
    Access(BAtom, Spanned<String>),
    Index(BAtom, BAtom),

    Block(Option<Spanned<String>>, Vec<Spanned<Atom>>, Option<BAtom>),
    Loop(Option<Spanned<String>>, Option<BAtom>, BAtom),

    Let(Spanned<IdentPair>, Option<BAtom>),
    If(BAtom, BAtom, Option<BAtom>),

    Return(Option<BAtom>),
    Br(Option<BAtom>, Spanned<String>, Option<BAtom>),
}

#[derive(Clone)]
pub struct IdentPair {
    pub mutable: Option<Span>,
    pub ident: Spanned<String>,
    pub t: Option<Spanned<Type>>,
}

#[derive(Clone, Copy)]
pub struct Spanned<T>(pub T, pub Span);
