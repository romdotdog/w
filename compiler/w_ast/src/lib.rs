use std::collections::HashMap;
use w_lexer::token::{BinOp, UnOp};

mod types;
pub use types::{Type, TypeVariant};

mod indir;
pub use indir::Indir;

mod span;
pub use span::Span;

mod codegen;

pub struct Program<'ast> {
    pub fns: Vec<Spanned<WFn<'ast>>>,
    pub structs: Vec<Spanned<WStruct<'ast>>>,
    pub unions: Vec<Spanned<WUnion<'ast>>>,
    pub enums: Vec<Spanned<WEnum<'ast>>>,
}

type SAtom<'ast> = Spanned<Atom<'ast>>;
type BAtom<'ast> = Box<SAtom<'ast>>;

pub struct WFn<'ast> {
    pub name: Spanned<&'ast str>,
    pub params: Vec<Spanned<IdentPair<'ast>>>,
    pub atom: Spanned<Atom<'ast>>,
    pub t: Option<Spanned<Type<'ast>>>,
    pub exported: bool,
}

pub struct WStruct<'ast> {
    pub name: Spanned<&'ast str>,
    pub fields: Spanned<Vec<Spanned<IdentPair<'ast>>>>,
}

pub struct WUnion<'ast> {
    pub name: Spanned<&'ast str>,
    pub fields: Spanned<Vec<Spanned<IdentPair<'ast>>>>,
}

pub struct WEnum<'ast> {
    pub name: Spanned<&'ast str>,
    pub fields: Spanned<HashMap<&'ast str, i64>>,
}

pub enum IncDec {
    Inc,
    Dec,
}

pub enum Atom<'ast> {
    String(&'ast str),
    Ident(&'ast str),
    Char(char),
    Integer(i64),
    UInteger(u64),
    Float(f64),

    Paren(BAtom<'ast>),
    BinOp(BAtom<'ast>, BinOp, BAtom<'ast>),
    UnOp(UnOp, BAtom<'ast>),
    Reinterpret(Spanned<Type<'ast>>, BAtom<'ast>),
    Cast(Spanned<Type<'ast>>, BAtom<'ast>),
    PostIncDec(BAtom<'ast>, IncDec),
    Return(Option<BAtom<'ast>>),

    Call(BAtom<'ast>, Vec<SAtom<'ast>>),
    Access(BAtom<'ast>, Spanned<&'ast str>),
    Index(BAtom<'ast>, BAtom<'ast>),

    Block {
        label: Option<Spanned<&'ast str>>,
        blocks: Vec<SAtom<'ast>>,
        ret: Option<BAtom<'ast>>,
    },

    Loop {
        label: Option<Spanned<&'ast str>>,
        binding: Option<BAtom<'ast>>,
        block: BAtom<'ast>,
    },

    Let(Spanned<IdentPair<'ast>>, Option<BAtom<'ast>>),

    If {
        cond: BAtom<'ast>,
        true_branch: BAtom<'ast>,
        false_branch: Option<BAtom<'ast>>,
    },

    Br {
        ret: Option<BAtom<'ast>>,
        label: Option<Spanned<&'ast str>>,
        cond: Option<BAtom<'ast>>,
    },
}

#[derive(Clone)]
pub struct IdentPair<'ast> {
    pub mutable: Option<Span>,
    pub ident: Spanned<&'ast str>,
    pub t: Option<Spanned<Type<'ast>>>,
}

#[derive(Clone, Copy)]
pub struct Spanned<T>(pub T, pub Span);
