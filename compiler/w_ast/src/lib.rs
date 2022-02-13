use std::collections::HashMap;
use w_lexer::token::{BinOp, UnOp};

mod types;
pub use types::{ReferenceKind, Type, TypeVariant};

mod indir;
pub use indir::Indir;

mod span;
pub use span::Span;

mod codegen;

pub struct Program<'ast>(pub Vec<Spanned<TopLevel<'ast>>>);
pub enum TopLevel<'ast> {
    Fn {
        name: Spanned<&'ast str>,
        params: Vec<Spanned<IdentPair<'ast>>>,
        atom: Spanned<Atom<'ast>>,
        t: Option<Spanned<Type<'ast>>>,
        exported: bool,
    },

    Enum {
        name: Spanned<&'ast str>,
        fields: Spanned<HashMap<&'ast str, i64>>,
    },

    Struct(Spanned<&'ast str>, Spanned<TypeBody<'ast>>),
    Union(Spanned<&'ast str>, Spanned<TypeBody<'ast>>),
    Static(Decl<'ast>),
}

#[derive(Clone)]
pub struct TypeBody<'ast>(pub Vec<Spanned<IdentPair<'ast>>>);
pub struct Decl<'ast> {
    pub pair: Spanned<IdentPair<'ast>>,
    pub rhs: Option<BAtom<'ast>>,
}

pub enum IncDec {
    Inc,
    Dec,
}

type SAtom<'ast> = Spanned<Atom<'ast>>;
type BAtom<'ast> = Box<SAtom<'ast>>;
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

    Static(Decl<'ast>),
    Let(Decl<'ast>),

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
