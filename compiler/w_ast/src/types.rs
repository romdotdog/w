use crate::{IdentPair, Indir, Spanned};

#[derive(Clone)]
pub struct Type<'ast> {
    pub v: TypeVariant<'ast>,
    pub indir: Indir,
}

impl<'ast> Type<'ast> {
    pub fn new(v: TypeVariant<'ast>) -> Self {
        Type {
            v,
            indir: Indir::none(),
        }
    }

    pub fn with_indir(v: TypeVariant<'ast>, indir: Indir) -> Self {
        Type { v, indir }
    }

    pub fn void() -> Self {
        Self::new(TypeVariant::Void)
    }
}

#[derive(Clone)]
pub enum TypeVariant<'ast> {
    Void,
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    Struct(Spanned<Vec<Spanned<IdentPair<'ast>>>>),
    Union(Spanned<Vec<Spanned<IdentPair<'ast>>>>),
    Unresolved(&'ast str),
}

impl<'ast> From<&'ast str> for TypeVariant<'ast> {
    fn from(s: &'ast str) -> Self {
        match s {
            "i32" => TypeVariant::I32,
            "i64" => TypeVariant::I64,
            "u32" => TypeVariant::U32,
            "u64" => TypeVariant::U64,
            "f32" => TypeVariant::F32,
            "f64" => TypeVariant::F64,
            "void" => TypeVariant::Void,
            _ => TypeVariant::Unresolved(s),
        }
    }
}
