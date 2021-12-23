use crate::{IdentPair, Indir, Spanned};

#[derive(Clone)]
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
}

#[derive(Clone)]
pub enum TypeVariant {
    Void,
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    Struct(Spanned<Vec<Spanned<IdentPair>>>),
    Union(Spanned<Vec<Spanned<IdentPair>>>),
    Unresolved(String),
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
            _ => TypeVariant::Unresolved(s),
        }
    }
}