use crate::{Indir, Spanned, TypeBody};

#[derive(Clone)]
pub enum ReferenceKind {
    None,
    Immutable,
    Mutable,
}

#[derive(Clone)]
pub struct Type<'ast> {
    pub v: TypeVariant<'ast>,
    pub indir: Indir,
    pub refkind: ReferenceKind,
}

impl<'ast> Type<'ast> {
    pub fn new(v: TypeVariant<'ast>, refkind: ReferenceKind) -> Self {
        Type {
            v,
            indir: Indir::none(),
            refkind,
        }
    }

    pub fn with_indir(v: TypeVariant<'ast>, indir: Indir, refkind: ReferenceKind) -> Self {
        Type { v, indir, refkind }
    }

    pub fn void() -> Self {
        Self::new(TypeVariant::Void, ReferenceKind::None)
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
    Struct(Spanned<TypeBody<'ast>>),
    Union(Spanned<TypeBody<'ast>>),
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
