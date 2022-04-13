use indir::Indir;

pub mod indir;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    None,
    Immutable,
    Mutable,
}

#[derive(Clone, Copy)]
pub struct Type {
    pub v: TypeVariant,
    pub indir: Indir,
    pub refkind: ReferenceKind,
}

impl Type {
    pub fn new(v: TypeVariant, refkind: ReferenceKind) -> Self {
        Type {
            v,
            indir: Indir::none(),
            refkind,
        }
    }

    pub fn with_indir(v: TypeVariant, indir: Indir, refkind: ReferenceKind) -> Self {
        Type { v, indir, refkind }
    }

    pub fn is_reference(&self) -> bool {
        return self.indir.len() == 0 && self.refkind == ReferenceKind::None;
    }
}

impl From<TypeVariant> for Type {
    fn from(v: TypeVariant) -> Self {
        Type {
            v,
            indir: Indir::none(),
            refkind: ReferenceKind::None,
        }
    }
}

#[derive(Clone)]
pub struct IdentPair<'ast> {
    pub mutable: bool,
    pub ident: &'ast str,
    pub t: Option<Type>,
}

#[derive(Clone, Copy)]
pub enum TypeVariant {
    Unreachable,
    Void,
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
}

impl<'ast> From<&'ast str> for TypeVariant {
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
