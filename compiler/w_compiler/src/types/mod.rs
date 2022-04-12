use indir::Indir;

pub mod indir;

#[derive(Clone, PartialEq, Eq)]
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

    fn is_reference(&self) -> bool {
        return self.indir.len() == 0 && self.refkind == ReferenceKind::None;
    }
}

impl<'ast> From<TypeVariant<'ast>> for Type<'ast> {
    fn from(v: TypeVariant<'ast>) -> Self {
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
    pub t: Option<Type<'ast>>,
}

#[derive(Clone)]
pub enum TypeVariant<'ast> {
    Unreachable,
    Void,
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
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

impl TypeVariant<'_> {
    fn isImplicitlyAssignableTo(&self, target: &TypeVariant) -> bool {
        match self {
            TypeVariant::U31 => match target {
                TypeVariant::U31
                | TypeVariant::U32
                | TypeVariant::U63
                | TypeVariant::U64
                | TypeVariant::I32
                | TypeVariant::I64
                | TypeVariant::F64 => true,
                _ => false,
            },
            TypeVariant::U32 => match target {
                TypeVariant::U32
                | TypeVariant::U63
                | TypeVariant::U64
                | TypeVariant::I64
                | TypeVariant::F64 => true,
                _ => false,
            },
            TypeVariant::U63 => match target {
                TypeVariant::U63 | TypeVariant::U64 | TypeVariant::I64 => true,
                _ => false,
            },
            TypeVariant::U64 => match target {
                TypeVariant::U64 => true,
                _ => false,
            },
            TypeVariant::I64 => match target {
                TypeVariant::I64 => true,
                _ => false,
            },
            TypeVariant::I32 => match target {
                TypeVariant::I32 | TypeVariant::I64 | TypeVariant::F64 => true,
                _ => false,
            },
            TypeVariant::F32 => match target {
                TypeVariant::F32 | TypeVariant::F64 => true,
                _ => false,
            },
            TypeVariant::F64 => match target {
                TypeVariant::F64 => true,
                _ => false,
            },
            _ => false,
        }
    }
}
