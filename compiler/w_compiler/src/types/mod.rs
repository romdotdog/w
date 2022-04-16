use std::convert::TryInto;

use indir::Indir;
use w_codegen::WASMType;

pub mod indir;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    None,
    Immutable,
    Mutable,
}

#[derive(Clone, Copy, PartialEq, Eq)]
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

    pub fn resolve(&self) -> WASMType {
        assert!(
            self.refkind == ReferenceKind::None,
            "attempt to resolve a reference"
        );
        if self.indir.len() > 0 {
            WASMType::I32
        } else {
            match self.v {
                TypeVariant::I32 => WASMType::I32,
                TypeVariant::I64 => WASMType::I64,
                TypeVariant::U32 => WASMType::I32,
                TypeVariant::U64 => WASMType::I64,
                TypeVariant::F32 => WASMType::F32,
                TypeVariant::F64 => WASMType::F64,
            }
        }
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TypeVariant {
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
        }
    }
}

#[derive(Clone)]
pub struct IdentPair<'ast> {
    pub mutable: bool,
    pub ident: &'ast str,
    pub t: Option<Type>,
}

pub enum Constant {
    U31(u32),
    U63(u64),
    Fxx(f32),
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    F64(f64),
}

impl Constant {
    pub fn from_u64(x: u64) -> Constant {
        if x <= i32::MAX as u64 {
            Constant::U31(x as u32)
        } else if x <= u32::MAX as u64 {
            Constant::U32(x as u32)
        } else if x <= i64::MAX as u64 {
            Constant::U63(x)
        } else {
            Constant::U64(x)
        }
    }

    pub fn from_i64(x: i64) -> Constant {
        if let Ok(x) = x.try_into() {
            Self::from_u64(x)
        } else if x >= i32::MIN as i64 {
            Constant::I32(x as i32)
        } else {
            Constant::I64(x)
        }
    }

    pub fn from_f64(r64: f64) -> Constant {
        #[allow(clippy::cast_possible_truncation)]
        let r32 = r64 as f32;

        // TODO: audit
        if (f64::from(r32) - r64).abs() < f64::EPSILON {
            // check that r64 can be represented in 32 bits
            let r32a = r32.abs();
            if f64::from(r32a + 1.0 - r32a) > 1.0 - f64::EPSILON {
                // check that 1 + |r32| is "correct"
                return Constant::Fxx(r32);
            }
        }

        return Constant::F64(r64);
    }
}
