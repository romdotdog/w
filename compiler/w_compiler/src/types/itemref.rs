use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum HeapType {
    I8,
    U8,
    I16,
    U16,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StackType {
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ItemRef {
    Void,
    Unreachable,
    HeapType(HeapType),
    StackType(StackType),
    Ref(usize),
}

impl ItemRef {
    pub fn from_str(x: &str) -> Option<ItemRef> {
        match x {
            "void" => Some(ItemRef::Void),
            "i8" => Some(ItemRef::HeapType(HeapType::I8)),
            "u8" => Some(ItemRef::HeapType(HeapType::U8)),
            "i16" => Some(ItemRef::HeapType(HeapType::I16)),
            "u16" => Some(ItemRef::HeapType(HeapType::U16)),
            "i32" => Some(ItemRef::StackType(StackType::I32)),
            "u32" => Some(ItemRef::StackType(StackType::U32)),
            "i64" => Some(ItemRef::StackType(StackType::I64)),
            "u64" => Some(ItemRef::StackType(StackType::U64)),
            "f32" => Some(ItemRef::StackType(StackType::F32)),
            "f64" => Some(ItemRef::StackType(StackType::F64)),
            _ => None,
        }
    }
}

impl Display for ItemRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemRef::Void => write!(f, "void"),
            ItemRef::Unreachable => write!(f, "unreachable"),
            ItemRef::HeapType(x) => match x {
                HeapType::I8 => write!(f, "i8"),
                HeapType::U8 => write!(f, "u8"),
                HeapType::I16 => write!(f, "i16"),
                HeapType::U16 => write!(f, "u16"),
            },
            ItemRef::StackType(x) => match x {
                StackType::I32 => write!(f, "i32"),
                StackType::U32 => write!(f, "u32"),
                StackType::I64 => write!(f, "i64"),
                StackType::U64 => write!(f, "u64"),
                StackType::F32 => write!(f, "f32"),
                StackType::F64 => write!(f, "f64"),
            },
            ItemRef::Ref(_) => write!(f, "<ref>"),
        }
    }
}
