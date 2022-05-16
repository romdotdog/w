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
