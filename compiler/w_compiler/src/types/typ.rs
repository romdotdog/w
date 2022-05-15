use super::{
    itemref::{HeapType, ItemRef, StackType},
    meta::{Meta, VALUE},
};

pub const VOID: Type = from_item(ItemRef::Void);
pub const UNREACHABLE: Type = from_item(ItemRef::Unreachable);
pub const I8: Type = from_item(ItemRef::HeapType(HeapType::I8));
pub const U8: Type = from_item(ItemRef::HeapType(HeapType::U8));
pub const I16: Type = from_item(ItemRef::HeapType(HeapType::I16));
pub const U16: Type = from_item(ItemRef::HeapType(HeapType::U16));
pub const I32: Type = from_item(ItemRef::StackType(StackType::I32));
pub const U32: Type = from_item(ItemRef::StackType(StackType::U32));
pub const I64: Type = from_item(ItemRef::StackType(StackType::I64));
pub const U64: Type = from_item(ItemRef::StackType(StackType::U64));
pub const F32: Type = from_item(ItemRef::StackType(StackType::F32));
pub const F64: Type = from_item(ItemRef::StackType(StackType::F64));

const fn from_item(item: ItemRef) -> Type {
    Type { meta: VALUE, item }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Type {
    pub meta: Meta,
    pub item: ItemRef,
}
