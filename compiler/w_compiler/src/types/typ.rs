use super::{
    itemref::{HeapType, ItemRef, StackType},
    meta::{Meta, VALUE},
};

pub const VOID: Type = Type(VALUE, ItemRef::Void);
pub const UNREACHABLE: Type = Type(VALUE, ItemRef::Unreachable);
pub const I8: Type = Type(VALUE, ItemRef::HeapType(HeapType::I8));
pub const U8: Type = Type(VALUE, ItemRef::HeapType(HeapType::U8));
pub const I16: Type = Type(VALUE, ItemRef::HeapType(HeapType::I16));
pub const U16: Type = Type(VALUE, ItemRef::HeapType(HeapType::U16));
pub const I32: Type = Type(VALUE, ItemRef::StackType(StackType::I32));
pub const U32: Type = Type(VALUE, ItemRef::StackType(StackType::U32));
pub const I64: Type = Type(VALUE, ItemRef::StackType(StackType::I64));
pub const U64: Type = Type(VALUE, ItemRef::StackType(StackType::U64));
pub const F32: Type = Type(VALUE, ItemRef::StackType(StackType::F32));
pub const F64: Type = Type(VALUE, ItemRef::StackType(StackType::F64));

#[derive(Clone, Copy)]
pub struct Type(Meta, ItemRef);
