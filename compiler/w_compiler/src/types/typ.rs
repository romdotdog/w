use std::{fmt::Display, panic};

use w_codegen::WASMType;

use super::{
    itemref::{HeapType, ItemRef, StackType},
    meta::{Meta, VALUE},
};

pub const VOID: Type = from_item(ItemRef::Void);
pub const UNREACHABLE: Type = from_item(ItemRef::Unreachable);
//pub const I8: Type = from_item(ItemRef::HeapType(HeapType::I8));
//pub const U8: Type = from_item(ItemRef::HeapType(HeapType::U8));
//pub const I16: Type = from_item(ItemRef::HeapType(HeapType::I16));
//pub const U16: Type = from_item(ItemRef::HeapType(HeapType::U16));
pub const I32: Type = from_item(ItemRef::StackType(StackType::I32));
pub const U32: Type = from_item(ItemRef::StackType(StackType::U32));
pub const I64: Type = from_item(ItemRef::StackType(StackType::I64));
pub const U64: Type = from_item(ItemRef::StackType(StackType::U64));
pub const F32: Type = from_item(ItemRef::StackType(StackType::F32));
pub const F64: Type = from_item(ItemRef::StackType(StackType::F64));

const fn from_item(item: ItemRef) -> Type {
    Type { meta: VALUE, item }
}

#[derive(Clone, Copy)]
pub struct Type {
    pub meta: Meta,
    pub item: ItemRef,
}

impl Type {
    pub fn resolve(self) -> WASMType {
        if self.meta.len() > 0 {
            WASMType::I32
        } else {
            match self.item {
                ItemRef::Void => todo!(),
                ItemRef::Unreachable => WASMType::I32, // maybe?
                ItemRef::HeapType(_) => todo!(),
                ItemRef::Ref(_) => todo!(),
                ItemRef::StackType(t) => match t {
                    StackType::I32 | StackType::U32 => WASMType::I32,
                    StackType::I64 | StackType::U64 => WASMType::I64,
                    StackType::F32 => WASMType::F32,
                    StackType::F64 => WASMType::F64,
                },
            }
        }
    }

    // cannot match mut with non mut var
    pub fn is_strict(self, other: Type) -> bool {
        self.item == other.item && self.meta == other.meta
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.meta.fmt(f)?;
        self.item.fmt(f)
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        println!("{} - {}", self, other);
        self.item == other.item && self.meta.assignable_to(other.meta)
            || self.item == ItemRef::Unreachable
            || other.item == ItemRef::Unreachable
    }
}
