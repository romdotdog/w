use super::{itemref::ItemRef, meta::Meta};

#[derive(Clone, Copy)]
pub struct Type(Meta, ItemRef);
