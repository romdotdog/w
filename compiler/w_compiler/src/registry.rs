use std::collections::{hash_map::Entry, HashMap};

use crate::types::{typ::Type, IdentPair};

pub enum Item<'ast> {
    Enum(HashMap<&'ast str, i64>),
    Struct(HashMap<&'ast str, (Type, i32)>),
    Union(HashMap<&'ast str, Type>),
    Global(&'ast str, Type),
    Fn(&'ast str, Vec<IdentPair<'ast>>, Type),
}

#[derive(Default)]
pub struct Registry<'ast> {
    items: Vec<Item<'ast>>,
    map: HashMap<&'ast str, usize>,
}

impl<'ast> Registry<'ast> {
    pub fn push(&mut self, name: &'ast str, item: Item<'ast>) -> Option<usize> {
        if let Entry::Vacant(x) = self.map.entry(name) {
            let r = self.items.len();
            self.items.push(item);
            x.insert(r);
            Some(r)
        } else {
            None
        }
    }

    pub fn get(&mut self, r: usize) -> Option<&Item<'ast>> {
        self.items.get(r)
    }

    pub fn resolve(&mut self, s: &str) -> Option<usize> {
        self.map.get(s).copied()
    }
}
