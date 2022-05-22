use crate::{types::constant::Constant, Type};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Clone, Copy)]
pub enum Binding {
    Type(Type),
    Constant(Constant),
}

#[derive(Default)]
pub struct SymbolStack<'ast> {
    table: HashMap<&'ast str, Binding>,
    stack: Vec<&'ast str>,
}

impl<'ast> SymbolStack<'ast> {
    pub fn get_top(&self) -> usize {
        self.stack.len()
    }

    pub fn free_frame(&mut self, top: usize) {
        for var in self.stack.drain(top..) {
            // free var
            self.table.remove(var);
        }
    }

    pub fn find(&mut self, name: &'ast str) -> Option<Binding> {
        self.table.get(name).copied()
    }

    #[must_use]
    pub fn push(&mut self, name: &'ast str, binding: Binding) -> bool {
        self.stack.push(name);
        match self.table.entry(name) {
            Entry::Occupied(_) => false,
            Entry::Vacant(x) => {
                x.insert(binding);
                true
            }
        }
    }
}
