use crate::{types::constant::Constant, Type};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Clone, Copy)]
pub enum Binding {
    Type(Type),
    Constant(Constant),
}

#[derive(Default)]
pub struct SymbolStack<'ast> {
    table: HashMap<&'ast str, Vec<Binding>>,
    stack: Vec<&'ast str>,
}

impl<'ast> SymbolStack<'ast> {
    pub fn get_top(&self) -> usize {
        self.stack.len()
    }

    pub fn free_frame(&mut self, top: usize) {
        for var in self.stack.drain(top..) {
            self.free_var(var);
        }
    }

    pub fn find(&mut self, name: &'ast str) -> Option<Binding> {
        self.table.get(name).and_then(|s| s.last()).copied()
    }

    fn free_var(&mut self, name: &'ast str) {
        let v = self.table.get(name).unwrap();
        if v.len() == 1 {
            self.table.remove(name);
        } else {
            v.pop();
        }
    }

    pub fn push(&mut self, name: &'ast str, binding: Binding) {
        self.stack.push(name);
        match self.table.entry(name) {
            Entry::Occupied(x) => x.get_mut().push(binding),
            Entry::Vacant(x) => {
                x.insert(vec![binding]);
            }
        };
    }
}
