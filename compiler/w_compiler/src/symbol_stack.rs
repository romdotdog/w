use std::collections::HashMap;
use crate::Type;

#[derive(Default)]
pub struct SymbolStack<'ast> {
	table: HashMap<&'ast str, Vec<Type>>,
	stack: Vec<&'ast str>
}

impl<'ast> SymbolStack<'ast> {
	pub fn get_top(&self) -> usize {
		self.stack.len()
	}

	pub fn free_frame(&mut self, top: usize) {
		for var in self.stack.drain(top..) {
			self.free_var();
		}
	}

	pub fn find(&mut self, name: &'ast str) -> Option<Type> {
		self.table.get(name).map(|s| s.last())
	}

	fn free_var(&mut self, name: &'ast str) {
		assert!(self.table.contains(name));
		match &self.table.get(name) {
			[_] => {
				self.table.remove(name);	
			}
			e => {
				e.pop();
			}
		}
	}
}


