use std::collections::{hash_map::Entry, HashMap};

use w_codegen::{Serializer, WASMType};

use crate::types::typ::Type;

#[derive(Default)]
pub struct Locals(Vec<String>, HashMap<WASMType, Vec<bool>>);

impl Locals {
    pub fn register_local(&mut self, x: String) {
        self.0.push(x)
    }

    pub fn get_temp_local(&mut self, t: Type) -> (String, usize) {
        let r = t.resolve();
        match self.1.entry(r) {
            Entry::Occupied(x) => {
                let x = x.get_mut();
                if let Some(l) = x.iter().position(|&x| x) {
                    (format!("~{}{}", r, l), l)
                } else {
                    let l = x.len();
                    x.push(false);
                    (format!("~{}{}", r, l), l)
                }
            }
            Entry::Vacant(x) => {
                x.insert(vec![false]);
                (format!("~{}0", r), 0)
            }
        }
    }

    pub fn free_temp_local<S: Serializer>(&mut self, module: &mut S, t: Type, index: usize) {
        self.1.get_mut(&t.resolve()).unwrap()[index] = true;
    }
}
