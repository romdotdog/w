use std::{
    collections::{hash_map::Entry, HashMap},
    mem,
};

use w_codegen::{Serializer, WASMType};

use crate::types::typ::Type;

#[derive(Default)]
pub struct Flow(Vec<(String, WASMType)>, HashMap<WASMType, Vec<bool>>); // TODO: &'ast str?

impl Flow {
    pub fn register_local(&mut self, s: String, t: Type) {
        self.0.push((s, t.resolve()))
    }

    pub fn get_temp_local(&mut self, t: Type) -> (String, usize) {
        let r = t.resolve();
        match self.1.entry(r) {
            Entry::Occupied(mut x) => {
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

    pub fn free_temp_local<S: Serializer>(&mut self, t: Type, index: usize) {
        self.1.get_mut(&t.resolve()).unwrap()[index] = true;
    }

    // clears the struct
    pub fn vars(&mut self) -> Vec<(String, WASMType)> {
        let mut res = mem::take(&mut self.0);
        for (t, v) in self.1.drain() {
            for n in 0..v.len() {
                res.push((format!("~{}{}", t, n), t))
            }
        }
        self.1.clear();
        res
    }
}
