pub mod loader;
use loader::Loader;

use crate::source::Source;
use std::{cell::RefCell, rc::Rc};

pub struct SourceMap<L: Loader> {
    loader: L,
    sources: RefCell<Vec<Rc<Source>>>,
}

impl<L: Loader> SourceMap<L> {
    pub fn new(loader: L) -> SourceMap<L> {
        SourceMap {
            loader,
            sources: RefCell::new(Vec::new()),
        }
    }

    pub fn load_source(&self, name: String) -> Result<Rc<Source>, String> {
        match self.loader.load(&name) {
            Some(src) => Ok(self.register_source(name, src)),
            None => Err(name),
        }
    }

    pub fn register_source(&self, name: String, src: String) -> Rc<Source> {
        let mut sources = self.sources.borrow_mut();
        sources.push(Rc::new(Source::new(name, src)));
        Rc::clone(sources.last().unwrap())
    }
}
