pub mod loader;
pub mod source;
use loader::Loader;

use source::Source;
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

    /// # Errors
    /// returns the original `name` string
    pub fn load_source(&self, name: String) -> Result<Rc<Source>, String> {
        match self.loader.load(&name) {
            Some(src) => Ok(self.register_source(name, src)),
            None => Err(name),
        }
    }

    pub fn register_source(&self, name: String, src: String) -> Rc<Source> {
        let mut sources = self.sources.borrow_mut();
        let r = sources.len();
        sources.push(Rc::new(Source::new(name, src)));
        Rc::clone(&sources[r])
    }
}
