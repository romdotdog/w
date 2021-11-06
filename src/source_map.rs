use std::{cell::RefCell, fs::read_to_string, rc::Rc};

use crate::source::Source;

pub trait Loader {
    fn load(&self, name: &str) -> Option<String>;
}

pub struct FileLoader;

impl Loader for FileLoader {
    fn load(&self, name: &str) -> Option<String> {
        read_to_string(name).ok()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SourceRef(usize);

pub struct SourceMap {
    loader: Box<dyn Loader>,
    sources: RefCell<Vec<Rc<Source>>>,
}

impl SourceMap {
    pub fn new(loader: Box<dyn Loader>) -> SourceMap {
        SourceMap {
            loader,
            sources: RefCell::new(Vec::new()),
        }
    }

    pub fn load_source(&self, name: String) -> Option<SourceRef> {
        let src = self.loader.load(&name)?;
        Some(self.register_source(name, src))
    }

    pub fn register_source(&self, name: String, src: String) -> SourceRef {
        let mut sources = self.sources.borrow_mut();
        let r = sources.len();
        sources.push(Rc::new(Source::new(name, src)));
        SourceRef(r)
    }

    pub fn get_source(&self, i: SourceRef) -> Rc<Source> {
        Rc::clone(&self.sources.borrow_mut()[i.0])
    }
}
