pub mod loader;
pub mod source;
use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    path::PathBuf,
};

use appendlist::AppendList;
use loader::Loader;

use source::Source;

pub struct SourceMap<L: Loader> {
    loader: L,
    sources: AppendList<Source>,
    map: RefCell<HashMap<u64, usize>>,
}

impl<L: Loader> SourceMap<L> {
    pub fn new(loader: L) -> SourceMap<L> {
        SourceMap {
            loader,
            map: RefCell::new(HashMap::new()),
            sources: AppendList::new(),
        }
    }

    /// # Errors
    /// returns the original `name` string
    pub fn load_source(&self, path: PathBuf) -> Result<&Source, PathBuf> {
        match self.loader.load(&path) {
            Some(src) => {
                let mut hasher = DefaultHasher::new();
                src.hash(&mut hasher);
                if let Some(&s) = self.map.borrow().get(&hasher.finish()) {
                    return Ok(&self.sources[s]);
                }
                Ok(self.register_source(path, src))
            }
            None => Err(path),
        }
    }

    pub fn register_source(&self, path: PathBuf, src: String) -> &Source {
        let r = self.sources.len();

        let mut hasher = DefaultHasher::new();
        src.hash(&mut hasher);
        self.map.borrow_mut().insert(hasher.finish(), r);

        self.sources.push(Source::new(path, src));
        &self.sources[r]
    }
}
