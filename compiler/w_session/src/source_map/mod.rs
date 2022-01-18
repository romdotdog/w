pub mod loader;
pub mod source;
use appendlist::AppendList;
use loader::Loader;

use source::Source;

pub struct SourceMap<L: Loader> {
    loader: L,
    sources: AppendList<Source>,
}

impl<L: Loader> SourceMap<L> {
    pub fn new(loader: L) -> SourceMap<L> {
        SourceMap {
            loader,
            sources: AppendList::new(),
        }
    }

    /// # Errors
    /// returns the original `name` string
    pub fn load_source(&self, name: String) -> Result<&Source, String> {
        match self.loader.load(&name) {
            Some(src) => Ok(self.register_source(name, src)),
            None => Err(name),
        }
    }

    pub fn register_source(&self, name: String, src: String) -> &Source {
        let r = self.sources.len();
        self.sources.push(Source::new(name, src));
        &self.sources[r]
    }
}
