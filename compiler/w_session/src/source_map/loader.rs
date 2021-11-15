#![allow(clippy::module_name_repetitions)]

pub trait Loader {
    fn load(&self, name: &str) -> Option<String>;
}

pub struct FileLoader;

impl Loader for FileLoader {
    fn load(&self, name: &str) -> Option<String> {
        std::fs::read_to_string(name).ok()
    }
}
