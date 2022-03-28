#![allow(clippy::module_name_repetitions)]

use std::path::Path;

pub trait Loader {
    fn load(&self, path: &Path) -> Option<String>;
}

pub struct FileLoader;

impl Loader for FileLoader {
    fn load(&self, path: &Path) -> Option<String> {
        std::fs::read_to_string(path).ok()
    }
}
