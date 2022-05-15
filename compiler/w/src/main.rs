use std::{
    env,
    io::{self, Read},
    path::PathBuf,
};

use w_session::{diag::emitter::DefaultEmitter, source_map::loader::FileLoader, Session};

fn usage() {
    println!(
        r"

USAGE:
w [OPTIONS] INPUT

DESCRIPTION:
w is a compiler toolchain that emits unoptimized WebAssembly with a contemporary Rust-like syntax, and C-like semantics.
to learn more please visit https://github.com/romdotdog/w

OPTIONS:
    -h, --help          Display this message
        --emit [wasm|wat]
	-v, --verbose       
"
    );
}

/// # Panics
/// if matching was unsuccessful
pub fn handle_options(args: &[String]) -> Option<getopts::Matches> {
    if args.is_empty() {
        usage();
        return None;
    }

    let mut options = getopts::Options::new();
    options.optflag("h", "help", "display this message and exit");
    options.optflag("V", "version", "print version information and exit");
    options.optopt(
        "",
        "emit",
        "comma separated list of formats to emit",
        "[wasm|wat]",
    );
    options.optopt("", "color", "configure coloring", "[auto|always|never]");

    let matches = options.parse(args).unwrap_or_else(|e| panic!("{}", e));

    if matches.opt_present("help") {
        usage();
        return None;
    }

    if matches.opt_present("version") {
        // TODO
        return None;
    }

    Some(matches)
}

enum SourcePath {
    Source(String),
    Loader(String),
}

fn make_input(free_matches: &[String]) -> Option<SourcePath> {
    if free_matches.len() == 1 {
        let file_path = &free_matches[0];
        if file_path == "-" {
            let mut src = String::new();
            assert!(
                io::stdin().read_to_string(&mut src).is_ok(),
                "stdin contains invalid utf-8"
            );
            return Some(SourcePath::Source(src));
        }
        return Some(SourcePath::Loader(file_path.to_string()));
    }

    None
}

fn main() {
    let args: Vec<String> = env::args_os()
        .enumerate()
        .map(|(i, arg)| {
            arg.into_string()
                .unwrap_or_else(|arg| panic!("argument {} is not valid Unicode: {:?}", i, arg))
        })
        .skip(1)
        .collect();

    let matches = match handle_options(&args) {
        Some(matches) => matches,
        None => return,
    };

    let entry = make_input(&matches.free);
    let session = Session::new(FileLoader, DefaultEmitter);

    let src = match entry {
        Some(SourcePath::Loader(name)) => session
            .source_map()
            .load_source(PathBuf::from(&name))
            .unwrap_or_else(|name| panic!("loader could not find file with name {:?}", name)),
        Some(SourcePath::Source(src)) => session
            .source_map()
            .register_source(PathBuf::from("./main.w"), src), // TODO: fix
        None => todo!(),
    };

    session.compile(src);
}
