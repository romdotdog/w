static foo = 1;

static hello = "Hello, world!";

static naughty_static = "no semicolon";

fn main() {
	static scoped_static = "hello, world!";
}

// "missing a semicolon or closing brace" - 5:1+2
