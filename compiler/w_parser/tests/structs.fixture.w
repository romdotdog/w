enum <unknown> {
}

enum <unknown> {
}

enum Foo {
	Foo = 1,
	Bar,
	Baz = 5,
}

struct <unknown> {
}

struct <unknown> {
}

struct Foo {
	mut foo: i32;
	bar: i32;
}

union <unknown> {
}

union <unknown> {
}

union Foo {
	foo: i32;
	mut bar: i32;
}

// "missing identifier here" - 1:7+1
// "invalid identifier here" - 2:8+1
// "missing identifier here" - 9:6+1
// "invalid identifier here" - 10:7+1
// "missing identifier here" - 17:5+1
// "invalid identifier here" - 18:6+1
