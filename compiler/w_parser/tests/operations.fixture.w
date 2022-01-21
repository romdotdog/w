fn main() {
	let a = 1;
	let b = 2;
	let mut c = 3;
	let mut d = 4;
	let <unknown> = 5;
	let <unknown> = 6;
	let <unknown>;
	7;
	let $ = 8;
	a > b >= c < d <= e == f != g;
	a + b - c * d / e % f;
	a ^ b & c | d >> e << f;
	a * (b + c);
	a = b === c >>= d;
	!~<f64!><f64>--+++-&*a++--;
}

// "labels cannot be used as identifiers" - 9:6+2
// "invalid identifier here" - 10:6+1
// "invalid identifier here" - 11:6+1
// "missing a semicolon or closing brace" - 11:8+1
