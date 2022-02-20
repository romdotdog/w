fn main() {
	/*
		ooo fancy block comment
	*/
	let a = 1;
	let b = 2;
	let mut c = 3;
	let mut d = 4;
	let $f = 5; // illegal
	let ! = 6; // illegal
	let = 7; // legal (just kidding, that's illegal)
	let $ = 8; // legal

	a > b >= c < d <= e == f != g;
	a + b - c * d / e % f;
	a ^ b & c | d >> e << f;
	a * (b + c); // parentheses
	a = b === c >>= d; // right-associativity
	!~<f64!><f64>--+++-&*a++--;
	foo.field;
	Foo->field;
}

// comment that ends with eof