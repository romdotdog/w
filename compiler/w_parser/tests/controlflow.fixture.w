fn main() {
	if 1 "good";
	if 0 "bad" else "good";
	return;
	return 1;
	$hi: loop let i = 0 {
		br -> $hi if ++i < 10;
		br -> $hi;
	};
	loop let i = 0 {
	};
	$hi2: {
		br -> $hi2;
	};
	loop return 1;
	{
	};
	return 2 + 2;
}

// "loop body may only be a block" - 21:7+8
// "missing a semicolon or closing brace" - 21:16+1
