fn main() {
	if 1 "good";
	if 0 "bad" else "good";
	
	return;
	return 1;

	// i will be in the inner scope
	$hi: loop let i = 0 {
		br -> $hi if ++i < 10;
		br -> $hi;
	};

	loop let i = 0 {};

	$hi2: {
		br -> $hi2;
	};

	let foo = {
		bar + 1
	};

	let bar = {
		br bar + 1
	};

	// error: need semicolon after 1
	loop return 1 {};
	
	return 2 + 2;
}