fn main() {
	if 1 "good";
	if 0 "bad" else "good";

	// i will be in the inner scope
	loop let i = 0 {
		br if ++i < 10;
		br;
	};

	// error: need semicolon after 1
	loop return 1 {};
	
	return 2 + 2;
}