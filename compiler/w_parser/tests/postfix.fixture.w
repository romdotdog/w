fn main(): void {
	f.a.b;
	a[1][2];
	a(1, 2, 3);
	b(2 + 2, 3 + 3);
}

// "missing identifier here" - 3:4+4
// "']' expected here" - 5:5+1
// "unexpected token" - 8:7+1
// "')' expected here" - 10:1+1
