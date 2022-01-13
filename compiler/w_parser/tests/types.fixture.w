fn main(mut a: *mut *mut **i32, mut b: *i64, c: u32, d: u64, e: f32, f: f64, g): ****void {
	let a: i32 = 1;
	let b: *f64 = &2.6;
	let c: *****i32 = b;
	let d: *****f32 = c;
	let e: *****f32 = c;
	let f: struct { foo: i32; bar: i32 } = e;
	let g: union { foo: i32 } = f;
	let h: UnresolvedType = g;
}

// "missing type here" - 1:80+1
// "at most only 5 levels of indirection are allowed" - 5:14+1
// "at most only 5 levels of indirection are allowed" - 6:14+35
// "found malformed type" - 7:9+1
