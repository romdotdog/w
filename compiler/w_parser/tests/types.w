fn main(mut a: *mut *mut **i32, mut b: *i64, c: u32, d: u64, e: f32, f: f64): ****void {
	let a: i32 = 1;
	let b: *f64 = &2.6;
	let c: *****i32 = b;
	let d: ******f32 = c; // error
	let e: struct { foo: i32 } = d;
	let f: union { foo: i32 } = e;
}