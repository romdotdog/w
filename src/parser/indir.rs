// TODO:
// * tests

#[derive(Debug, Copy, Clone)]
pub struct Indir(u8);

impl Indir {
	pub fn none() -> Self {
		Self(0)
	}

	/// example inputs:
	///
	/// u: `0b1`
	/// output: *mut x
	///
	/// u: `0b10`
	/// output: *mut *x
	/// 
	/// u: `0b10101`
	/// output: *mut **mut **mut x
	pub fn pointers(u: u8) -> Self {
		assert_eq!(0b11100000u8 & u, 0u8);
		Self((u << 3u8) | (5u8 - u.leading_zeros() as u8))
	}

	pub fn add(&mut self, mutable: bool) {
		let len = self.len();
		assert!(len <= 4u8);
		assert_eq!(self.0.leading_zeros() as u8, 5u8 - len);
		unsafe { self.add_unchecked(mutable) }
	} 

	/// invariants:
	/// last three bits must be 4 or below
	/// first 5 - len bits must be zeroed
	pub unsafe fn add_unchecked(&mut self, mutable: bool) {
		let a = (mutable as u8) << 3u8 << self.len();
		self.0 = self.0 & !a | a; // override the bit
		self.0 += 1u8; // add one to length
	} 


	pub fn sub(&mut self) {
		assert!(self.len() >= 1);
		unsafe { self.sub() }
	}

	/// invariants:
	/// last three bits must be 1 or above
	pub unsafe fn sub_unchecked(&mut self) {		
		self.0 -= 1u8;
		self.0 = self.0 & ((0b00001000 << self.len()) - 1u8); // zero what's needed
	}

	pub fn len(&mut self) -> u8 {
		self.0 & 0b111u8
	}
}