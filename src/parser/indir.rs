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
	pub fn pointers(u: u8, l: u8) -> Self {
		assert_eq!(0b11100000u8 & u, 0u8);
		assert!(l <= 5);
		Self((u << 3u8) | l)
	}

	pub fn add(&mut self, mutable: bool) {
		let len = self.len();
		assert!(len <= 4u8);
		assert!(self.0.leading_zeros() as u8 >= 5u8 - len);
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
		unsafe { self.sub_unchecked() }
	}

	/// invariants:
	/// last three bits must be 1 or above
	pub unsafe fn sub_unchecked(&mut self) {		
		self.0 -= 1u8;
		self.0 = self.0 & ((0b00001000 << self.len()) - 1u8); // zero what's unneeded
	}

	pub fn len(&mut self) -> u8 {
		self.0 & 0b111u8
	}

	#[cfg(test)]
	pub fn into_inner(&self) -> u8 {
		self.0
	}
}

#[cfg(test)]
mod tests {
	use super::Indir;

	#[test]
	fn pointers() {
		assert_eq!(Indir::pointers(0b010, 3).into_inner(), 0b00010_011);
		assert_eq!(Indir::pointers(0b11111, 5).into_inner(), 0b11111_101);
	}

	#[test]
	fn add() {
		let mut indir = Indir::none();
		assert_eq!(indir.into_inner(), 0b00000_000);
		indir.add(false);
		// *x
		assert_eq!(indir.into_inner(), 0b00000_001);
		indir.add(true);
		// *mut *x
		assert_eq!(indir.into_inner(), 0b00010_010);
		indir.add(false);
		// **mut *x
		assert_eq!(indir.into_inner(), 0b00010_011);
		indir.add(true);
		// *mut **mut *x
		assert_eq!(indir.into_inner(), 0b01010_100);
		indir.add(false);
		// **mut **mut *x
		assert_eq!(indir.into_inner(), 0b01010_101);
	}

	#[test]
	fn sub() {
		let mut indir = Indir::pointers(0b01010, 5);
		// **mut **mut *x
		assert_eq!(indir.into_inner(), 0b01010_101);
		indir.sub();
		// *mut **mut *x
		assert_eq!(indir.into_inner(), 0b01010_100);
		indir.sub();
		// **mut *x
		assert_eq!(indir.into_inner(), 0b00010_011);
		indir.sub();
		// *mut *x
		assert_eq!(indir.into_inner(), 0b00010_010);
		indir.sub();
		// *x
		assert_eq!(indir.into_inner(), 0b00000_001);
		indir.sub();
		// x
		assert_eq!(indir.into_inner(), 0b00000_000);
	}
}