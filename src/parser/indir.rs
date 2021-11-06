use std::fmt::Display;

#[derive(Copy, Clone)]
pub struct Indir(u8);

impl Indir {
    pub fn none() -> Self {
        Self(0)
    }

    /// # Examples
    ///
    /// u: `0b1` l: 1
    /// output: *mut x
    ///
    /// u: `0b10` l: 2
    /// output: *mut *x
    ///
    /// u: `0b10101` l: 5
    /// output: *mut **mut **mut x
    /// # Panics
    /// if l > 5
    /// if u has more than 5 bitflags
    pub fn pointers(u: u8, l: u8) -> Self {
        assert_eq!(0b1110_0000_u8 & u, 0_u8);
        assert!(l <= 5);
        Self((u << 3_u8) | l)
    }

    /// # Panics
    /// if trying to add indir with .len() of 5
    /// first 5 - len bits aren't zeroed
    pub fn add(&mut self, mutable: bool) {
        let len = self.len();
        assert!(len <= 4_u8);
        assert!(self.0.leading_zeros() >= 5 - len as u32);
        unsafe { self.add_unchecked(mutable) }
    }

    /// # Safety
    /// last three bits must be 4 or below
    /// first 5 - len bits must be zeroed
    pub unsafe fn add_unchecked(&mut self, mutable: bool) {
        let a = (mutable as u8) << 3_u8 << self.len();
        self.0 = self.0 & !a | a; // override the bit
        self.0 += 1_u8; // add one to length
    }

    /// # Panics
    /// if attempting to sub with 0 pointers
    pub fn sub(&mut self) {
        assert!(!self.is_empty());
        unsafe { self.sub_unchecked() }
    }

    /// # Safety
    /// last three bits must be 1 or above
    pub unsafe fn sub_unchecked(&mut self) {
        self.0 -= 1_u8;
        self.0 &= (0b0000_1000 << self.len()) - 1_u8; // zero what's unneeded
    }

    pub fn len(&self) -> u8 {
        self.0 & 0b111_u8
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[cfg(test)]
    pub fn into_inner(self) -> u8 {
        self.0
    }
}

impl Display for Indir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let l = self.len();
        let mut n = 0b0000_1000_u8;
        for _ in 0..l {
            if self.0 & n == 0 {
                write!(f, "*")?;
            } else {
                write!(f, "*mut ")?;
            }
            n <<= 1_u8;
        }
        Ok(())
    }
}

#[allow(clippy::unusual_byte_groupings)]
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
