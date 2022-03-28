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
    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn add(self, mutable: bool) -> Self {
        let len = self.len();
        assert!(len <= 4_u8);
        assert!(self.0.leading_zeros() >= 5 - u32::from(len));
        unsafe { self.add_unchecked(mutable) }
    }

    /// # Safety
    /// last three bits must be 4 or below
    /// first 5 - len bits must be zeroed
    #[must_use]
    pub unsafe fn add_unchecked(mut self, mutable: bool) -> Self {
        let a = u8::from(mutable) << 3_u8 << self.len();
        self.0 = self.0 & !a | a; // override the bit
        self.0 += 1_u8; // add one to length
        self
    }

    /// # Panics
    /// if attempting to sub with 0 pointers
    #[must_use]
    pub fn sub(self) -> Self {
        assert!(self.len() != 0);
        unsafe { self.sub_unchecked() }
    }

    /// # Safety
    /// last three bits must be 1 or above
    #[must_use]
    pub unsafe fn sub_unchecked(mut self) -> Self {
        self.0 -= 1_u8;
        self.0 &= (0b0000_1000 << self.len()) - 1_u8; // zero what's unneeded
        self
    }

    // len is O(1) so this lint can be allowed
    #[allow(clippy::len_without_is_empty)]
    pub fn len(self) -> u8 {
        self.0 & 0b111_u8
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
        indir = indir.add(false);
        // *x
        assert_eq!(indir.into_inner(), 0b00000_001);
        indir = indir.add(true);
        // *mut *x
        assert_eq!(indir.into_inner(), 0b00010_010);
        indir = indir.add(false);
        // **mut *x
        assert_eq!(indir.into_inner(), 0b00010_011);
        indir = indir.add(true);
        // *mut **mut *x
        assert_eq!(indir.into_inner(), 0b01010_100);
        indir = indir.add(false);
        // **mut **mut *x
        assert_eq!(indir.into_inner(), 0b01010_101);
    }

    #[test]
    fn sub() {
        let mut indir = Indir::pointers(0b01010, 5);
        // **mut **mut *x
        assert_eq!(indir.into_inner(), 0b01010_101);
        indir = indir.sub();
        // *mut **mut *x
        assert_eq!(indir.into_inner(), 0b01010_100);
        indir = indir.sub();
        // **mut *x
        assert_eq!(indir.into_inner(), 0b00010_011);
        indir = indir.sub();
        // *mut *x
        assert_eq!(indir.into_inner(), 0b00010_010);
        indir = indir.sub();
        // *x
        assert_eq!(indir.into_inner(), 0b00000_001);
        indir = indir.sub();
        // x
        assert_eq!(indir.into_inner(), 0b00000_000);
    }
}
