const IS_MUTABLE: u32 = 1 << 30;
const IS_REFERENCE: u32 = 1 << 31;

pub const VALUE: Meta = Meta(0);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Meta(u32);

impl Meta {
    #[must_use]
    pub fn ref_(self, mutable: bool) -> Option<Meta> {
        if self.len() >= 25 {
            return None;
        }

        let mut meta = self.0;
        meta += 1;
        meta |= u32::from(mutable) << 5 << self.len();

        return Some(Meta(meta));
    }

    #[must_use]
    pub fn deref(self) -> Option<Meta> {
        if self.len() == 0 {
            return None;
        }

        let a = 0b1000_0000 << self.len();
        let mut meta = if self.0 & a == 0 {
            self.unset_mutable()
        } else {
            self.set_mutable()
        };

        meta.0 -= 1;
        meta.0 &= (a - 1) | IS_MUTABLE | IS_REFERENCE; // TODO: audit
        return Some(meta);
    }

    #[must_use]
    pub fn len(self) -> u32 {
        (self.0 & 0b11111) as u32
    }

    #[must_use]
    pub fn is_mutable(self) -> bool {
        self.0 & IS_MUTABLE != 0
    }

    #[must_use]
    pub fn set_mutable(self) -> Meta {
        Meta(self.0 | IS_MUTABLE)
    }

    #[must_use]
    pub fn unset_mutable(self) -> Meta {
        Meta(self.0 & !IS_MUTABLE)
    }

    #[must_use]
    pub fn is_reference(self) -> bool {
        self.0 & IS_REFERENCE != 0
    }

    #[must_use]
    pub fn set_reference(self) -> Meta {
        Meta(self.0 | IS_REFERENCE)
    }

    #[must_use]
    pub fn unset_reference(self) -> Meta {
        Meta(self.0 & !IS_REFERENCE)
    }
}
