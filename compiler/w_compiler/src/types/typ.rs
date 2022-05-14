use w_codegen::WASMType;

const IS_REFERENCE: u32 = 1 << 12;
const IS_MUTABLE_REFERENCE: u32 = 1 << 13;
const IS_NUMBER: u32 = 1 << 14;
const IS_SIGNED: u32 = 1 << 15;
const IS_FLOAT: u32 = 1 << 16;
const IS_HIGH: u32 = 1 << 17;

pub const U32: Type = Type(IS_NUMBER);
pub const U64: Type = Type(IS_NUMBER | IS_HIGH);
pub const I32: Type = Type(IS_NUMBER | IS_SIGNED);
pub const I64: Type = Type(IS_NUMBER | IS_SIGNED | IS_HIGH);
pub const F32: Type = Type(IS_NUMBER | IS_SIGNED | IS_FLOAT);
pub const F64: Type = Type(IS_NUMBER | IS_SIGNED | IS_FLOAT | IS_HIGH);
pub const VOID: Type = Type(0);

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Type(u32);

impl Type {
    pub fn add_pointer(self, mutable: bool) -> Option<Type> {
        if self.len() >= 8 {
            return None;
        }

        self.0 += 1;
        self.0 |= u32::from(mutable) << 3 << self.len();

        return Some(self);
    }

    pub fn sub_pointer(self) -> Option<Type> {
        if self.len() <= 0 {
            return None;
        }

        self.0 -= 1;
        self.0 &= (0b0000_1000 << self.len()) - 1;

        return Some(self);
    }

    pub fn len(self) -> u32 {
        self.0 & 0b111
    }

    pub fn is_reference(self) -> bool {
        self.0 & IS_REFERENCE != 0
    }

    pub fn set_reference(self, x: bool) {
        self.0 |= IS_REFERENCE;
    }

    pub fn is_mutable_reference(self) -> bool {
        self.0 & IS_MUTABLE_REFERENCE != 0
    }

    pub fn set_mutable_reference(self, x: bool) {
        self.0 |= IS_REFERENCE;
    }

    pub fn is_number(self) -> bool {
        self.0 & IS_NUMBER != 0
    }

    pub fn is_signed(self) -> bool {
        self.0 & IS_SIGNED != 0
    }

    pub fn is_float(self) -> bool {
        self.0 & IS_FLOAT != 0
    }

    pub fn is_high(self) -> bool {
        self.0 & IS_HIGH != 0
    }

    pub fn resolve(self) -> WASMType {
        assert!(!self.is_reference(), "attempt to resolve reference");

        if self.len() > 0 {
            WASMType::I32
        } else if self.is_high() {
            if self.is_float() {
                WASMType::F64
            } else {
                WASMType::I64
            }
        } else if self.is_float() {
            WASMType::F32
        } else {
            WASMType::I32
        }
    }
}
