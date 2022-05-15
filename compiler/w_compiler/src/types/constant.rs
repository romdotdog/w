use std::convert::TryInto;

use w_codegen::Serializer;
use w_lexer::token::{BinOp, BinOpVariant};

use super::{
    expression::Expression,
    itemref::{ItemRef, StackType},
    meta::Meta,
    typ::*,
};

// arbitrary bounds
const MIN_FLOAT: i64 = -(2i64.pow(53));
const MAX_FLOAT: i64 = 2i64.pow(53);
const UMAX_FLOAT: u64 = 2u64.pow(53);

#[derive(Clone, Copy)]
enum UntypedConstant {
    Integer(i64),
    Uinteger(u64),
    Float(f64),
}

use UntypedConstant::*;
impl UntypedConstant {
    pub fn coerce_to_i64(self) -> Option<i64> {
        match self {
            Integer(x) => Some(x),
            Uinteger(x) => x.try_into().ok(),
            Float(x) => check_f64(x).map(|w| w as i64),
        }
    }

    pub fn coerce_to_i32(self) -> Option<i64> {
        self.coerce_to_i64().and_then(|x| x.try_into().ok())
    }

    pub fn coerce_to_u64(self) -> Option<u64> {
        match self {
            Integer(x) => x.try_into().ok(),
            Uinteger(x) => Some(x),
            Float(x) if x >= 0.0 => check_f64(x).map(|w| w as u64),
            _ => None,
        }
    }

    pub fn coerce_to_u32(self) -> Option<u32> {
        self.coerce_to_u64().and_then(|x| x.try_into().ok())
    }

    pub fn coerce_to_f64(self) -> Option<f64> {
        match self {
            Integer(x) if x > MIN_FLOAT && x < MAX_FLOAT => Some(x as f64),
            Uinteger(x) if x < UMAX_FLOAT => Some(x as f64),
            Float(x) => Some(x),
            _ => None,
        }
    }

    pub fn coerce_to_f32(self) -> Option<f32> {
        self.coerce_to_f64().and_then(f64_to_f32)
    }

    pub fn coerce(self, right: UntypedConstant) -> Option<UntypedConstant> {
        match right {
            Integer(_) => self.coerce_to_i64().map(Integer),
            Uinteger(_) => self.coerce_to_u64().map(Uinteger),
            Float(_) => self.coerce_to_f64().map(Float),
        }
    }

    fn const_lt(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(Uinteger(u64::from(match (self, right) {
            (Integer(x), Integer(y)) => x < y,
            (Uinteger(x), Uinteger(y)) => x < y,
            (Float(x), Float(y)) => x < y,
            _ => return None,
        })))
    }

    fn const_le(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(Uinteger(u64::from(match (self, right) {
            (Integer(x), Integer(y)) => x <= y,
            (Uinteger(x), Uinteger(y)) => x <= y,
            (Float(x), Float(y)) => x <= y,
            _ => return None,
        })))
    }

    fn const_gt(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(Uinteger(u64::from(match (self, right) {
            (Integer(x), Integer(y)) => x > y,
            (Uinteger(x), Uinteger(y)) => x > y,
            (Float(x), Float(y)) => x > y,
            _ => return None,
        })))
    }

    fn const_ge(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(Uinteger(u64::from(match (self, right) {
            (Integer(x), Integer(y)) => x >= y,
            (Uinteger(x), Uinteger(y)) => x >= y,
            (Float(x), Float(y)) => x >= y,
            _ => return None,
        })))
    }

    fn const_eq(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(Uinteger(u64::from(match (self, right) {
            (Integer(x), Integer(y)) => x == y,
            (Uinteger(x), Uinteger(y)) => x == y,
            (Float(x), Float(y)) => x == y,
            _ => return None,
        })))
    }

    fn const_neq(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(Uinteger(u64::from(match (self, right) {
            (Integer(x), Integer(y)) => x != y,
            (Uinteger(x), Uinteger(y)) => x != y,
            (Float(x), Float(y)) => x != y,
            _ => return None,
        })))
    }

    fn const_add(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(match (self, right) {
            (Integer(x), Integer(y)) => Integer(x.wrapping_add(y)),
            (Uinteger(x), Uinteger(y)) => Uinteger(x.wrapping_add(y)),
            (Float(x), Float(y)) => Float(x + y),
            _ => return None,
        })
    }

    fn const_sub(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(match (self, right) {
            (Integer(x), Integer(y)) => Integer(x.wrapping_sub(y)),
            (Uinteger(x), Uinteger(y)) => Uinteger(x.wrapping_sub(y)),
            (Float(x), Float(y)) => Float(x - y),
            _ => return None,
        })
    }

    fn const_mul(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(match (self, right) {
            (Integer(x), Integer(y)) => Integer(x.wrapping_mul(y)),
            (Uinteger(x), Uinteger(y)) => Uinteger(x.wrapping_mul(y)),
            (Float(x), Float(y)) => Float(x * y),
            _ => return None,
        })
    }

    fn const_div(self, right: UntypedConstant) -> Option<UntypedConstant> {
        match (self, right) {
            (Integer(x), Integer(y)) => x.checked_div(y).map(Integer),
            (Uinteger(x), Uinteger(y)) => x.checked_div(y).map(Uinteger),
            (Float(x), Float(y)) => Some(Float(x / y)),
            _ => return None,
        }
    }

    fn const_mod(self, right: UntypedConstant) -> Option<UntypedConstant> {
        match (self, right) {
            (Integer(x), Integer(y)) => x.checked_rem(y).map(Integer),
            (Uinteger(x), Uinteger(y)) => x.checked_rem(y).map(Uinteger),
            (Float(x), Float(y)) => Some(Float(x % y)),
            _ => return None,
        }
    }

    fn const_and(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(match (self, right) {
            (Integer(x), Integer(y)) => Integer(x & y),
            (Uinteger(x), Uinteger(y)) => Uinteger(x & y),
            _ => return None,
        })
    }

    fn const_or(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(match (self, right) {
            (Integer(x), Integer(y)) => Integer(x | y),
            (Uinteger(x), Uinteger(y)) => Uinteger(x | y),
            _ => return None,
        })
    }

    fn const_xor(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(match (self, right) {
            (Integer(x), Integer(y)) => Integer(x ^ y),
            (Uinteger(x), Uinteger(y)) => Uinteger(x ^ y),
            _ => return None,
        })
    }

    fn const_shl(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(match (self, right) {
            (Integer(x), Integer(y)) if y > 0 => Integer(x << (y as u64)),
            (Uinteger(x), Uinteger(y)) => Uinteger(x << y),
            _ => return None,
        })
    }

    fn const_shr(self, right: UntypedConstant) -> Option<UntypedConstant> {
        Some(match (self, right) {
            (Integer(x), Integer(y)) if y > 0 => Integer(x >> (y as u64)),
            (Uinteger(x), Uinteger(y)) => Uinteger(x >> y),
            _ => return None,
        })
    }

    pub fn operate(self, right: UntypedConstant, op: BinOp) -> Option<UntypedConstant> {
        if op.0 {
            return None;
        }

        let left = self.coerce(right).unwrap_or(self);
        let right = right.coerce(self).unwrap_or(right);

        match op.1 {
            BinOpVariant::Id => None,
            BinOpVariant::Lt => left.const_lt(right),
            BinOpVariant::Le => left.const_le(right),
            BinOpVariant::Gt => left.const_gt(right),
            BinOpVariant::Ge => left.const_ge(right),
            BinOpVariant::EqC => left.const_eq(right),
            BinOpVariant::Neq => left.const_neq(right),
            BinOpVariant::Add => left.const_add(right),
            BinOpVariant::Sub => left.const_sub(right),
            BinOpVariant::Mul => left.const_mul(right),
            BinOpVariant::Div => left.const_div(right),
            BinOpVariant::Mod => left.const_mod(right),
            BinOpVariant::And => left.const_and(right),
            BinOpVariant::Or => left.const_or(right),
            BinOpVariant::Xor => left.const_xor(right),
            BinOpVariant::Shl => left.const_shl(right),
            BinOpVariant::Shr => left.const_shr(right),
        }
    }

    /// does not typecheck
    pub fn is_true(self) -> bool {
        match self {
            Integer(x) => x != 0,
            Uinteger(x) => x != 0,
            Float(x) => x != 0.0,
        }
    }
}
#[derive(Clone, Copy)]
pub struct Constant {
    meta: Meta,
    constant: UntypedConstant,
}

impl Constant {
    pub fn compile<S: Serializer>(
        self,
        module: &mut S,
        contextual_type: Type,
    ) -> Option<Expression<S>> {
        if self.meta != contextual_type.meta {
            return None;
        }

        if self.meta.len() > 0 {
            // MUST be i32 (when in wasm32)
            return self
                .constant
                .coerce_to_i64()
                .and_then(|x| x.try_into().ok())
                .map(|x| Expression(module.i32_const(x), contextual_type));
        }

        if let ItemRef::StackType(x) = contextual_type.item {
            match x {
                StackType::I32 => self
                    .constant
                    .coerce_to_i64()
                    .and_then(|x| x.try_into().ok())
                    .map(|x| module.i32_const(x)),
                StackType::U32 => self
                    .constant
                    .coerce_to_u64()
                    .and_then(|x| x.try_into().ok())
                    .map(|x| module.i32_const(reinterpret_u32(x))),
                StackType::I64 => self.constant.coerce_to_i64().map(|x| module.i64_const(x)),
                StackType::U64 => self
                    .constant
                    .coerce_to_u64()
                    .map(|x| module.i64_const(reinterpret_u64(x))),
                StackType::F32 => self
                    .constant
                    .coerce_to_f64()
                    .and_then(f64_to_f32)
                    .map(|x| module.f32_const(x)),
                StackType::F64 => self.constant.coerce_to_f64().map(|x| module.f64_const(x)),
            }
            .map(|x| Expression(x, contextual_type))
        } else {
            None
        }
    }

    fn operate_ptr(self, offset: Constant, op: BinOp) -> Option<Constant> {
        // only pointer addition is allowed on pointers
        if let BinOp(_, BinOpVariant::Add) = op {
            self.constant
                .const_add(offset.constant)
                .map(|constant| Constant {
                    meta: self.meta,
                    constant,
                })
        } else {
            None
        }
    }

    pub fn operate(self, right: Constant, op: BinOp) -> Option<Constant> {
        if op.0 {
            return None;
        }

        if self.meta.is_reference() || right.meta.is_reference() {
            return None;
        }

        if self.meta.len() > 0 && right.meta.len() == 0 {
            self.operate_ptr(right, op)
        } else if self.meta.len() == 0 && right.meta.len() > 0 {
            right.operate_ptr(self, op)
        } else {
            self.constant
                .operate(right.constant, op)
                .map(|constant| Constant {
                    meta: self.meta,
                    constant,
                })
        }
    }
}

fn f64_to_f32(r64: f64) -> Option<f32> {
    #[allow(clippy::cast_possible_truncation)]
    let r32 = r64 as f32;

    // TODO: audit
    if (f64::from(r32) - r64).abs() < f64::EPSILON {
        // check that r64 can be represented in 32 bits
        let r32a = r32.abs();
        if f64::from(r32a + 1.0 - r32a) > 1.0 - f64::EPSILON {
            // check that 1 + |r32| is "correct"
            return Some(r32);
        }
    }
    return None;
}

fn check_f64(x: f64) -> Option<f64> {
    let w = x.round();
    if (x - w).abs() < f64::EPSILON {
        Some(w)
    } else {
        None
    }
}

fn reinterpret_u32(n: u32) -> i32 {
    unsafe { std::mem::transmute::<u32, i32>(n) }
}

fn reinterpret_u64(n: u64) -> i64 {
    unsafe { std::mem::transmute::<u64, i64>(n) }
}
