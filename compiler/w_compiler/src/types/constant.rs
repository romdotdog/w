use std::convert::TryInto;

use w_codegen::Serializer;
use w_lexer::token::{BinOp, BinOpVariant};

use super::{
    expression::Expression,
    typ::{Type, F32, F64, I32, I64, U32, U64},
};

// arbitrary bounds
const MIN_FLOAT: i64 = -(2i64.pow(53));
const MAX_FLOAT: i64 = 2i64.pow(53);
const UMAX_FLOAT: u64 = 2u64.pow(53);

#[derive(Clone, Copy)]
pub enum Constant {
    Integer(i64),
    Uinteger(u64),
    Float(f64),
}

fn check_f64(x: f64) -> Option<f64> {
    let w = x.round();
    if (x - w).abs() < f64::EPSILON {
        Some(w)
    } else {
        None
    }
}

impl Constant {
    fn coerce_to_i64(self) -> Option<i64> {
        match self {
            Constant::Integer(x) => Some(x),
            Constant::Uinteger(x) => x.try_into().ok(),
            Constant::Float(x) => check_f64(x).map(|w| w as i64),
        }
    }

    fn coerce_to_u64(self) -> Option<u64> {
        match self {
            Constant::Integer(x) => x.try_into().ok(),
            Constant::Uinteger(x) => Some(x),
            Constant::Float(x) if x >= 0.0 => check_f64(x).map(|w| w as u64),
            _ => None,
        }
    }

    fn coerce_to_f64(self) -> Option<f64> {
        match self {
            Constant::Integer(x) if x > MIN_FLOAT && x < MAX_FLOAT => Some(x as f64),
            Constant::Uinteger(x) if x < UMAX_FLOAT => Some(x as f64),
            Constant::Float(x) => Some(x),
            _ => None,
        }
    }

    pub fn coerce(self, right: Constant) -> Option<Constant> {
        match right {
            Constant::Integer(_) => self.coerce_to_i64().map(Constant::Integer),
            Constant::Uinteger(_) => self.coerce_to_u64().map(Constant::Uinteger),
            Constant::Float(_) => self.coerce_to_f64().map(Constant::Float),
        }
    }

    fn const_lt(self, right: Constant) -> Option<Constant> {
        Some(Constant::Uinteger(u64::from(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => x < y,
            (Constant::Uinteger(x), Constant::Uinteger(y)) => x < y,
            (Constant::Float(x), Constant::Float(y)) => x < y,
            _ => return None,
        })))
    }

    fn const_le(self, right: Constant) -> Option<Constant> {
        Some(Constant::Uinteger(u64::from(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => x <= y,
            (Constant::Uinteger(x), Constant::Uinteger(y)) => x <= y,
            (Constant::Float(x), Constant::Float(y)) => x <= y,
            _ => return None,
        })))
    }

    fn const_gt(self, right: Constant) -> Option<Constant> {
        Some(Constant::Uinteger(u64::from(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => x > y,
            (Constant::Uinteger(x), Constant::Uinteger(y)) => x > y,
            (Constant::Float(x), Constant::Float(y)) => x > y,
            _ => return None,
        })))
    }

    fn const_ge(self, right: Constant) -> Option<Constant> {
        Some(Constant::Uinteger(u64::from(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => x >= y,
            (Constant::Uinteger(x), Constant::Uinteger(y)) => x >= y,
            (Constant::Float(x), Constant::Float(y)) => x >= y,
            _ => return None,
        })))
    }

    fn const_eq(self, right: Constant) -> Option<Constant> {
        Some(Constant::Uinteger(u64::from(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => x == y,
            (Constant::Uinteger(x), Constant::Uinteger(y)) => x == y,
            (Constant::Float(x), Constant::Float(y)) => x == y,
            _ => return None,
        })))
    }

    fn const_neq(self, right: Constant) -> Option<Constant> {
        Some(Constant::Uinteger(u64::from(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => x != y,
            (Constant::Uinteger(x), Constant::Uinteger(y)) => x != y,
            (Constant::Float(x), Constant::Float(y)) => x != y,
            _ => return None,
        })))
    }

    fn const_add(self, right: Constant) -> Option<Constant> {
        Some(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => Constant::Integer(x.wrapping_add(y)),
            (Constant::Uinteger(x), Constant::Uinteger(y)) => Constant::Uinteger(x.wrapping_add(y)),
            (Constant::Float(x), Constant::Float(y)) => Constant::Float(x + y),
            _ => return None,
        })
    }

    fn const_sub(self, right: Constant) -> Option<Constant> {
        Some(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => Constant::Integer(x.wrapping_sub(y)),
            (Constant::Uinteger(x), Constant::Uinteger(y)) => Constant::Uinteger(x.wrapping_sub(y)),
            (Constant::Float(x), Constant::Float(y)) => Constant::Float(x - y),
            _ => return None,
        })
    }

    fn const_mul(self, right: Constant) -> Option<Constant> {
        Some(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => Constant::Integer(x.wrapping_mul(y)),
            (Constant::Uinteger(x), Constant::Uinteger(y)) => Constant::Uinteger(x.wrapping_mul(y)),
            (Constant::Float(x), Constant::Float(y)) => Constant::Float(x * y),
            _ => return None,
        })
    }

    fn const_div(self, right: Constant) -> Option<Constant> {
        match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => x.checked_div(y).map(Constant::Integer),
            (Constant::Uinteger(x), Constant::Uinteger(y)) => {
                x.checked_div(y).map(Constant::Uinteger)
            }
            (Constant::Float(x), Constant::Float(y)) => Some(Constant::Float(x / y)),
            _ => return None,
        }
    }

    fn const_mod(self, right: Constant) -> Option<Constant> {
        match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => x.checked_rem(y).map(Constant::Integer),
            (Constant::Uinteger(x), Constant::Uinteger(y)) => {
                x.checked_rem(y).map(Constant::Uinteger)
            }
            (Constant::Float(x), Constant::Float(y)) => Some(Constant::Float(x % y)),
            _ => return None,
        }
    }

    fn const_and(self, right: Constant) -> Option<Constant> {
        Some(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => Constant::Integer(x & y),
            (Constant::Uinteger(x), Constant::Uinteger(y)) => Constant::Uinteger(x & y),
            _ => return None,
        })
    }

    fn const_or(self, right: Constant) -> Option<Constant> {
        Some(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => Constant::Integer(x | y),
            (Constant::Uinteger(x), Constant::Uinteger(y)) => Constant::Uinteger(x | y),
            _ => return None,
        })
    }

    fn const_xor(self, right: Constant) -> Option<Constant> {
        Some(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) => Constant::Integer(x ^ y),
            (Constant::Uinteger(x), Constant::Uinteger(y)) => Constant::Uinteger(x ^ y),
            _ => return None,
        })
    }

    fn const_shl(self, right: Constant) -> Option<Constant> {
        Some(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) if y > 0 => {
                Constant::Integer(x << (y as u64))
            }
            (Constant::Uinteger(x), Constant::Uinteger(y)) => Constant::Uinteger(x << y),
            _ => return None,
        })
    }

    fn const_shr(self, right: Constant) -> Option<Constant> {
        Some(match (self, right) {
            (Constant::Integer(x), Constant::Integer(y)) if y > 0 => {
                Constant::Integer(x >> (y as u64))
            }
            (Constant::Uinteger(x), Constant::Uinteger(y)) => Constant::Uinteger(x >> y),
            _ => return None,
        })
    }

    pub fn operate(self, right: Constant, op: BinOp) -> Option<Constant> {
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

    pub fn compile<S: Serializer>(
        self,
        module: &mut S,
        contextual_type: Type,
    ) -> Option<Expression<S>> {
        if contextual_type.is_reference() || !contextual_type.is_number() {
            return None;
        }

        if contextual_type.is_high() {
            if contextual_type.is_float() {
                self.coerce_to_f64()
                    .map(|x| Expression(module.f64_const(x), F64))
            } else if contextual_type.is_signed() {
                self.coerce_to_i64()
                    .map(|x| Expression(module.i64_const(x), I64))
            } else {
                self.coerce_to_u64()
                    .map(|x| Expression(module.i64_const(reinterpret_u64(x)), U64))
            }
        } else if contextual_type.is_float() {
            self.coerce_to_f64().map(|x| match f64_to_f32(x) {
                Some(x) => Expression(module.f32_const(x), F32),
                None => Expression(module.f64_const(x), F64),
            })
        } else if contextual_type.is_signed() {
            self.coerce_to_i64().map(|x| match x.try_into() {
                Ok(x) => Expression(module.i32_const(x), I32),
                _ => Expression(module.i64_const(x), I64),
            })
        } else {
            self.coerce_to_u64().map(|x| match x.try_into() {
                Ok(x) => Expression(module.i32_const(reinterpret_u32(x)), U32),
                _ => Expression(module.i64_const(reinterpret_u64(x)), U64),
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

fn reinterpret_u32(n: u32) -> i32 {
    unsafe { std::mem::transmute::<u32, i32>(n) }
}

fn reinterpret_u64(n: u64) -> i64 {
    unsafe { std::mem::transmute::<u64, i64>(n) }
}
