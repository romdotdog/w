use std::convert::TryInto;

use w_codegen::Serializer;
use w_lexer::token::{BinOp, BinOpVariant};

use super::{
    expression::Expression,
    itemref::{ItemRef, StackType},
    meta::{Meta, VALUE},
    typ::{Type, F64, I64, U64},
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

use UntypedConstant::{Float, Integer, Uinteger};
impl UntypedConstant {
    pub fn coerce_to_i64(self) -> Option<i64> {
        match self {
            Integer(x) => Some(x),
            Uinteger(x) => x.try_into().ok(),

            #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
            Float(x) => check_f64(x).map(|w| w as i64),
        }
    }

    pub fn coerce_to_i32(self) -> Option<i32> {
        self.coerce_to_i64().and_then(|x| x.try_into().ok())
    }

    pub fn coerce_to_u64(self) -> Option<u64> {
        match self {
            Integer(x) => x.try_into().ok(),
            Uinteger(x) => Some(x),

            #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
            Float(x) if x >= 0.0 => check_f64(x).map(|w| w as u64),
            Float(_) => None,
        }
    }

    pub fn coerce_to_u32(self) -> Option<u32> {
        self.coerce_to_u64().and_then(|x| x.try_into().ok())
    }

    pub fn coerce_to_f64(self) -> Option<f64> {
        #[allow(clippy::cast_precision_loss)]
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

    pub fn operate_int(x: i64, y: i64, op: BinOpVariant) -> UntypedConstant {
        match op {
            BinOpVariant::Id => Integer(y),
            BinOpVariant::Lt => Uinteger(u64::from(x < y)),
            BinOpVariant::Le => Uinteger(u64::from(x <= y)),
            BinOpVariant::Gt => Uinteger(u64::from(x > y)),
            BinOpVariant::Ge => Uinteger(u64::from(x >= y)),
            BinOpVariant::EqC => Uinteger(u64::from(x == y)),
            BinOpVariant::Neq => Uinteger(u64::from(x != y)),
            BinOpVariant::Add => Integer(x.wrapping_add(y)),
            BinOpVariant::Sub => Integer(x.wrapping_sub(y)),
            BinOpVariant::Mul => Integer(x.wrapping_mul(y)),
            BinOpVariant::Div => Integer(x.wrapping_div(y)),
            BinOpVariant::Mod => Integer(x.wrapping_rem(y)),
            BinOpVariant::And => Integer(x & y),
            BinOpVariant::Or => Integer(x | y),
            BinOpVariant::Xor => Integer(x ^ y),
            BinOpVariant::Shl => Integer(x << y),
            BinOpVariant::Shr => Integer(x >> y),
        }
    }

    pub fn operate_uint(x: u64, y: u64, op: BinOpVariant) -> UntypedConstant {
        match op {
            BinOpVariant::Id => Uinteger(y),
            BinOpVariant::Lt => Uinteger(u64::from(x < y)),
            BinOpVariant::Le => Uinteger(u64::from(x <= y)),
            BinOpVariant::Gt => Uinteger(u64::from(x > y)),
            BinOpVariant::Ge => Uinteger(u64::from(x >= y)),
            BinOpVariant::EqC => Uinteger(u64::from(x == y)),
            BinOpVariant::Neq => Uinteger(u64::from(x != y)),
            BinOpVariant::Add => Uinteger(x.wrapping_add(y)),
            BinOpVariant::Sub => Uinteger(x.wrapping_sub(y)),
            BinOpVariant::Mul => Uinteger(x.wrapping_mul(y)),
            BinOpVariant::Div => Uinteger(x.wrapping_div(y)),
            BinOpVariant::Mod => Uinteger(x.wrapping_rem(y)),
            BinOpVariant::And => Uinteger(x & y),
            BinOpVariant::Or => Uinteger(x | y),
            BinOpVariant::Xor => Uinteger(x ^ y),
            BinOpVariant::Shl => Uinteger(x << y),
            BinOpVariant::Shr => Uinteger(x >> y),
        }
    }

    pub fn operate_float(x: f64, y: f64, op: BinOpVariant) -> Option<UntypedConstant> {
        #[allow(clippy::float_cmp)]
        Some(match op {
            BinOpVariant::Id => Float(y),
            BinOpVariant::Lt => Uinteger(u64::from(x < y)),
            BinOpVariant::Le => Uinteger(u64::from(x <= y)),
            BinOpVariant::Gt => Uinteger(u64::from(x > y)),
            BinOpVariant::Ge => Uinteger(u64::from(x >= y)),
            BinOpVariant::EqC => Uinteger(u64::from(x == y)),
            BinOpVariant::Neq => Uinteger(u64::from(x != y)),
            BinOpVariant::Add => Float(x + y),
            BinOpVariant::Sub => Float(x - y),
            BinOpVariant::Mul => Float(x * y),
            BinOpVariant::Div => Float(x / y),
            _ => return None,
        })
    }

    pub fn operate(self, right: UntypedConstant, op: BinOp) -> Option<UntypedConstant> {
        if op.0 {
            return None;
        }

        let left = self.coerce(right).unwrap_or(self);
        let right = right.coerce(self).unwrap_or(right);

        match (left, right) {
            (Integer(x), Integer(y)) => Some(Self::operate_int(x, y, op.1)),
            (Uinteger(x), Uinteger(y)) => Some(Self::operate_uint(x, y, op.1)),
            (Float(x), Float(y)) => Self::operate_float(x, y, op.1),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Constant {
    meta: Meta,
    constant: UntypedConstant,
}

impl Constant {
    pub fn i64(x: i64) -> Constant {
        Constant {
            meta: VALUE,
            constant: Integer(x),
        }
    }

    pub fn u64(x: u64) -> Constant {
        Constant {
            meta: VALUE,
            constant: Uinteger(x),
        }
    }

    pub fn f64(x: f64) -> Constant {
        Constant {
            meta: VALUE,
            constant: Float(x),
        }
    }

    fn compile_to_type<S: Serializer>(
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
                StackType::I32 => self.constant.coerce_to_i32().map(|x| module.i32_const(x)),
                StackType::U32 => self
                    .constant
                    .coerce_to_u32()
                    .map(|x| module.i32_const(reinterpret_u32(x))),
                StackType::I64 => self.constant.coerce_to_i64().map(|x| module.i64_const(x)),
                StackType::U64 => self
                    .constant
                    .coerce_to_u64()
                    .map(|x| module.i64_const(reinterpret_u64(x))),
                StackType::F32 => self.constant.coerce_to_f32().map(|x| module.f32_const(x)),
                StackType::F64 => self.constant.coerce_to_f64().map(|x| module.f64_const(x)),
            }
            .map(|x| Expression(x, contextual_type))
        } else {
            None
        }
    }

    pub fn compile<S: Serializer>(
        self,
        module: &mut S,
        contextual_type: Option<Type>,
    ) -> Expression<S> {
        contextual_type
            .and_then(|contextual_type| self.compile_to_type(module, contextual_type))
            .unwrap_or_else(|| {
                if self.meta.len() > 0 {
                    todo!();
                }

                match self.constant {
                    Integer(x) => Expression(module.i64_const(x), I64),
                    Uinteger(x) => Expression(module.i64_const(reinterpret_u64(x)), U64),
                    Float(x) => Expression(module.f64_const(x), F64),
                }
            })
    }

    fn operate_ptr(self, offset: Constant, op: BinOp) -> Option<Constant> {
        // only pointer addition is allowed on pointers
        if let BinOp(_, BinOpVariant::Add) = op {
            self.constant
                .operate(offset.constant, op) // TODO: improve?
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

    pub fn coerce(self, right: Constant) -> Option<Constant> {
        if self.meta != right.meta || self.meta.len() > 0 || self.meta.is_reference() {
            None
        } else {
            match right.constant {
                Integer(_) => self.constant.coerce_to_i64().map(|x| Constant {
                    meta: right.meta,
                    constant: Integer(x),
                }),
                Uinteger(_) => self.constant.coerce_to_u64().map(|x| Constant {
                    meta: right.meta,
                    constant: Uinteger(x),
                }),
                Float(_) => self.constant.coerce_to_f64().map(|x| Constant {
                    meta: right.meta,
                    constant: Float(x),
                }),
            }
        }
    }

    /// does not typecheck
    pub fn is_true(self) -> Option<bool> {
        if self.meta.len() > 0 || self.meta.is_reference() {
            None
        } else {
            Some(match self.constant {
                Integer(x) => x != 0,
                Uinteger(x) => x != 0,
                Float(x) => x != 0.0,
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
    None
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
