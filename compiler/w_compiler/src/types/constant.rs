use std::convert::TryInto;

use w_codegen::Serializer;
use w_lexer::token::{BinOp, BinOpVariant};

use super::{
    expression::Expression,
    itemref::{ItemRef, StackType},
    meta::{Meta, VALUE},
    typ,
};

// arbitrary bounds
const MIN_FLOAT: i64 = -(2i64.pow(53));
const MAX_FLOAT: i64 = 2i64.pow(53);
const UMAX_FLOAT: u64 = 2u64.pow(53);

#[derive(Clone, Copy)]
pub enum UntypedConstant {
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
}

use UntypedConstant::*;
impl UntypedConstant {
    pub fn coerce_to_i64(self) -> Option<i64> {
        match self {
            I32(x) => Some(x as i64),
            U32(x) => Some(x as i64),
            F32(x) => check_f32(x).map(|w| w as i64),

            I64(x) => Some(x),
            U64(x) => x.try_into().ok(),

            #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
            F64(x) => check_f64(x).map(|w| w as i64),
        }
    }

    pub fn coerce_to_i32(self) -> Option<i32> {
        self.coerce_to_i64().and_then(|x| x.try_into().ok())
    }

    pub fn coerce_to_u64(self) -> Option<u64> {
        match self {
            I32(x) => x.try_into().ok(),
            U32(x) => Some(x as u64),
            F32(x) if x >= 0.0 => check_f32(x).map(|w| w as u64),

            I64(x) => x.try_into().ok(),
            U64(x) => Some(x),

            #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
            F64(x) if x >= 0.0 => check_f64(x).map(|w| w as u64),
            F64(_) | F32(_) => None,
        }
    }

    pub fn coerce_to_u32(self) -> Option<u32> {
        self.coerce_to_u64().and_then(|x| x.try_into().ok())
    }

    pub fn coerce_to_f64(self) -> Option<f64> {
        #[allow(clippy::cast_precision_loss)]
        match self {
            I64(x) if x > MIN_FLOAT && x < MAX_FLOAT => Some(x as f64),
            U64(x) if x < UMAX_FLOAT => Some(x as f64),
            F64(x) => Some(x),
            _ => None,
        }
    }

    pub fn coerce_to_f32(self) -> Option<f32> {
        self.coerce_to_f64().and_then(f64_to_f32)
    }

    pub fn binop_i64(x: i64, y: i64, op: BinOpVariant) -> UntypedConstant {
        match op {
            BinOpVariant::Id => I64(y),
            BinOpVariant::Lt => U64(u64::from(x < y)),
            BinOpVariant::Le => U64(u64::from(x <= y)),
            BinOpVariant::Gt => U64(u64::from(x > y)),
            BinOpVariant::Ge => U64(u64::from(x >= y)),
            BinOpVariant::EqC => U64(u64::from(x == y)),
            BinOpVariant::Neq => U64(u64::from(x != y)),
            BinOpVariant::Add => I64(x.wrapping_add(y)),
            BinOpVariant::Sub => I64(x.wrapping_sub(y)),
            BinOpVariant::Mul => I64(x.wrapping_mul(y)),
            BinOpVariant::Div => I64(x.wrapping_div(y)),
            BinOpVariant::Mod => I64(x.wrapping_rem(y)),
            BinOpVariant::And => I64(x & y),
            BinOpVariant::Or => I64(x | y),
            BinOpVariant::Xor => I64(x ^ y),
            BinOpVariant::Shl => I64(x << y),
            BinOpVariant::Shr => I64(x >> y),
        }
    }

    pub fn binop_i32(x: i32, y: i32, op: BinOpVariant) -> UntypedConstant {
        match op {
            BinOpVariant::Id => I32(y),
            BinOpVariant::Lt => U64(u64::from(x < y)),
            BinOpVariant::Le => U64(u64::from(x <= y)),
            BinOpVariant::Gt => U64(u64::from(x > y)),
            BinOpVariant::Ge => U64(u64::from(x >= y)),
            BinOpVariant::EqC => U64(u64::from(x == y)),
            BinOpVariant::Neq => U64(u64::from(x != y)),
            BinOpVariant::Add => I32(x.wrapping_add(y)),
            BinOpVariant::Sub => I32(x.wrapping_sub(y)),
            BinOpVariant::Mul => I32(x.wrapping_mul(y)),
            BinOpVariant::Div => I32(x.wrapping_div(y)),
            BinOpVariant::Mod => I32(x.wrapping_rem(y)),
            BinOpVariant::And => I32(x & y),
            BinOpVariant::Or => I32(x | y),
            BinOpVariant::Xor => I32(x ^ y),
            BinOpVariant::Shl => I32(x << y),
            BinOpVariant::Shr => I32(x >> y),
        }
    }

    pub fn binop_u64(x: u64, y: u64, op: BinOpVariant) -> UntypedConstant {
        match op {
            BinOpVariant::Id => U64(y),
            BinOpVariant::Lt => U64(u64::from(x < y)),
            BinOpVariant::Le => U64(u64::from(x <= y)),
            BinOpVariant::Gt => U64(u64::from(x > y)),
            BinOpVariant::Ge => U64(u64::from(x >= y)),
            BinOpVariant::EqC => U64(u64::from(x == y)),
            BinOpVariant::Neq => U64(u64::from(x != y)),
            BinOpVariant::Add => U64(x.wrapping_add(y)),
            BinOpVariant::Sub => U64(x.wrapping_sub(y)),
            BinOpVariant::Mul => U64(x.wrapping_mul(y)),
            BinOpVariant::Div => U64(x.wrapping_div(y)),
            BinOpVariant::Mod => U64(x.wrapping_rem(y)),
            BinOpVariant::And => U64(x & y),
            BinOpVariant::Or => U64(x | y),
            BinOpVariant::Xor => U64(x ^ y),
            BinOpVariant::Shl => U64(x << y),
            BinOpVariant::Shr => U64(x >> y),
        }
    }

    pub fn binop_u32(x: u32, y: u32, op: BinOpVariant) -> UntypedConstant {
        match op {
            BinOpVariant::Id => U32(y),
            BinOpVariant::Lt => U64(u64::from(x < y)),
            BinOpVariant::Le => U64(u64::from(x <= y)),
            BinOpVariant::Gt => U64(u64::from(x > y)),
            BinOpVariant::Ge => U64(u64::from(x >= y)),
            BinOpVariant::EqC => U64(u64::from(x == y)),
            BinOpVariant::Neq => U64(u64::from(x != y)),
            BinOpVariant::Add => U32(x.wrapping_add(y)),
            BinOpVariant::Sub => U32(x.wrapping_sub(y)),
            BinOpVariant::Mul => U32(x.wrapping_mul(y)),
            BinOpVariant::Div => U32(x.wrapping_div(y)),
            BinOpVariant::Mod => U32(x.wrapping_rem(y)),
            BinOpVariant::And => U32(x & y),
            BinOpVariant::Or => U32(x | y),
            BinOpVariant::Xor => U32(x ^ y),
            BinOpVariant::Shl => U32(x << y),
            BinOpVariant::Shr => U32(x >> y),
        }
    }

    pub fn binop_f64(x: f64, y: f64, op: BinOpVariant) -> Option<UntypedConstant> {
        #[allow(clippy::float_cmp)]
        Some(match op {
            BinOpVariant::Id => F64(y),
            BinOpVariant::Lt => U64(u64::from(x < y)),
            BinOpVariant::Le => U64(u64::from(x <= y)),
            BinOpVariant::Gt => U64(u64::from(x > y)),
            BinOpVariant::Ge => U64(u64::from(x >= y)),
            BinOpVariant::EqC => U64(u64::from(x == y)),
            BinOpVariant::Neq => U64(u64::from(x != y)),
            BinOpVariant::Add => F64(x + y),
            BinOpVariant::Sub => F64(x - y),
            BinOpVariant::Mul => F64(x * y),
            BinOpVariant::Div => F64(x / y),
            _ => return None,
        })
    }

    pub fn binop_f32(x: f32, y: f32, op: BinOpVariant) -> Option<UntypedConstant> {
        #[allow(clippy::float_cmp)]
        Some(match op {
            BinOpVariant::Id => F32(y),
            BinOpVariant::Lt => U64(u64::from(x < y)),
            BinOpVariant::Le => U64(u64::from(x <= y)),
            BinOpVariant::Gt => U64(u64::from(x > y)),
            BinOpVariant::Ge => U64(u64::from(x >= y)),
            BinOpVariant::EqC => U64(u64::from(x == y)),
            BinOpVariant::Neq => U64(u64::from(x != y)),
            BinOpVariant::Add => F32(x + y),
            BinOpVariant::Sub => F32(x - y),
            BinOpVariant::Mul => F32(x * y),
            BinOpVariant::Div => F32(x / y),
            _ => return None,
        })
    }

    pub fn binop(self, right: UntypedConstant, op: BinOp) -> Option<UntypedConstant> {
        if op.0 {
            return None;
        }

        match (self, right) {
            (I32(x), I32(y)) => Some(Self::binop_i32(x, y, op.1)),
            (U32(x), U32(y)) => Some(Self::binop_u32(x, y, op.1)),
            (F32(x), F32(y)) => Self::binop_f32(x, y, op.1),
            (I64(x), I64(y)) => Some(Self::binop_i64(x, y, op.1)),
            (U64(x), U64(y)) => Some(Self::binop_u64(x, y, op.1)),
            (F64(x), F64(y)) => Self::binop_f64(x, y, op.1),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Constant {
    pub meta: Meta,
    pub constant: UntypedConstant,
}

impl Constant {
    pub fn i64(x: i64) -> Constant {
        Constant {
            meta: VALUE,
            constant: I64(x),
        }
    }

    pub fn u64(x: u64) -> Constant {
        Constant {
            meta: VALUE,
            constant: U64(x),
        }
    }

    pub fn f64(x: f64) -> Constant {
        Constant {
            meta: VALUE,
            constant: F64(x),
        }
    }

    pub fn to_type(self) -> typ::Type {
        typ::Type {
            meta: self.meta,
            item: ItemRef::StackType(match self.constant {
                I32(_) => StackType::I32,
                I64(_) => StackType::I64,
                U32(_) => StackType::U32,
                U64(_) => StackType::U64,
                F32(_) => StackType::F32,
                F64(_) => StackType::F64,
            }),
        }
    }

    pub fn coerce(self, to: typ::Type) -> Option<Constant> {
        if self.meta != to.meta || self.meta.len() > 0 || self.meta.is_reference() {
            return None;
        }

        if let ItemRef::StackType(x) = to.item {
            match x {
                StackType::I32 => self.constant.coerce_to_i32().map(I32),
                StackType::U32 => self.constant.coerce_to_u32().map(U32),
                StackType::I64 => self.constant.coerce_to_i64().map(I64),
                StackType::U64 => self.constant.coerce_to_u64().map(U64),
                StackType::F32 => self.constant.coerce_to_f32().map(F32),
                StackType::F64 => self.constant.coerce_to_f64().map(F64),
            }
            .map(|x| Constant {
                meta: self.meta,
                constant: x,
            })
        } else {
            None
        }
    }

    fn compile_to_type<S: Serializer>(
        self,
        module: &mut S,
        contextual_type: typ::Type,
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
        contextual_type: Option<typ::Type>,
    ) -> Expression<S> {
        contextual_type
            .and_then(|contextual_type| self.compile_to_type(module, contextual_type))
            .unwrap_or_else(|| {
                if self.meta.len() > 0 {
                    todo!();
                }

                match self.constant {
                    U32(x) => Expression(module.i32_const(reinterpret_u32(x)), typ::U32),
                    I32(x) => Expression(module.i32_const(x), typ::I32),
                    F32(x) => Expression(module.f32_const(x), typ::F32),
                    I64(x) => Expression(module.i64_const(x), typ::I64),
                    U64(x) => Expression(module.i64_const(reinterpret_u64(x)), typ::U64),
                    F64(x) => Expression(module.f64_const(x), typ::F64),
                }
            })
    }

    fn binop_ptr(self, offset: Constant, op: BinOp) -> Option<Constant> {
        // only pointer addition is allowed on pointers
        if let BinOp(_, BinOpVariant::Add) = op {
            self.constant
                .binop(offset.constant, op) // TODO: improve?
                .map(|constant| Constant {
                    meta: self.meta,
                    constant,
                })
        } else {
            None
        }
    }

    pub fn binop(self, right: Constant, op: BinOp) -> Option<Constant> {
        if op.0 {
            return None;
        }

        if self.meta.is_reference() || right.meta.is_reference() {
            return None;
        }

        if self.meta.len() > 0 && right.meta.len() == 0 {
            self.binop_ptr(right, op)
        } else if self.meta.len() == 0 && right.meta.len() > 0 {
            right.binop_ptr(self, op)
        } else {
            let left = self.coerce(right.to_type()).unwrap_or(self);
            let right = right.coerce(self.to_type()).unwrap_or(right);

            left.constant
                .binop(right.constant, op)
                .map(|constant| Constant {
                    meta: left.meta,
                    constant,
                })
        }
    }

    /// does not typecheck
    pub fn is_true(self) -> Option<bool> {
        if self.meta.len() > 0 || self.meta.is_reference() {
            None
        } else {
            Some(match self.constant {
                I32(x) => x != 0,
                U32(x) => x != 0,
                F32(x) => x != 0.0,
                I64(x) => x != 0,
                U64(x) => x != 0,
                F64(x) => x != 0.0,
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

fn check_f32(x: f32) -> Option<f32> {
    let w = x.round();
    if (x - w).abs() < f32::EPSILON {
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
