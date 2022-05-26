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

    fn cast_to_i32(self) -> i32 {
        match self {
            I32(x) => x as i32,
            I64(x) => x as i32,
            U32(x) => x as i32,
            U64(x) => x as i32,
            F32(x) => x as i32,
            F64(x) => x as i32,
        }
    }

    fn cast_to_i64(self) -> i64 {
        match self {
            I32(x) => x as i64,
            I64(x) => x as i64,
            U32(x) => x as i64,
            U64(x) => x as i64,
            F32(x) => x as i64,
            F64(x) => x as i64,
        }
    }

    fn cast_to_u32(self) -> u32 {
        match self {
            I32(x) => x as u32,
            I64(x) => x as u32,
            U32(x) => x as u32,
            U64(x) => x as u32,
            F32(x) => x as u32,
            F64(x) => x as u32,
        }
    }

    fn cast_to_u64(self) -> u64 {
        match self {
            I32(x) => x as u64,
            I64(x) => x as u64,
            U32(x) => x as u64,
            U64(x) => x as u64,
            F32(x) => x as u64,
            F64(x) => x as u64,
        }
    }

    fn cast_to_f32(self) -> f32 {
        match self {
            I32(x) => x as f32,
            I64(x) => x as f32,
            U32(x) => x as f32,
            U64(x) => x as f32,
            F32(x) => x as f32,
            F64(x) => x as f32,
        }
    }

    fn cast_to_f64(self) -> f64 {
        match self {
            I32(x) => x as f64,
            I64(x) => x as f64,
            U32(x) => x as f64,
            U64(x) => x as f64,
            F32(x) => x as f64,
            F64(x) => x as f64,
        }
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
    pub typ: typ::Type,
    pub constant: UntypedConstant,
}

impl Constant {
    pub fn i64(x: i64) -> Constant {
        Constant {
            typ: typ::I64,
            constant: I64(x),
        }
    }

    pub fn u64(x: u64) -> Constant {
        Constant {
            typ: typ::U64,
            constant: U64(x),
        }
    }

    pub fn f64(x: f64) -> Constant {
        Constant {
            typ: typ::F64,
            constant: F64(x),
        }
    }

    pub fn i32(x: i32) -> Constant {
        Constant {
            typ: typ::I32,
            constant: I32(x),
        }
    }

    pub fn u32(x: u32) -> Constant {
        Constant {
            typ: typ::U32,
            constant: U32(x),
        }
    }

    pub fn f32(x: f32) -> Constant {
        Constant {
            typ: typ::F32,
            constant: F32(x),
        }
    }

    pub fn coerce(self, to: typ::Type) -> Option<Constant> {
        if self.typ.meta != to.meta || self.typ.meta.len() > 0 || self.typ.meta.is_reference() {
            return None;
        }

        if let ItemRef::StackType(x) = to.item {
            match x {
                StackType::I32 => self.constant.coerce_to_i32().map(Self::i32),
                StackType::U32 => self.constant.coerce_to_u32().map(Self::u32),
                StackType::I64 => self.constant.coerce_to_i64().map(Self::i64),
                StackType::U64 => self.constant.coerce_to_u64().map(Self::u64),
                StackType::F32 => self.constant.coerce_to_f32().map(Self::f32),
                StackType::F64 => self.constant.coerce_to_f64().map(Self::f64),
            }
        } else {
            None
        }
    }

    pub fn cast(self, to: typ::Type) -> Option<Constant> {
        if self.typ.meta.is_reference() != to.meta.is_reference() {
            return None;
        }

        if self.typ.meta.len() > 0 || to.meta.len() > 0 {
            return None;
        }

        match to.item {
            ItemRef::Void => todo!(),
            ItemRef::Unreachable => todo!(),
            ItemRef::HeapType(_) => todo!(),
            ItemRef::StackType(x) => Some(match x {
                StackType::I32 => Self::i32(self.constant.cast_to_i32()),
                StackType::U32 => Self::u32(self.constant.cast_to_u32()),
                StackType::I64 => Self::i64(self.constant.cast_to_i64()),
                StackType::U64 => Self::u64(self.constant.cast_to_u64()),
                StackType::F32 => Self::f32(self.constant.cast_to_f32()),
                StackType::F64 => Self::f64(self.constant.cast_to_f64()),
            }),
            ItemRef::Ref(_) => todo!(),
        }
    }

    // does not typecheck
    pub fn compile_to_type<S: Serializer>(
        self,
        module: &mut S,
        contextual_type: typ::Type,
    ) -> Expression<S> {
        self.coerce(contextual_type).unwrap_or(self).compile(module)
    }

    pub fn compile<S: Serializer>(self, module: &mut S) -> Expression<S> {
        Expression(
            match self.constant {
                U32(x) => module.i32_const(reinterpret_u32(x)),
                I32(x) => module.i32_const(x),
                F32(x) => module.f32_const(x),
                I64(x) => module.i64_const(x),
                U64(x) => module.i64_const(reinterpret_u64(x)),
                F64(x) => module.f64_const(x),
            },
            self.typ,
        )
    }

    fn binop_ptr(self, offset: Constant, op: BinOp) -> Option<Constant> {
        // only pointer addition is allowed on pointers
        if let BinOp(_, BinOpVariant::Add) = op {
            self.constant
                .binop(offset.constant, op) // TODO: improve?
                .map(|constant| Constant {
                    typ: self.typ,
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

        if self.typ.meta.is_reference() || right.typ.meta.is_reference() {
            return None;
        }

        if self.typ.meta.len() > 0 && right.typ.meta.len() == 0 {
            self.binop_ptr(right, op)
        } else if self.typ.meta.len() == 0 && right.typ.meta.len() > 0 {
            right.binop_ptr(self, op)
        } else {
            let left = self.coerce(right.typ).unwrap_or(self);
            let right = right.coerce(self.typ).unwrap_or(right);

            left.constant
                .binop(right.constant, op)
                .map(|constant| Constant {
                    typ: left.typ,
                    constant,
                })
        }
    }

    /// does not typecheck
    pub fn is_true(self) -> Option<bool> {
        if self.typ.meta.len() > 0 || self.typ.meta.is_reference() {
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
