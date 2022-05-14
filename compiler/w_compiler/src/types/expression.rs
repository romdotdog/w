use w_codegen::Serializer;
use w_lexer::token::{BinOp, BinOpVariant};

use super::typ::{Type, F32, F64, I32, I64, U32, U64};

pub struct Expression<S: Serializer>(pub S::ExpressionRef, pub Type);

impl<S: Serializer> Expression<S> {
    pub fn operate(self, module: &mut S, right: Expression<S>, op: BinOp) -> Option<Expression<S>> {
        if self.1.is_reference()
            || right.1.is_reference()
            || !(self.1.is_number() && right.1.is_number())
            || self.1.is_signed() != right.1.is_signed()
            || self.1.is_float() != right.1.is_float()
            || self.1.is_high() != right.1.is_high()
        {
            return None;
        }

        Some(if self.1.is_high() {
            if self.1.is_float() {
                match op.1 {
                    BinOpVariant::Id => todo!(),
                    BinOpVariant::Lt => Expression(module.f64_lt(self.0, right.0), U32),
                    BinOpVariant::Le => Expression(module.f64_le(self.0, right.0), U32),
                    BinOpVariant::Gt => Expression(module.f64_gt(self.0, right.0), U32),
                    BinOpVariant::Ge => Expression(module.f64_ge(self.0, right.0), U32),
                    BinOpVariant::EqC => Expression(module.f64_eq(self.0, right.0), U32),
                    BinOpVariant::Neq => Expression(module.f64_ne(self.0, right.0), U32),
                    BinOpVariant::Add => Expression(module.f64_add(self.0, right.0), F64),
                    BinOpVariant::Sub => Expression(module.f64_sub(self.0, right.0), F64),
                    BinOpVariant::Mul => Expression(module.f64_mul(self.0, right.0), F64),
                    BinOpVariant::Div => Expression(module.f64_div(self.0, right.0), F64),
                    _ => return None,
                }
            } else if self.1.is_signed() {
                match op.1 {
                    BinOpVariant::Id => todo!(),
                    BinOpVariant::Lt => Expression(module.i64_lt_s(self.0, right.0), U32),
                    BinOpVariant::Le => Expression(module.i64_le_s(self.0, right.0), U32),
                    BinOpVariant::Gt => Expression(module.i64_gt_s(self.0, right.0), U32),
                    BinOpVariant::Ge => Expression(module.i64_ge_s(self.0, right.0), U32),
                    BinOpVariant::EqC => Expression(module.i64_eq(self.0, right.0), U32),
                    BinOpVariant::Neq => Expression(module.i64_ne(self.0, right.0), U32),
                    BinOpVariant::Add => Expression(module.i64_add(self.0, right.0), I64),
                    BinOpVariant::Sub => Expression(module.i64_sub(self.0, right.0), I64),
                    BinOpVariant::Mul => Expression(module.i64_mul(self.0, right.0), I64),
                    BinOpVariant::Div => Expression(module.i64_div_s(self.0, right.0), I64),
                    BinOpVariant::Mod => Expression(module.i64_rem_s(self.0, right.0), I64),
                    BinOpVariant::And => Expression(module.i64_and(self.0, right.0), I64),
                    BinOpVariant::Or => Expression(module.i64_or(self.0, right.0), I64),
                    BinOpVariant::Xor => Expression(module.i64_xor(self.0, right.0), I64),
                    BinOpVariant::Shl => Expression(module.i64_shl(self.0, right.0), I64),
                    BinOpVariant::Shr => Expression(module.i64_shr_s(self.0, right.0), I64),
                }
            } else {
                match op.1 {
                    BinOpVariant::Id => todo!(),
                    BinOpVariant::Lt => Expression(module.i64_lt_u(self.0, right.0), U32),
                    BinOpVariant::Le => Expression(module.i64_le_u(self.0, right.0), U32),
                    BinOpVariant::Gt => Expression(module.i64_gt_u(self.0, right.0), U32),
                    BinOpVariant::Ge => Expression(module.i64_ge_u(self.0, right.0), U32),
                    BinOpVariant::EqC => Expression(module.i64_eq(self.0, right.0), U32),
                    BinOpVariant::Neq => Expression(module.i64_ne(self.0, right.0), U32),
                    BinOpVariant::Add => Expression(module.i64_add(self.0, right.0), U64),
                    BinOpVariant::Sub => Expression(module.i64_sub(self.0, right.0), U64),
                    BinOpVariant::Mul => Expression(module.i64_mul(self.0, right.0), U64),
                    BinOpVariant::Div => Expression(module.i64_div_u(self.0, right.0), U64),
                    BinOpVariant::Mod => Expression(module.i64_rem_u(self.0, right.0), U64),
                    BinOpVariant::And => Expression(module.i64_and(self.0, right.0), U64),
                    BinOpVariant::Or => Expression(module.i64_or(self.0, right.0), U64),
                    BinOpVariant::Xor => Expression(module.i64_xor(self.0, right.0), U64),
                    BinOpVariant::Shl => Expression(module.i64_shl(self.0, right.0), U64),
                    BinOpVariant::Shr => Expression(module.i64_shr_u(self.0, right.0), U64),
                }
            }
        } else if self.1.is_float() {
            match op.1 {
                BinOpVariant::Id => todo!(),
                BinOpVariant::Lt => Expression(module.f32_lt(self.0, right.0), U32),
                BinOpVariant::Le => Expression(module.f32_le(self.0, right.0), U32),
                BinOpVariant::Gt => Expression(module.f32_gt(self.0, right.0), U32),
                BinOpVariant::Ge => Expression(module.f32_ge(self.0, right.0), U32),
                BinOpVariant::EqC => Expression(module.f32_eq(self.0, right.0), U32),
                BinOpVariant::Neq => Expression(module.f32_ne(self.0, right.0), U32),
                BinOpVariant::Add => Expression(module.f32_add(self.0, right.0), F32),
                BinOpVariant::Sub => Expression(module.f32_sub(self.0, right.0), F32),
                BinOpVariant::Mul => Expression(module.f32_mul(self.0, right.0), F32),
                BinOpVariant::Div => Expression(module.f32_div(self.0, right.0), F32),
                _ => return None,
            }
        } else if self.1.is_signed() {
            match op.1 {
                BinOpVariant::Id => todo!(),
                BinOpVariant::Lt => Expression(module.i32_lt_s(self.0, right.0), U32),
                BinOpVariant::Le => Expression(module.i32_le_s(self.0, right.0), U32),
                BinOpVariant::Gt => Expression(module.i32_gt_s(self.0, right.0), U32),
                BinOpVariant::Ge => Expression(module.i32_ge_s(self.0, right.0), U32),
                BinOpVariant::EqC => Expression(module.i32_eq(self.0, right.0), U32),
                BinOpVariant::Neq => Expression(module.i32_ne(self.0, right.0), U32),
                BinOpVariant::Add => Expression(module.i32_add(self.0, right.0), I32),
                BinOpVariant::Sub => Expression(module.i32_sub(self.0, right.0), I32),
                BinOpVariant::Mul => Expression(module.i32_mul(self.0, right.0), I32),
                BinOpVariant::Div => Expression(module.i32_div_s(self.0, right.0), I32),
                BinOpVariant::Mod => Expression(module.i32_rem_s(self.0, right.0), I32),
                BinOpVariant::And => Expression(module.i32_and(self.0, right.0), I32),
                BinOpVariant::Or => Expression(module.i32_or(self.0, right.0), I32),
                BinOpVariant::Xor => Expression(module.i32_xor(self.0, right.0), I32),
                BinOpVariant::Shl => Expression(module.i32_shl(self.0, right.0), I32),
                BinOpVariant::Shr => Expression(module.i32_shr_s(self.0, right.0), I32),
            }
        } else {
            match op.1 {
                BinOpVariant::Id => todo!(),
                BinOpVariant::Lt => Expression(module.i32_lt_u(self.0, right.0), U32),
                BinOpVariant::Le => Expression(module.i32_le_u(self.0, right.0), U32),
                BinOpVariant::Gt => Expression(module.i32_gt_u(self.0, right.0), U32),
                BinOpVariant::Ge => Expression(module.i32_ge_u(self.0, right.0), U32),
                BinOpVariant::EqC => Expression(module.i32_eq(self.0, right.0), U32),
                BinOpVariant::Neq => Expression(module.i32_ne(self.0, right.0), U32),
                BinOpVariant::Add => Expression(module.i32_add(self.0, right.0), U32),
                BinOpVariant::Sub => Expression(module.i32_sub(self.0, right.0), U32),
                BinOpVariant::Mul => Expression(module.i32_mul(self.0, right.0), U32),
                BinOpVariant::Div => Expression(module.i32_div_u(self.0, right.0), U32),
                BinOpVariant::Mod => Expression(module.i32_rem_u(self.0, right.0), U32),
                BinOpVariant::And => Expression(module.i32_and(self.0, right.0), U32),
                BinOpVariant::Or => Expression(module.i32_or(self.0, right.0), U32),
                BinOpVariant::Xor => Expression(module.i32_xor(self.0, right.0), U32),
                BinOpVariant::Shl => Expression(module.i32_shl(self.0, right.0), U32),
                BinOpVariant::Shr => Expression(module.i32_shr_u(self.0, right.0), U32),
            }
        })
    }
}
