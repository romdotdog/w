use w_codegen::Serializer;
use w_lexer::token::{BinOp, BinOpVariant};

use super::{
    itemref::{ItemRef, StackType},
    typ::{Type, F32, F64, I32, I64, U32, U64},
};

pub struct Expression<S: Serializer>(pub S::ExpressionRef, pub Type);

impl<S: Serializer> Expression<S> {
    fn operate_ptr(
        self,
        module: &mut S,
        offset: Expression<S>,
        op: BinOp,
    ) -> Option<Expression<S>> {
        // only pointer addition is allowed on pointers
        if let BinOp(_, BinOpVariant::Add) = op {
            if offset.1 == I32 {
                Some(Expression(module.i32_add(self.0, offset.0), self.1))
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn operate(self, module: &mut S, right: Expression<S>, op: BinOp) -> Option<Expression<S>> {
        let typ = self.1;
        let right_typ = right.1;
        if typ.meta.is_reference() || right_typ.meta.is_reference() {
            return None;
        }

        if typ.meta.len() > 0 && right_typ.meta.len() == 0 {
            return self.operate_ptr(module, right, op);
        } else if typ.meta.len() == 0 && right_typ.meta.len() > 0 {
            return right.operate_ptr(module, self, op);
        }

        if typ != right.1 {
            return None;
        }

        match typ.item {
            ItemRef::Void => unreachable!(),
            ItemRef::Unreachable => Some(self),
            ItemRef::HeapType(_) => None,
            ItemRef::ItemRef(_) => None,
            ItemRef::StackType(x) => match x {
                StackType::I32 => self.operate_i32(op, module, right),
                StackType::U32 => self.operate_u32(op, module, right),
                StackType::I64 => self.operate_i64(op, module, right),
                StackType::U64 => self.operate_u64(op, module, right),
                StackType::F32 => self.operate_f32(op, module, right),
                StackType::F64 => self.operate_f64(op, module, right),
            },
        }
    }

    fn operate_f64(self, op: BinOp, module: &mut S, right: Expression<S>) -> Option<Expression<S>> {
        Some(match op.1 {
            BinOpVariant::Id => right,
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
        })
    }

    fn operate_f32(self, op: BinOp, module: &mut S, right: Expression<S>) -> Option<Expression<S>> {
        Some(match op.1 {
            BinOpVariant::Id => right,
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
        })
    }

    fn operate_u64(self, op: BinOp, module: &mut S, right: Expression<S>) -> Option<Expression<S>> {
        Some(match op.1 {
            BinOpVariant::Id => right,
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
        })
    }

    fn operate_i64(self, op: BinOp, module: &mut S, right: Expression<S>) -> Option<Expression<S>> {
        Some(match op.1 {
            BinOpVariant::Id => right,
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
        })
    }

    fn operate_u32(self, op: BinOp, module: &mut S, right: Expression<S>) -> Option<Expression<S>> {
        Some(match op.1 {
            BinOpVariant::Id => right,
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
        })
    }

    fn operate_i32(self, op: BinOp, module: &mut S, right: Expression<S>) -> Option<Expression<S>> {
        Some(match op.1 {
            BinOpVariant::Id => right,
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
        })
    }
}
