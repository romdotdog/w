use w_codegen::Serializer;
use w_lexer::token::{BinOp, BinOpVariant};

use super::{
    itemref::{ItemRef, StackType},
    typ::{Type, F32, F64, I32, I64, U32, U64},
};

pub struct Expression<S: Serializer>(pub S::ExpressionRef, pub Type);

impl<S: Serializer> Expression<S> {
    pub fn cast(self, module: &mut S, to: Type) -> Option<Expression<S>> {
        let typ = self.1;
        if typ.meta.is_reference() != to.meta.is_reference() {
            return None;
        }

        if typ.meta.len() > 0 || to.meta.len() > 0 {
            return None;
        }

        match to.item {
            ItemRef::Void => todo!(),
            ItemRef::Unreachable => todo!(),
            ItemRef::HeapType(_) => todo!(),
            ItemRef::StackType(x) => match x {
                StackType::I32 => Some(self.cast_to_i32(module)),
                StackType::U32 => Some(self.cast_to_u32(module)),
                StackType::I64 => Some(self.cast_to_i64(module)),
                StackType::U64 => Some(self.cast_to_u64(module)),
                StackType::F32 => Some(self.cast_to_f32(module)),
                StackType::F64 => Some(self.cast_to_f64(module)),
            },
            ItemRef::Ref(_) => todo!(),
        }
    }

    fn cast_to_i32(self, module: &mut S) -> Expression<S> {
        match self.1.item {
            ItemRef::Void => todo!(),
            ItemRef::Unreachable => todo!(),
            ItemRef::HeapType(_) => todo!(),
            ItemRef::StackType(x) => match x {
                StackType::I32 => self,
                StackType::U32 => Expression(self.0, I32),
                StackType::I64 | StackType::U64 => Expression(module.i32_wrap(self.0), I32),
                StackType::F32 => Expression(module.i32_trunc_sat_f32_s(self.0), I32),
                StackType::F64 => Expression(module.i32_trunc_sat_f64_s(self.0), I32),
            },
            ItemRef::Ref(_) => todo!(),
        }
    }

    fn cast_to_u32(self, module: &mut S) -> Expression<S> {
        match self.1.item {
            ItemRef::Void => todo!(),
            ItemRef::Unreachable => todo!(),
            ItemRef::HeapType(_) => todo!(),
            ItemRef::StackType(x) => match x {
                StackType::I32 => Expression(self.0, U32),
                StackType::U32 => self,
                StackType::I64 | StackType::U64 => Expression(module.i32_wrap(self.0), U32),
                StackType::F32 => Expression(module.i32_trunc_sat_f32_u(self.0), U32),
                StackType::F64 => Expression(module.i32_trunc_sat_f64_u(self.0), U32),
            },
            ItemRef::Ref(_) => todo!(),
        }
    }

    fn cast_to_i64(self, module: &mut S) -> Expression<S> {
        match self.1.item {
            ItemRef::Void => todo!(),
            ItemRef::Unreachable => todo!(),
            ItemRef::HeapType(_) => todo!(),
            ItemRef::StackType(x) => match x {
                StackType::I32 => Expression(module.i64_extend_s(self.0), I64),
                StackType::U32 => Expression(module.i64_extend_u(self.0), I64),
                StackType::I64 => self,
                StackType::U64 => Expression(self.0, I64),
                StackType::F32 => Expression(module.i64_trunc_sat_f32_s(self.0), I64),
                StackType::F64 => Expression(module.i64_trunc_sat_f64_s(self.0), I64),
            },
            ItemRef::Ref(_) => todo!(),
        }
    }

    fn cast_to_u64(self, module: &mut S) -> Expression<S> {
        match self.1.item {
            ItemRef::Void => todo!(),
            ItemRef::Unreachable => todo!(),
            ItemRef::HeapType(_) => todo!(),
            ItemRef::StackType(x) => match x {
                StackType::I32 => Expression(module.i64_extend_s(self.0), U64),
                StackType::U32 => Expression(module.i64_extend_u(self.0), U64),
                StackType::I64 => Expression(self.0, U64),
                StackType::U64 => self,
                StackType::F32 => Expression(module.i64_trunc_sat_f32_u(self.0), U64),
                StackType::F64 => Expression(module.i64_trunc_sat_f64_u(self.0), U64),
            },
            ItemRef::Ref(_) => todo!(),
        }
    }

    fn cast_to_f32(self, module: &mut S) -> Expression<S> {
        match self.1.item {
            ItemRef::Void => todo!(),
            ItemRef::Unreachable => todo!(),
            ItemRef::HeapType(_) => todo!(),
            ItemRef::StackType(x) => match x {
                StackType::I32 => Expression(module.f32_convert_i32_s(self.0), F32),
                StackType::U32 => Expression(module.f32_convert_i32_u(self.0), F32),
                StackType::I64 => Expression(module.f32_convert_i64_s(self.0), F32),
                StackType::U64 => Expression(module.f32_convert_i64_u(self.0), F32),
                StackType::F32 => self,
                StackType::F64 => Expression(module.f32_demote(self.0), F32),
            },
            ItemRef::Ref(_) => todo!(),
        }
    }

    fn cast_to_f64(self, module: &mut S) -> Expression<S> {
        match self.1.item {
            ItemRef::Void => todo!(),
            ItemRef::Unreachable => todo!(),
            ItemRef::HeapType(_) => todo!(),
            ItemRef::StackType(x) => match x {
                StackType::I32 => Expression(module.f64_convert_i32_s(self.0), F64),
                StackType::U32 => Expression(module.f64_convert_i32_u(self.0), F64),
                StackType::I64 => Expression(module.f64_convert_i64_s(self.0), F64),
                StackType::U64 => Expression(module.f64_convert_i64_u(self.0), F64),
                StackType::F32 => Expression(module.f64_promote(self.0), F64),
                StackType::F64 => self,
            },
            ItemRef::Ref(_) => todo!(),
        }
    }

    pub fn binop(self, module: &mut S, right: Expression<S>, op: BinOp) -> Option<Expression<S>> {
        let typ = self.1;
        let right_typ = right.1;
        if typ.meta.is_reference() || right_typ.meta.is_reference() {
            return None;
        }

        if typ.meta.len() > 0 && right_typ.meta.len() == 0 {
            return self.binop_ptr(module, right, op);
        } else if typ.meta.len() == 0 && right_typ.meta.len() > 0 {
            return right.binop_ptr(module, self, op);
        }

        if typ != right.1 {
            return None;
        }

        match typ.item {
            ItemRef::Void => unreachable!(),
            ItemRef::Unreachable => Some(self),
            ItemRef::HeapType(_) | ItemRef::Ref(_) => None,
            ItemRef::StackType(x) => match x {
                StackType::I32 => Some(self.binop_i32(op, module, right)),
                StackType::U32 => Some(self.binop_u32(op, module, right)),
                StackType::I64 => Some(self.binop_i64(op, module, right)),
                StackType::U64 => Some(self.binop_u64(op, module, right)),
                StackType::F32 => self.binop_f32(op, module, right),
                StackType::F64 => self.binop_f64(op, module, right),
            },
        }
    }

    fn binop_ptr(self, module: &mut S, offset: Expression<S>, op: BinOp) -> Option<Expression<S>> {
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

    fn binop_f64(self, op: BinOp, module: &mut S, right: Expression<S>) -> Option<Expression<S>> {
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

    fn binop_f32(self, op: BinOp, module: &mut S, right: Expression<S>) -> Option<Expression<S>> {
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

    fn binop_u64(self, op: BinOp, module: &mut S, right: Expression<S>) -> Expression<S> {
        match op.1 {
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
        }
    }

    fn binop_i64(self, op: BinOp, module: &mut S, right: Expression<S>) -> Expression<S> {
        match op.1 {
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
        }
    }

    fn binop_u32(self, op: BinOp, module: &mut S, right: Expression<S>) -> Expression<S> {
        match op.1 {
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
        }
    }

    fn binop_i32(self, op: BinOp, module: &mut S, right: Expression<S>) -> Expression<S> {
        match op.1 {
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
        }
    }
}
