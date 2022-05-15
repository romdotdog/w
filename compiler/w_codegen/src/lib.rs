use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum WASMType {
    I32,
    I64,
    F32,
    F64,
}

impl Display for WASMType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WASMType::I32 => write!(f, "i32"),
            WASMType::I64 => write!(f, "i64"),
            WASMType::F32 => write!(f, "f32"),
            WASMType::F64 => write!(f, "f64"),
        }
    }
}

#[rustfmt::skip]
pub trait Serializer {
    type ExpressionRef;

    fn block(&mut self, label: Option<&str>, children: &[Self::ExpressionRef], result_type: Option<WASMType>) -> Self::ExpressionRef;
    fn if_(
        &mut self,
        condition: Self::ExpressionRef,
        if_true: Self::ExpressionRef,
        if_false: Option<Self::ExpressionRef>,
    ) -> Self::ExpressionRef;
    fn loop_(&mut self, label: Option<&str>, body: Self::ExpressionRef) -> Self::ExpressionRef;
    fn call(&mut self, target: Self::ExpressionRef, operands: &[Self::ExpressionRef]) -> Self::ExpressionRef;

    fn local_get(&mut self, index: &str) -> Self::ExpressionRef;
    fn local_set(&mut self, index: &str, value: Self::ExpressionRef) -> Self::ExpressionRef;
    fn local_tee(&mut self, index: &str, value: Self::ExpressionRef) -> Self::ExpressionRef;

    fn i32_load(&mut self, offset: i32, ptr: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_load8_s(&mut self, offset: i32, ptr: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_load8_u(&mut self, offset: i32, ptr: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_load16_s(&mut self, offset: i32, ptr: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_load16_u(&mut self, offset: i32, ptr: Self::ExpressionRef) -> Self::ExpressionRef;

    fn i32_const(&mut self, value: i32) -> Self::ExpressionRef;
    fn i32_add(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_sub(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_mul(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_div_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_div_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_rem_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_rem_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_and(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_or(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_xor(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_shl(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_shr_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_shr_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    
    
    fn i32_eq(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_ne(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_lt_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_lt_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_le_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_le_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_gt_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_gt_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_ge_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i32_ge_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    

    fn i64_load(&mut self, offset: i32, ptr: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_const(&mut self, value: i64) -> Self::ExpressionRef;
    fn i64_add(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_sub(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_mul(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_div_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_div_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_rem_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_rem_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_and(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_or(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_xor(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_shl(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_shr_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_shr_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    
    fn i64_eq(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_ne(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_lt_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_lt_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_le_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_le_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_gt_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_gt_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_ge_s(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn i64_ge_u(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;

    fn f32_load(&mut self, offset: i32, ptr: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_const(&mut self, value: f32) -> Self::ExpressionRef;
    fn f32_add(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_sub(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_mul(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_div(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;

    fn f32_eq(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_ne(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_lt(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_le(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_gt(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f32_ge(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;

    fn f64_load(&mut self, offset: i32, ptr: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_const(&mut self, value: f64) -> Self::ExpressionRef;
    fn f64_add(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_sub(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_mul(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_div(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;

    fn f64_eq(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_ne(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_lt(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_le(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_gt(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;
    fn f64_ge(&mut self, left: Self::ExpressionRef, right: Self::ExpressionRef) -> Self::ExpressionRef;


    fn return_(&mut self, value: Option<Self::ExpressionRef>);
    fn unreachable(&mut self) -> Self::ExpressionRef;
    fn add_function(
        &mut self,
        name: &str,
        params: Vec<(&str, WASMType)>,
        results: Vec<WASMType>,
        vars: Vec<(&str, WASMType)>,
        body: Self::ExpressionRef,
    );
}
