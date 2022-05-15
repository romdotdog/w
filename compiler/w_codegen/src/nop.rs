use crate::{Serializer, WASMType};
pub struct NopSerializer;
impl Serializer for NopSerializer {
    type ExpressionRef = ();

    fn block(&mut self, _label: Option<&str>, _children: &[()], _result_type: Option<WASMType>) {}
    fn if_(&mut self, _condition: (), _if_true: (), _if_false: Option<()>) {}
    fn loop_(&mut self, _label: Option<&str>, _body: ()) {}
    fn call(&mut self, _target: (), _operands: &[()]) {}
    fn local_get(&mut self, _index: &str) {}
    fn local_set(&mut self, _index: &str, _value: ()) {}
    fn local_tee(&mut self, _index: &str, _value: ()) {}
    fn i32_load(&mut self, _offset: i32, _ptr: ()) {}
    fn i32_load8_s(&mut self, _offset: i32, _ptr: ()) {}
    fn i32_load8_u(&mut self, _offset: i32, _ptr: ()) {}
    fn i32_load16_s(&mut self, _offset: i32, _ptr: ()) {}
    fn i32_load16_u(&mut self, _offset: i32, _ptr: ()) {}
    fn i32_const(&mut self, _value: i32) {}
    fn i32_add(&mut self, _left: (), _right: ()) {}
    fn i32_sub(&mut self, _left: (), _right: ()) {}
    fn i32_mul(&mut self, _left: (), _right: ()) {}
    fn i32_div_s(&mut self, _left: (), _right: ()) {}
    fn i32_div_u(&mut self, _left: (), _right: ()) {}
    fn i32_rem_s(&mut self, _left: (), _right: ()) {}
    fn i32_rem_u(&mut self, _left: (), _right: ()) {}
    fn i32_and(&mut self, _left: (), _right: ()) {}
    fn i32_or(&mut self, _left: (), _right: ()) {}
    fn i32_xor(&mut self, _left: (), _right: ()) {}
    fn i32_shl(&mut self, _left: (), _right: ()) {}
    fn i32_shr_s(&mut self, _left: (), _right: ()) {}
    fn i32_shr_u(&mut self, _left: (), _right: ()) {}
    fn i32_eq(&mut self, _left: (), _right: ()) {}
    fn i32_ne(&mut self, _left: (), _right: ()) {}
    fn i32_lt_s(&mut self, _left: (), _right: ()) {}
    fn i32_lt_u(&mut self, _left: (), _right: ()) {}
    fn i32_le_s(&mut self, _left: (), _right: ()) {}
    fn i32_le_u(&mut self, _left: (), _right: ()) {}
    fn i32_gt_s(&mut self, _left: (), _right: ()) {}
    fn i32_gt_u(&mut self, _left: (), _right: ()) {}
    fn i32_ge_s(&mut self, _left: (), _right: ()) {}
    fn i32_ge_u(&mut self, _left: (), _right: ()) {}
    fn i64_load(&mut self, _offset: i32, _ptr: ()) {}
    fn i64_const(&mut self, _value: i64) {}
    fn i64_add(&mut self, _left: (), _right: ()) {}
    fn i64_sub(&mut self, _left: (), _right: ()) {}
    fn i64_mul(&mut self, _left: (), _right: ()) {}
    fn i64_div_s(&mut self, _left: (), _right: ()) {}
    fn i64_div_u(&mut self, _left: (), _right: ()) {}
    fn i64_rem_s(&mut self, _left: (), _right: ()) {}
    fn i64_rem_u(&mut self, _left: (), _right: ()) {}
    fn i64_and(&mut self, _left: (), _right: ()) {}
    fn i64_or(&mut self, _left: (), _right: ()) {}
    fn i64_xor(&mut self, _left: (), _right: ()) {}
    fn i64_shl(&mut self, _left: (), _right: ()) {}
    fn i64_shr_s(&mut self, _left: (), _right: ()) {}
    fn i64_shr_u(&mut self, _left: (), _right: ()) {}
    fn i64_eq(&mut self, _left: (), _right: ()) {}
    fn i64_ne(&mut self, _left: (), _right: ()) {}
    fn i64_lt_s(&mut self, _left: (), _right: ()) {}
    fn i64_lt_u(&mut self, _left: (), _right: ()) {}
    fn i64_le_s(&mut self, _left: (), _right: ()) {}
    fn i64_le_u(&mut self, _left: (), _right: ()) {}
    fn i64_gt_s(&mut self, _left: (), _right: ()) {}
    fn i64_gt_u(&mut self, _left: (), _right: ()) {}
    fn i64_ge_s(&mut self, _left: (), _right: ()) {}
    fn i64_ge_u(&mut self, _left: (), _right: ()) {}
    fn f32_load(&mut self, _offset: i32, _ptr: ()) {}
    fn f32_const(&mut self, _value: f32) {}
    fn f32_add(&mut self, _left: (), _right: ()) {}
    fn f32_sub(&mut self, _left: (), _right: ()) {}
    fn f32_mul(&mut self, _left: (), _right: ()) {}
    fn f32_div(&mut self, _left: (), _right: ()) {}
    fn f32_eq(&mut self, _left: (), _right: ()) {}
    fn f32_ne(&mut self, _left: (), _right: ()) {}
    fn f32_lt(&mut self, _left: (), _right: ()) {}
    fn f32_le(&mut self, _left: (), _right: ()) {}
    fn f32_gt(&mut self, _left: (), _right: ()) {}
    fn f32_ge(&mut self, _left: (), _right: ()) {}
    fn f64_load(&mut self, _offset: i32, _ptr: ()) {}
    fn f64_const(&mut self, _value: f64) {}
    fn f64_add(&mut self, _left: (), _right: ()) {}
    fn f64_sub(&mut self, _left: (), _right: ()) {}
    fn f64_mul(&mut self, _left: (), _right: ()) {}
    fn f64_div(&mut self, _left: (), _right: ()) {}
    fn f64_eq(&mut self, _left: (), _right: ()) {}
    fn f64_ne(&mut self, _left: (), _right: ()) {}
    fn f64_lt(&mut self, _left: (), _right: ()) {}
    fn f64_le(&mut self, _left: (), _right: ()) {}
    fn f64_gt(&mut self, _left: (), _right: ()) {}
    fn f64_ge(&mut self, _left: (), _right: ()) {}
    fn return_(&mut self, _value: Option<()>) {}
    fn unreachable(&mut self) {}
    fn add_function(
        &mut self,
        _name: &str,
        _params: Vec<(&str, WASMType)>,
        _results: Vec<WASMType>,
        _vars: Vec<(&str, WASMType)>,
        _body: (),
    ) {
    }
}
