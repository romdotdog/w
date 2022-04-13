pub enum WASMType {
    I32,
    I64,
    F32,
    F64,
}

pub trait Serializer {
    type ExpressionRef;

    fn if_(
        &mut self,
        condition: Self::ExpressionRef,
        if_true: Self::ExpressionRef,
        if_false: Option<Self::ExpressionRef>,
    ) -> Self::ExpressionRef;
    fn i32_const(&mut self, value: i32) -> Self::ExpressionRef;

    fn i64_const(&mut self, value: i64) -> Self::ExpressionRef;

    fn f32_const(&mut self, value: f32) -> Self::ExpressionRef;

    fn f64_const(&mut self, value: f64) -> Self::ExpressionRef;

    fn return_(&mut self, value: Option<Self::ExpressionRef>);
    fn unreachable(&mut self) -> Self::ExpressionRef;
	fn add_function(&mut self, name: &str, params: Vec<(&str, WASMType)>, results: Vec<(&str, WASMType)>, vars: Vec<(&str, WASMType)>, body: Self::ExpressionRef);
}
