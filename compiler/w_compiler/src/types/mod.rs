use w_codegen::Serializer;

use self::{constant::Constant, expression::Expression, typ::Type};

pub mod constant;
pub mod expression;
pub mod typ;

pub enum Value<S: Serializer> {
    Expression(Expression<S>),
    Constant(Constant),
    Unreachable,
}

impl<S: Serializer> Value<S> {
    fn coerce_to_expr(self, module: &mut S, typ: Type) -> Option<Value<S>> {
        match self {
            x @ Value::Expression(Expression(_, xt)) if xt == typ => Some(x),
            Value::Constant(x) => x.compile(module, typ).map(Value::Expression),
            Value::Unreachable => Some(Value::Unreachable),
            _ => None,
        }
    }

    fn coerce_to_const(self, module: &mut S, y: Constant) -> Option<Value<S>> {
        match self {
            Value::Expression(x @ Expression(_, xt)) => None,
            Value::Constant(x) => x.coerce(y).map(Value::Constant),
            Value::Unreachable => Some(Value::Unreachable),
        }
    }

    pub fn coerce(self, module: &mut S, right: Value<S>) -> Option<Value<S>> {
        match right {
            Value::Expression(Expression(_, typ)) => self.coerce_to_expr(module, typ),
            Value::Constant(y) => self.coerce_to_const(module, y),
            Value::Unreachable => Some(Value::Unreachable),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct IdentPair<'ast> {
    pub mutable: bool,
    pub ident: &'ast str,
    pub t: Option<typ::Type>,
}
