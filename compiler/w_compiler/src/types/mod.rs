use w_codegen::Serializer;

use self::{constant::Constant, expression::Expression, typ::*};

pub mod constant;
pub mod expression;
pub mod itemref;
pub mod meta;
pub mod typ;

pub enum Value<S: Serializer> {
    Expression(Expression<S>),
    Constant(Constant),
}

fn unreachable_expr<S: Serializer>(module: &mut S) -> Expression<S> {
    Expression(module.unreachable(), UNREACHABLE)
}

fn unreachable<S: Serializer>(module: &mut S) -> Value<S> {
    Value::Expression(unreachable_expr(module))
}

impl<S: Serializer> Value<S> {
    fn coerce_to_expr(self, module: &mut S, typ: Type) -> Option<Value<S>> {
        match self {
            x @ Value::Expression(Expression(_, xt)) if xt == UNREACHABLE || xt == typ => Some(x),
            Value::Constant(x) => x.compile(module, typ).map(Value::Expression),
            _ => None,
        }
    }

    fn coerce_to_const(self, module: &mut S, y: Constant) -> Option<Value<S>> {
        match self {
            Value::Expression(Expression(_, xt)) if xt == UNREACHABLE => Some(unreachable(module)),
            Value::Expression(x @ Expression(_, xt)) => None,
            Value::Constant(x) => x.coerce(y).map(Value::Constant),
        }
    }

    fn coerce_inner(self, module: &mut S, right: Value<S>) -> Option<Value<S>> {
        match right {
            Value::Expression(Expression(_, typ)) if typ == UNREACHABLE => {
                Some(unreachable(module))
            }
            Value::Expression(Expression(_, typ)) => self.coerce_to_expr(module, typ),
            Value::Constant(y) => self.coerce_to_const(module, y),
            _ => None,
        }
    }

    pub fn coerce(self, module: &mut S, right: Value<S>) -> Option<(Value<S>, Value<S>)> {
        self.coerce_inner(module, right)
            .map(|x| (x, right))
            .or_else(|| right.coerce_inner(module, self).map(|x| (self, x)))
    }

    // TODO: refactor to match Constant::compile?
    pub fn compile(self, module: &mut S, contextual_type: Option<Type>) -> Option<Expression<S>> {
        match self {
            Value::Expression(x) => Some(x),
            Value::Constant(x) => contextual_type.and_then(|typ| x.compile(module, typ)),
        }
    }
}

#[derive(Clone)]
pub struct IdentPair<'ast> {
    pub ident: &'ast str,
    pub typ: typ::Type,
}
