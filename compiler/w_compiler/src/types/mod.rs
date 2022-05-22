use w_codegen::Serializer;

use self::{constant::Constant, expression::Expression, meta::Meta, typ::Type};

pub mod constant;
pub mod expression;
pub mod itemref;
pub mod meta;
pub mod typ;

pub enum Value<S: Serializer> {
    Expression(Expression<S>),
    Constant(Constant),
}

impl<S: Serializer> Value<S> {
    // result moves self back
    fn coerce_inner(self, module: &mut S, to: Type) -> Result<Value<S>, Value<S>> {
        match self {
            Value::Expression(Expression(_, xt)) if xt == to => Ok(self),
            Value::Expression(_) => Err(self),
            Value::Constant(x) => x.coerce(to).map(Value::Constant).ok_or(self),
        }
    }

    pub fn to_type(&self) -> Type {
        match self {
            Value::Expression(Expression(_, t)) => *t,
            Value::Constant(c) => c.to_type(),
        }
    }

    pub fn set_meta(&mut self, meta: Meta) {
        match self {
            Value::Expression(Expression(_, t)) => {
                t.meta = meta;
            }
            Value::Constant(c) => {
                c.meta = meta;
            }
        }
    }

    pub fn coerce(self, module: &mut S, right: Value<S>) -> Option<(Value<S>, Value<S>)> {
        match self.coerce_inner(module, right.to_type()) {
            Ok(x) => Some((x, right)),
            Err(another_self) => right
                .coerce_inner(module, another_self.to_type())
                .map(|x| (another_self, x))
                .ok(),
        }
    }

    // TODO: refactor to match Constant::compile?
    pub fn compile(self, module: &mut S, contextual_type: Option<Type>) -> Expression<S> {
        match self {
            Value::Expression(x) => x,
            Value::Constant(x) => x.compile(module, contextual_type),
        }
    }
}

#[derive(Clone)]
pub struct IdentPair<'ast> {
    pub ident: &'ast str,
    pub typ: typ::Type,
}
