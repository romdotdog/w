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
    // result moves self back
    fn coerce_to_expr(self, module: &mut S, typ: Type) -> Result<Value<S>, Value<S>> {
        match self {
            Value::Expression(Expression(_, xt)) if xt == typ => Ok(self),
            Value::Expression(_) => Err(self),
            Value::Constant(x) => {
                let comp = x.compile(module, Some(typ));
                if comp.1 == typ {
                    Ok(Value::Expression(comp))
                } else {
                    Err(self)
                }
            }
        }
    }

    // result moves self back
    fn coerce_to_const(self, module: &mut S, y: Constant) -> Result<Value<S>, Value<S>> {
        match self {
            Value::Expression(_) => todo!(),
            Value::Constant(x) => x.coerce(y).map(Value::Constant).ok_or(self),
        }
    }

    // result moves self back
    fn coerce_inner(self, module: &mut S, right: &Value<S>) -> Result<Value<S>, Value<S>> {
        match right {
            Value::Expression(Expression(_, typ)) => self.coerce_to_expr(module, *typ),
            Value::Constant(y) => self.coerce_to_const(module, *y),
        }
    }

    pub fn coerce(self, module: &mut S, right: Value<S>) -> Option<(Value<S>, Value<S>)> {
        match self.coerce_inner(module, &right) {
            Ok(x) => Some((x, right)),
            Err(another_self) => right
                .coerce_inner(module, &another_self)
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
