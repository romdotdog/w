
use std::convert::TryInto;

use w_codegen::Serializer;
use w_errors::Message;
use w_lexer::token::{AmbiguousOp, BinOp, BinOpVariant, Token};
use w_lexer::Lexer;
use types::{IdentPair, ReferenceKind, indir::Indir, Type, TypeVariant, Constant};

pub mod handler;
mod primaryatom;
mod simpleatom;
mod toplevel;
mod types;
mod symbol_stack;

use handler::{Handler, Status, ImportlessHandler, ImportlessHandlerHandler};
use symbol_stack::SymbolStack;


pub struct Compiler<'ast, H: Handler<'ast>, S: Serializer> {
    session: &'ast H, // TODO: lifetime review
    src_ref: &'ast H::SourceRef,
	
    lex: Lexer<'ast>,
    start: usize,
    end: usize,
    tk: Option<Token<'ast>>,

	//

	module: &'ast mut S,
	symbols: SymbolStack<'ast>
}

pub enum Value<S: Serializer> {
    Expression(Expression<S>),
    Constant(Constant),
    Unreachable
}

pub struct Expression<S: Serializer>(S::ExpressionRef, Type);


enum Take<'ast, T> {
    Next(T),
    Fill(T, Option<Token<'ast>>),
    // self.next() already called
    // dangerous if used improperly
    NoFill(T),
}

use Take::{Fill, Next, NoFill};
use w_utils::span::Span;

impl<'ast, H: ImportlessHandler<'ast>, S: Serializer> Compiler<'ast, ImportlessHandlerHandler<'ast, H>, S> {
    pub fn compile_string(session: &'ast ImportlessHandlerHandler<'ast, H>, module: &'ast mut S, src: &'ast str) -> Self {
        let lex = Lexer::from_str(src);

        let mut compiler = Compiler {
            session,
            src_ref: &(),
            
			lex,
            start: 0,
            end: 0,
            tk: None,

			module,
			symbols: SymbolStack::default()
        };

        compiler.next();
        compiler
    }
}

impl<'ast, H: Handler<'ast>, S: Serializer> Compiler<'ast, H, S> {
    pub fn compile(session: &'ast H, module: &'ast mut S, src_ref: &'ast H::SourceRef) {
        let src = session.get_source(src_ref);
        let mut lex = Lexer::from_str(src);

        // includes
        for (path, start, end) in lex.process_includes() {
            if let Some((new_src_ref, status)) = session.load_source(src_ref, path) {
                match status {
                    Status::NotParsing => {
                        Compiler::compile(session, module, new_src_ref);
                    }
                    Status::CurrentlyParsing => {
                        session.error(
                            src_ref,
                            Message::RecursiveInclude,
                            Span::new(start, end),
                        );
                    }
                    Status::AlreadyParsed => {}
                }
            } else {
                session.error(
                    src_ref,
                    Message::FileNotFound,
                    Span::new(start, end),
                );
            }
        }

        let mut compiler = Compiler {
            session,
            src_ref,
			
            lex,
            start: 0,
            end: 0,
            tk: None,

			module,
			symbols: SymbolStack::default()
        };

        compiler.next();
    }

    fn next(&mut self) {
        match self.lex.next() {
            Some((tk, start, end)) => {
                self.tk = Some(tk);
                self.start = start;
                self.end = end;
            }
            _ => {
                self.tk = None;
            }
        }
    }

    /// returns true if the current token can begin
    /// an atom
    pub fn can_begin_atom(&self) -> bool {
        matches!(
            self.tk,
            Some(
                // keywords
                Token::Let |
                Token::Loop |
                Token::Br |
                Token::Return |
                Token::If |

                // unary ops
                Token::BinOp(BinOp(false, BinOpVariant::Lt)) | // cast
                Token::AmbiguousOp(_) |
                Token::UnOp(_) |
                
                // symbols
                Token::LeftParen |
                Token::LeftBracket |
                
                // literals

                Token::Uinteger(_) |
                Token::Integer(_) |
                Token::Float(_) |
                Token::Overflown |
                Token::Ident(_) |
                Token::String(_) |
                Token::Char(_) |
                Token::Label(_)
            )
        )
    }

    fn span(&self) -> Span {
        Span::new(self.start, self.end)
    }

    pub(crate) fn error(&self, msg: Message, span: Span) {
        self.session.error(self.src_ref, msg, span);
    }

    /// for owning enum fields
    fn take<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self, Option<Token<'ast>>) -> Take<'ast, T>,
    {
        let t = self.tk.take();
        match f(self, t) {
            Take::Next(t) => {
                self.next();
                t
            }
            Take::Fill(t, tk) => {
                self.tk = tk;
                t
            }
            Take::NoFill(t) => t,
        }
    }

    pub fn operate_values(&mut self, lhs: Value<S>, rhs: Value<S>, op: BinOp) -> Value<S> {
        match (lhs, rhs) {
            (Value::Constant(l), Value::Expression(Expression(_, t))) => self.operate_values(
                self.compile_constant(l, Some(t)), 
                rhs,
                op
            ),
            (Value::Expression(Expression(_, t)), Value::Constant(r)) => self.operate_values(
                lhs,
                self.compile_constant(r, Some(t)),
                op
            ),
            (Value::Unreachable, _) | (_, Value::Unreachable) => Value::Unreachable,
            (Value::Expression(Expression(l, lt)), Value::Expression(Expression(r, rt))) => {
                if lt != rt {
                    self.error(Message::InvalidCoercion, self.span());
                    return Value::Unreachable;
                }

                if lt.indir.len() > 0 {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
                
                self.operate_values_expr(l, r, lt, op)
            }
            (Value::Constant(left), Value::Constant(right)) => {
                // since coercion is a DAG, ambiguity cannot happen here
                let left = self.coerce_constant(left, right).unwrap_or(left);
                let right = self.coerce_constant(right, left).unwrap_or(right);
                self.operate_values_const(left, right, op)
            },
        }
    }

    pub fn operate_values_const(&mut self, left: Constant, right: Constant, op: BinOp) -> Value<S> {
        match op.1 {
            BinOpVariant::Id => todo!(),
            BinOpVariant::Lt => {
                Value::Constant(Constant::U31(match (left, right) {
                    (Constant::U31(l), Constant::U31(r)) => u32::from(l < r),
                    (Constant::U63(l), Constant::U63(r)) => u32::from(l < r),
                    (Constant::Fxx(l), Constant::Fxx(r)) => u32::from(l < r),
                    (Constant::I32(l), Constant::I32(r)) => u32::from(l < r),
                    (Constant::I64(l), Constant::I64(r)) => u32::from(l < r),
                    (Constant::U32(l), Constant::U32(r)) => u32::from(l < r),
                    (Constant::U64(l), Constant::U64(r)) => u32::from(l < r),
                    (Constant::F64(l), Constant::F64(r)) => u32::from(l < r),
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                }))
            },
            BinOpVariant::Le => {
                Value::Constant(Constant::U31(match (left, right) {
                    (Constant::U31(l), Constant::U31(r)) => u32::from(l <= r),
                    (Constant::U63(l), Constant::U63(r)) => u32::from(l <= r),
                    (Constant::Fxx(l), Constant::Fxx(r)) => u32::from(l <= r),
                    (Constant::I32(l), Constant::I32(r)) => u32::from(l <= r),
                    (Constant::I64(l), Constant::I64(r)) => u32::from(l <= r),
                    (Constant::U32(l), Constant::U32(r)) => u32::from(l <= r),
                    (Constant::U64(l), Constant::U64(r)) => u32::from(l <= r),
                    (Constant::F64(l), Constant::F64(r)) => u32::from(l <= r),
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                }))
            },
            BinOpVariant::Gt => {
                Value::Constant(Constant::U31(match (left, right) {
                    (Constant::U31(l), Constant::U31(r)) => u32::from(l > r),
                    (Constant::U63(l), Constant::U63(r)) => u32::from(l > r),
                    (Constant::Fxx(l), Constant::Fxx(r)) => u32::from(l > r),
                    (Constant::I32(l), Constant::I32(r)) => u32::from(l > r),
                    (Constant::I64(l), Constant::I64(r)) => u32::from(l > r),
                    (Constant::U32(l), Constant::U32(r)) => u32::from(l > r),
                    (Constant::U64(l), Constant::U64(r)) => u32::from(l > r),
                    (Constant::F64(l), Constant::F64(r)) => u32::from(l > r),
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                }))
            },
            BinOpVariant::Ge => {
                Value::Constant(Constant::U31(match (left, right) {
                    (Constant::U31(l), Constant::U31(r)) => u32::from(l >= r),
                    (Constant::U63(l), Constant::U63(r)) => u32::from(l >= r),
                    (Constant::Fxx(l), Constant::Fxx(r)) => u32::from(l >= r),
                    (Constant::I32(l), Constant::I32(r)) => u32::from(l >= r),
                    (Constant::I64(l), Constant::I64(r)) => u32::from(l >= r),
                    (Constant::U32(l), Constant::U32(r)) => u32::from(l >= r),
                    (Constant::U64(l), Constant::U64(r)) => u32::from(l >= r),
                    (Constant::F64(l), Constant::F64(r)) => u32::from(l >= r),
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                }))
            },
            BinOpVariant::EqC => {
                Value::Constant(Constant::U31(match (left, right) {
                    (Constant::U31(l), Constant::U31(r)) => u32::from(l == r),
                    (Constant::U63(l), Constant::U63(r)) => u32::from(l == r),
                    (Constant::Fxx(l), Constant::Fxx(r)) => u32::from(l == r),
                    (Constant::I32(l), Constant::I32(r)) => u32::from(l == r),
                    (Constant::I64(l), Constant::I64(r)) => u32::from(l == r),
                    (Constant::U32(l), Constant::U32(r)) => u32::from(l == r),
                    (Constant::U64(l), Constant::U64(r)) => u32::from(l == r),
                    (Constant::F64(l), Constant::F64(r)) => u32::from(l == r),
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                }))
            },
            BinOpVariant::Neq => {
                Value::Constant(Constant::U31(match (left, right) {
                    (Constant::U31(l), Constant::U31(r)) => u32::from(l != r),
                    (Constant::U63(l), Constant::U63(r)) => u32::from(l != r),
                    (Constant::Fxx(l), Constant::Fxx(r)) => u32::from(l != r),
                    (Constant::I32(l), Constant::I32(r)) => u32::from(l != r),
                    (Constant::I64(l), Constant::I64(r)) => u32::from(l != r),
                    (Constant::U32(l), Constant::U32(r)) => u32::from(l != r),
                    (Constant::U64(l), Constant::U64(r)) => u32::from(l != r),
                    (Constant::F64(l), Constant::F64(r)) => u32::from(l != r),
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                }))
            },

            // PREVENT OVERFLOWS BELOW 64 BIT
            BinOpVariant::Add => Value::Constant(match (left, right) {
                (Constant::U31(l), Constant::U31(r)) => {
                    Constant::from_u64(u64::from(l).wrapping_add(r.into()))
                },
                (Constant::U63(l), Constant::U63(r)) => {
                    Constant::from_u64(u64::from(l).wrapping_add(r.into()))
                },
                (Constant::Fxx(l), Constant::Fxx(r)) => {
                    Constant::from_f64(f64::from(l) + f64::from(r))
                },
                (Constant::I32(l), Constant::I32(r)) => {
                    Constant::from_i64(i64::from(l).wrapping_add(r.into()))
                },
                (Constant::I64(l), Constant::I64(r)) => {
                    Constant::from_i64(l.wrapping_add(r))
                },
                (Constant::U32(l), Constant::U32(r)) => {
                    Constant::from_u64(u64::from(l).wrapping_add(r.into()))
                },
                (Constant::U64(l), Constant::U64(r)) => {
                    Constant::from_u64(l.wrapping_add(r))
                },
                (Constant::F64(l), Constant::F64(r)) => {
                    Constant::from_f64(l + r)
                },
                _ => {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
            }),
            BinOpVariant::Sub => Value::Constant(match (left, right) {
                (Constant::U31(l), Constant::U31(r)) => {
                    Constant::from_u64(u64::from(l).wrapping_add(r.into()))
                },
                (Constant::U63(l), Constant::U63(r)) => {
                    Constant::from_u64(u64::from(l).wrapping_add(r.into()))
                },
                (Constant::Fxx(l), Constant::Fxx(r)) => {
                    Constant::from_f64(f64::from(l) + f64::from(r))
                },
                (Constant::I32(l), Constant::I32(r)) => {
                    Constant::from_i64(i64::from(l).wrapping_add(r.into()))
                },
                (Constant::I64(l), Constant::I64(r)) => {
                    Constant::from_i64(l.wrapping_add(r))
                },
                (Constant::U32(l), Constant::U32(r)) => {
                    Constant::from_i64(i64::from(l).wrapping_add(r.into()))
                },
                (Constant::U64(l), Constant::U64(r)) => {
                    Constant::from_u64(l.wrapping_add(r))
                },
                (Constant::F64(l), Constant::F64(r)) => {
                    Constant::from_f64(l + r)
                },
                _ => {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
            }),
            BinOpVariant::Mul => Value::Constant(match (left, right) {
                (Constant::U31(l), Constant::U31(r)) => {
                    Constant::from_u64(u64::from(l).wrapping_mul(r.into()))
                },
                (Constant::U63(l), Constant::U63(r)) => {
                    Constant::from_u64(u64::from(l).wrapping_mul(r.into()))
                },
                (Constant::Fxx(l), Constant::Fxx(r)) => {
                    Constant::from_f64(f64::from(l) * f64::from(r))
                },
                (Constant::I32(l), Constant::I32(r)) => {
                    Constant::from_i64(i64::from(l).wrapping_mul(r.into()))
                },
                (Constant::I64(l), Constant::I64(r)) => {
                    Constant::from_i64(l.wrapping_mul(r))
                },
                (Constant::U32(l), Constant::U32(r)) => {
                    Constant::from_i64(i64::from(l).wrapping_mul(r.into()))
                },
                (Constant::U64(l), Constant::U64(r)) => {
                    Constant::from_u64(l.wrapping_mul(r))
                },
                (Constant::F64(l), Constant::F64(r)) => {
                    Constant::from_f64(l + r)
                },
                _ => {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
            }),
            BinOpVariant::Div => Value::Constant(match (left, right) {
                (Constant::U31(l), Constant::U31(r)) => {
                    Constant::from_u64(u64::from(l / r))
                },
                (Constant::U63(l), Constant::U63(r)) => {
                    Constant::from_u64(l / r)
                },
                (Constant::Fxx(l), Constant::Fxx(r)) => {
                    Constant::Fxx(l / r)
                },
                (Constant::I32(l), Constant::I32(r)) => {
                    Constant::from_i64(i64::from(l / r))
                },
                (Constant::I64(l), Constant::I64(r)) => {
                    Constant::from_i64(l / r)
                },
                (Constant::U32(l), Constant::U32(r)) => {
                    Constant::from_u64(u64::from(l / r))
                },
                (Constant::U64(l), Constant::U64(r)) => {
                    Constant::from_u64(l / r)
                },
                (Constant::F64(l), Constant::F64(r)) => {
                    Constant::from_f64(l / r)
                },
                _ => {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
            }),
            BinOpVariant::Mod => Value::Constant(match (left, right) {
                (Constant::U31(l), Constant::U31(r)) => {
                    Constant::from_u64(u64::from(l % r))
                },
                (Constant::U63(l), Constant::U63(r)) => {
                    Constant::from_u64(l % r)
                },
                (Constant::I32(l), Constant::I32(r)) => {
                    Constant::from_i64(i64::from(l % r))
                },
                (Constant::I64(l), Constant::I64(r)) => {
                    Constant::from_i64(l % r)
                },
                (Constant::U32(l), Constant::U32(r)) => {
                    Constant::from_u64(u64::from(l % r))
                },
                (Constant::U64(l), Constant::U64(r)) => {
                    Constant::from_u64(l % r)
                },
                _ => {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
            }),
            BinOpVariant::And => Value::Constant(match (left, right) {
                (Constant::U31(l), Constant::U31(r)) => {
                    Constant::from_u64(u64::from(l & r))
                },
                (Constant::U63(l), Constant::U63(r)) => {
                    Constant::from_u64(l & r)
                },
                (Constant::I32(l), Constant::I32(r)) => {
                    Constant::from_i64(i64::from(l & r))
                },
                (Constant::I64(l), Constant::I64(r)) => {
                    Constant::from_i64(l & r)
                },
                (Constant::U32(l), Constant::U32(r)) => {
                    Constant::from_u64(u64::from(l & r))
                },
                (Constant::U64(l), Constant::U64(r)) => {
                    Constant::from_u64(l & r)
                },
                _ => {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
            }),
            BinOpVariant::Or => Value::Constant(match (left, right) {
                (Constant::U31(l), Constant::U31(r)) => {
                    Constant::from_u64(u64::from(l | r))
                },
                (Constant::U63(l), Constant::U63(r)) => {
                    Constant::from_u64(l | r)
                },
                (Constant::I32(l), Constant::I32(r)) => {
                    Constant::from_i64(i64::from(l | r))
                },
                (Constant::I64(l), Constant::I64(r)) => {
                    Constant::from_i64(l | r)
                },
                (Constant::U32(l), Constant::U32(r)) => {
                    Constant::from_u64(u64::from(l | r))
                },
                (Constant::U64(l), Constant::U64(r)) => {
                    Constant::from_u64(l | r)
                },
                _ => {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
            }),
            BinOpVariant::Xor => Value::Constant(match (left, right) {
                (Constant::U31(l), Constant::U31(r)) => {
                    Constant::from_u64(u64::from(l ^ r))
                },
                (Constant::U63(l), Constant::U63(r)) => {
                    Constant::from_u64(l ^ r)
                },
                (Constant::I32(l), Constant::I32(r)) => {
                    Constant::from_i64(i64::from(l ^ r))
                },
                (Constant::I64(l), Constant::I64(r)) => {
                    Constant::from_i64(l ^ r)
                },
                (Constant::U32(l), Constant::U32(r)) => {
                    Constant::from_u64(u64::from(l ^ r))
                },
                (Constant::U64(l), Constant::U64(r)) => {
                    Constant::from_u64(l ^ r)
                },
                _ => {
                    self.error(Message::InvalidOperation, self.span());
                    return Value::Unreachable;
                }
            }),
            BinOpVariant::Shl => Value::Constant({
                let r = match right {
                    Constant::U31(n) if n < 65 => n,
                    Constant::U63(n) if n < 65 => n as u32,
                    Constant::I32(n) if n > 0 && n < 65 => n as u32,
                    Constant::I64(n) if n > 0 && n < 65 => n as u32,
                    Constant::U32(n) if n < 65 => n,
                    Constant::U64(n) if n < 65 => n as u32,
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                };

                match left {
                    Constant::U31(l) => {
                        Constant::from_u64(u64::from(l).wrapping_shl(r))
                    },
                    Constant::U63(l) => {
                        Constant::from_u64(l.wrapping_shl(r))
                    },
                    Constant::I32(l) => {
                        Constant::from_i64(i64::from(l).wrapping_shl(r))
                    },
                    Constant::I64(l) => {
                        Constant::from_i64(l.wrapping_shl(r))
                    },
                    Constant::U32(l) => {
                        Constant::from_i64(i64::from(l).wrapping_shl(r))
                    },
                    Constant::U64(l) => {
                        Constant::from_u64(l.wrapping_shl(r))
                    },
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                }
            }),
            BinOpVariant::Shr => Value::Constant({
                let r = match right {
                    Constant::U31(n) if n < 65 => n,
                    Constant::U63(n) if n < 65 => n as u32,
                    Constant::I32(n) if n > 0 && n < 65 => n as u32,
                    Constant::I64(n) if n > 0 && n < 65 => n as u32,
                    Constant::U32(n) if n < 65 => n,
                    Constant::U64(n) if n < 65 => n as u32,
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                };

                match left {
                    Constant::U31(l) => {
                        Constant::from_u64(u64::from(l).wrapping_shr(r))
                    },
                    Constant::U63(l) => {
                        Constant::from_u64(l.wrapping_shr(r))
                    },
                    Constant::I32(l) => {
                        Constant::from_i64(i64::from(l).wrapping_shr(r))
                    },
                    Constant::I64(l) => {
                        Constant::from_i64(l.wrapping_shr(r))
                    },
                    Constant::U32(l) => {
                        Constant::from_i64(i64::from(l).wrapping_shr(r))
                    },
                    Constant::U64(l) => {
                        Constant::from_u64(l.wrapping_shr(r))
                    },
                    _ => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable;
                    }
                }
            }),
        }
    }

    pub fn operate_values_expr(&mut self, left: S::ExpressionRef, right: S::ExpressionRef, t: Type, op: BinOp) -> Value<S> {
        match op.1 {
            BinOpVariant::Id => Value::Expression(Expression(right, t)),
            BinOpVariant::Lt => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 => self.module.i32_lt_s(left, right),
                    TypeVariant::I64 => self.module.i64_lt_s(left, right),
                    TypeVariant::U32 => self.module.i32_lt_u(left, right),
                    TypeVariant::U64 => self.module.i64_lt_u(left, right),
                    TypeVariant::F32 => self.module.f32_lt(left, right),
                    TypeVariant::F64 => self.module.f64_lt(left, right),
                }, Type::from(TypeVariant::I32))) // TODO: contextual type
            }
            BinOpVariant::Le => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 => self.module.i32_le_s(left, right),
                    TypeVariant::I64 => self.module.i64_le_s(left, right),
                    TypeVariant::U32 => self.module.i32_le_u(left, right),
                    TypeVariant::U64 => self.module.i64_le_u(left, right),
                    TypeVariant::F32 => self.module.f32_le(left, right),
                    TypeVariant::F64 => self.module.f64_le(left, right),
                }, Type::from(TypeVariant::I32))) // TODO: contextual type
            },
            BinOpVariant::Gt => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 => self.module.i32_gt_s(left, right),
                    TypeVariant::I64 => self.module.i64_gt_s(left, right),
                    TypeVariant::U32 => self.module.i32_gt_u(left, right),
                    TypeVariant::U64 => self.module.i64_gt_u(left, right),
                    TypeVariant::F32 => self.module.f32_gt(left, right),
                    TypeVariant::F64 => self.module.f64_gt(left, right),
                }, Type::from(TypeVariant::I32))) // TODO: contextual type
            },
            BinOpVariant::Ge => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 => self.module.i32_ge_s(left, right),
                    TypeVariant::I64 => self.module.i64_ge_s(left, right),
                    TypeVariant::U32 => self.module.i32_ge_u(left, right),
                    TypeVariant::U64 => self.module.i64_ge_u(left, right),
                    TypeVariant::F32 => self.module.f32_ge(left, right),
                    TypeVariant::F64 => self.module.f64_ge(left, right),
                }, Type::from(TypeVariant::I32))) // TODO: contextual type
            },
            BinOpVariant::EqC => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_eq(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_eq(left, right),
                    TypeVariant::F32 => self.module.f32_eq(left, right),
                    TypeVariant::F64 => self.module.f64_eq(left, right),
                }, Type::from(TypeVariant::I32))) // TODO: contextual type
            },
            BinOpVariant::Neq => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_eq(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_eq(left, right),
                    TypeVariant::F32 => self.module.f32_eq(left, right),
                    TypeVariant::F64 => self.module.f64_eq(left, right),
                }, Type::from(TypeVariant::I32))) // TODO: contextual type
            },
            BinOpVariant::Add => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_add(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_add(left, right),
                    TypeVariant::F32 => self.module.f32_add(left, right),
                    TypeVariant::F64 => self.module.f64_add(left, right),
                }, t))
            },
            BinOpVariant::Sub => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_sub(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_sub(left, right),
                    TypeVariant::F32 => self.module.f32_sub(left, right),
                    TypeVariant::F64 => self.module.f64_sub(left, right),
                }, t))
            },
            BinOpVariant::Mul => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_mul(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_mul(left, right),
                    TypeVariant::F32 => self.module.f32_mul(left, right),
                    TypeVariant::F64 => self.module.f64_mul(left, right),
                }, t))
            },
            BinOpVariant::Div => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::I32 => self.module.i32_div_s(left, right),
                    TypeVariant::I64 => self.module.i64_div_s(left, right),
                    TypeVariant::U32 => self.module.i32_div_u(left, right),
                    TypeVariant::U64 => self.module.i64_div_u(left, right),
                    TypeVariant::F32 => self.module.f32_div(left, right),
                    TypeVariant::F64 => self.module.f64_div(left, right),
                }, t))
            },
            BinOpVariant::Mod => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::F32 | TypeVariant::F64 => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable
                    },
                    TypeVariant::I32 => self.module.i32_rem_s(left, right),
                    TypeVariant::I64 => self.module.i64_rem_s(left, right),
                    TypeVariant::U32 => self.module.i32_rem_u(left, right),
                    TypeVariant::U64 => self.module.i64_rem_u(left, right),
                }, t))
            },
            BinOpVariant::And => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::F32 | TypeVariant::F64 => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable
                    },
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_and(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_and(left, right),
                }, t))
            },
            BinOpVariant::Or => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::F32 | TypeVariant::F64 => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable
                    },
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_or(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_or(left, right),
                }, t))
            },
            BinOpVariant::Xor => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::F32 | TypeVariant::F64 => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable
                    },
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_xor(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_xor(left, right),
                }, t))
            },
            BinOpVariant::Shl => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::F32 | TypeVariant::F64 => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable
                    },
                    TypeVariant::I32 | TypeVariant::U32 => self.module.i32_shl(left, right),
                    TypeVariant::I64 | TypeVariant::U64 => self.module.i64_shl(left, right),
                }, t))
            },
            BinOpVariant::Shr => {
                Value::Expression(Expression(match t.v {
                    TypeVariant::F32 | TypeVariant::F64 => {
                        self.error(Message::InvalidOperation, self.span());
                        return Value::Unreachable
                    },
                    TypeVariant::I32 => self.module.i32_shr_s(left, right),
                    TypeVariant::I64 => self.module.i64_shr_s(left, right),
                    TypeVariant::U32 => self.module.i32_shr_u(left, right),
                    TypeVariant::U64 => self.module.i64_shr_u(left, right),
                }, t))
            },
        }
    }

    pub fn compile_constant(&mut self, x: Constant, contextual_type: Option<Type>) -> Value<S> {
        match x {
            Constant::U31(n) => {
                if let Some(t) = contextual_type {
                    if t.is_reference() {
                        self.error(Message::ReferenceCoercion, self.span())
                    } else {
                        match t.v {
                            TypeVariant::U32 => {
                                return Value::Expression(Expression(
                                    self.module.i32_const(reinterpret_u32(n)),
                                    Type::from(TypeVariant::U32),
                                ));
                            },
                            TypeVariant::U64 => {
                                return Value::Expression(Expression(
                                    self.module.i64_const(reinterpret_u64(n.into())),
                                    Type::from(TypeVariant::U64),
                                ));
                            },
                            TypeVariant::I32 => {
                                return Value::Expression(Expression(
                                    self.module.i32_const(n.try_into().unwrap()),
                                    Type::from(TypeVariant::I32),
                                )); // invariant
                            }
                            TypeVariant::I64 => {
                                return Value::Expression(Expression(self.module.i64_const(n.into()), Type::from(TypeVariant::I64)));
                            }
                            TypeVariant::F64 => {
                                return Value::Expression(Expression(self.module.f64_const(n.into()), Type::from(TypeVariant::F64)));
                            }
                            _ => self.error(Message::InvalidCoercion, self.span()),
                        }
                    }
                } else {
                    self.error(Message::UncoercedU31, self.span());
                }

                Value::Unreachable
            },
            Constant::U63(n) => {
                if let Some(t) = contextual_type {
                    if t.is_reference() {
                        self.error(Message::ReferenceCoercion, self.span())
                    } else {
                        match t.v {
                            TypeVariant::U64 => {
                                return Value::Expression(Expression(
                                    self.module.i64_const(reinterpret_u64(n)),
                                    Type::from(TypeVariant::U32),
                                ))
                            }
                            TypeVariant::I64 => {
                                return Value::Expression(Expression(
                                    self.module.i64_const(n.try_into().unwrap()),
                                    Type::from(TypeVariant::I64),
                                ))
                            }
                            _ => self.error(Message::InvalidCoercion, self.span()),
                        }
                    }
                } else {
                    self.error(Message::UncoercedU63, self.span());
                }

                Value::Unreachable
            },
            Constant::Fxx(n) => {
                if let Some(t) = contextual_type {
                    if t.is_reference() {
                        self.error(Message::ReferenceCoercion, self.span())
                    } else {
                        match t.v {
                            TypeVariant::F32 => {
                                return Value::Expression(Expression(
                                    self.module.f32_const(n),
                                    Type::from(TypeVariant::F32),
                                ));
                            }
                            TypeVariant::F64 => {
                                return Value::Expression(Expression(
                                    self.module.f64_const(n.into()),
                                    Type::from(TypeVariant::F64),
                                ));
                            }
                            _ => self.error(Message::InvalidCoercion, self.span()),
                        }
                    }
                } else {
                    self.error(Message::UncoercedFxx, self.span());
                }

                Value::Unreachable
            },
            Constant::I32(n) => Value::Expression(Expression(self.module.i32_const(n), Type::from(TypeVariant::I32))),
            Constant::I64(n) => Value::Expression(Expression(self.module.i64_const(n), Type::from(TypeVariant::I64))),
            Constant::U32(n) => Value::Expression(Expression(
                self.module.i32_const(reinterpret_u32(n)),
                Type::from(TypeVariant::U32),
            )),
            Constant::U64(n) => Value::Expression(Expression(
                self.module.i64_const(reinterpret_u64(n)),
                Type::from(TypeVariant::U64),
            )),
            Constant::F64(n) => Value::Expression(Expression(self.module.f64_const(n), Type::from(TypeVariant::F64))),
        }
    }

    pub fn coerce_constant(&mut self, x: Constant, o: Constant) -> Option<Constant> {
        // we can coerce to the same degree as an explicit conversion, because it doesn't require an instruction
        match x {
            Constant::U31(n) => {
                Some(match o {
                    Constant::U31(_) => x,
                    Constant::U63(_) => Constant::U63(n.into()),
                    Constant::Fxx(_) => return None, // TODO
                    Constant::I32(_) => Constant::I32(n.try_into().unwrap()), // invariant
                    Constant::I64(_) => Constant::I64(n.into()),
                    Constant::U32(_) => Constant::U32(n),
                    Constant::U64(_) => Constant::U64(n.into()),
                    Constant::F64(_) => Constant::F64(n.into()),
                })
            },
            Constant::U63(n) => {
                Some(match o {
                    Constant::U31(_) => return None,
                    Constant::U63(_) => x,
                    Constant::Fxx(_) => return None,
                    Constant::I32(_) => return None, 
                    Constant::I64(_) => Constant::I64(n.try_into().unwrap()), // invariant
                    Constant::U32(_) => return None,
                    Constant::U64(_) => Constant::U64(n),
                    Constant::F64(_) => return None,
                })
            },
            Constant::Fxx(n) => {
                Some(match o {
                    Constant::U31(_) => return None,
                    Constant::U63(_) => return None,
                    Constant::Fxx(_) => x,
                    Constant::I32(_) => return None, 
                    Constant::I64(_) => return None,
                    Constant::U32(_) => return None,
                    Constant::U64(_) => return None,
                    Constant::F64(_) => Constant::F64(n.into()),
                })
            },
            Constant::I32(n) => {
                Some(match o {
                    Constant::U31(_) => return None,
                    Constant::U63(_) => return None,
                    Constant::Fxx(_) => return None,
                    Constant::I32(_) => x, 
                    Constant::I64(_) => Constant::I64(n.into()),
                    Constant::U32(_) => return None,
                    Constant::U64(_) => return None,
                    Constant::F64(_) => Constant::F64(n.into()),
                })
            },
            Constant::I64(n) => {
                match o {
                    Constant::I64(_) => Some(x),
                    _ => None
                }
            },
            Constant::U32(n) => {
                Some(match o {
                    Constant::U31(_) => return None,
                    Constant::U63(_) => return None,
                    Constant::Fxx(_) => return None,
                    Constant::I32(_) => return None, 
                    Constant::I64(_) => Constant::I64(n.into()),
                    Constant::U32(_) => x,
                    Constant::U64(_) => Constant::U64(n.into()),
                    Constant::F64(_) => Constant::F64(n.into()),
                })
            },
            Constant::U64(n) => {
                match o {
                    Constant::U64(_) => Some(x),
                    _ => None
                }
            },
            Constant::F64(n) => {
                match o {
                    Constant::F64(_) => Some(x),
                    _ => None
                }
            },
        }
    }

    pub fn expect_ident(&mut self, token_after: &Option<Token>) -> Option<&'ast str> {
        if &self.tk == token_after {
            // struct  {
            //        ^
            let pos = self.start;
            let span = Span::new(pos - 1, pos);
            self.error(Message::MissingIdentifier, span);
            None
        } else {
            self.take(|this, t| {
                Next(match t {
                    // take
                    Some(Token::Ident(s)) => {
                        // struct ident {
                        //        ^^^^^
                        Some(s)
                    }
                    Some(Token::Label(_)) => {
                        // struct $label {
                        //        ^^^^^^
                        this.error(Message::LabelIsNotIdentifier, this.span());
                        None
                    }
                    _ => {
                        // struct ! {
                        //        ^
                        this.error(Message::MalformedIdentifier, this.span());
                        None
                    }
                })
            })
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        let start = self.start;
		let refkind = match self.tk {
			Some(Token::AmbiguousOp(AmbiguousOp::Ampersand)) => {
				self.next();
				match self.tk {
					Some(Token::Mut) => {
						self.next();
						ReferenceKind::Mutable
					}
					_ => ReferenceKind::Immutable
				}
			}
			_ => ReferenceKind::None
		};

        let mut asterisk_overflow_start = None;
        let mut indir = Indir::none();
        let mut len = 0;
        loop {
            match self.tk {
                Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {
                    if len < 5 {
                        self.next();
                        indir = indir.add(match self.tk {
                            Some(Token::Mut) => {
                                self.next();
                                true
                            }
                            _ => false,
                        });
                    } else {
                        // *******type
                        //      ^^
                        if asterisk_overflow_start.is_none() {
                            asterisk_overflow_start = Some(self.start);
                        }

                        let end = self.end;
                        self.next();

                        match self.tk {
                            Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {}
                            _ => self.error(
                                Message::TooMuchIndirection,
                                Span::new(asterisk_overflow_start.unwrap(), end),
                            ),
                        }
                    }
                    len += 1;
                }
                Some(ref ch @ (Token::Union | Token::Struct)) => {
                    let is_struct = ch == &Token::Struct;
                    self.next();
                    let body = self.type_body(true)?;
                    let end = body.1.end;
                    /*return Some(Spanned(
                        Type::with_indir(
                            if is_struct {
                                TypeVariant::Struct(body)
                            } else {
                                TypeVariant::Union(body)
                            },
                            indir,
							refkind
                        ),
                        Span::new(start, end),
                    ));*/
					todo!()
                }
                _ => {
                    return self.take(|this, t| match t {
                        Some(Token::Ident(s)) => {
                            let end = this.end;
                            Next(Some(
                                Type::with_indir(s.into(), indir, refkind)
                            ))
                        }
                        t => {
                            this.error(Message::MalformedType, this.span());
                            Fill(None, t)
                        }
                    })
                }
            }
        }
    }

    fn parse_decl(&mut self) -> Option<(IdentPair<'ast>, Option<Value<S>>)> {
        let start = self.start;

        let pair = self.ident_type_pair(false)?;
        let rhs = match self.tk {
            Some(Token::BinOp(BinOp(true, BinOpVariant::Id))) => {
                self.next();
                Some(self.atom())
            }
            _ => None,
        };

        Some((pair, rhs))
    }

    fn ident_type_pair(&mut self, require_type: bool) -> Option<IdentPair<'ast>> {
        let start = self.start;

        // mut ident: type
        // ^^^
        let mutable = match self.tk {
            Some(Token::Mut) => {
                self.next();
                true
            }
            _ => false,
        };

        let ident = self.expect_ident(&Some(Token::Colon))?;
        let t = match self.tk {
            Some(Token::Colon) => {
                self.next();
                Some(self.parse_type()?)
            }
            _ => {
                if require_type {
                    // mut ident ...
                    //          ^
                    self.error(Message::MissingType, self.span());
                }
                None
            }
        };

        Some(
            IdentPair { mutable, ident, t },
        )
    }

    fn subatom(
        &mut self,
        mut lhs: Value<S>,
        min_prec: u8,
    ) -> Value<S> {
        // https://en.wikipedia.org/wiki/Operator-precedence_parser

        // while [t] is a
        loop {
            // binary operator
            let t = match self.tk {
                Some(Token::BinOp(t)) => t,
                Some(Token::AmbiguousOp(ref t)) => BinOp(false, t.binary()),
                _ => break,
            };

            // whose precedence is >= [min_prec]
            let current_prec = t.prec();
            if t.prec() >= min_prec {
                self.next();
                let mut rhs = self.primaryatom();
                loop {
                    let (right_associative, next_prec) = match self.tk {
                        Some(Token::BinOp(BinOp(true, v))) => (true, v.prec()),
                        Some(Token::BinOp(BinOp(false, v))) => (false, v.prec()),
                        Some(Token::AmbiguousOp(v)) => (false, v.binary().prec()),
                        _ => break,
                    };

                    if next_prec > current_prec {
                        rhs = self.subatom(rhs, current_prec + 1);
                    } else if right_associative && next_prec == current_prec {
                        rhs = self.subatom(rhs, current_prec);
                    } else {
                        break;
                    }
                }
                
                lhs = self.operate_values(lhs, rhs, t);
            } else {
                break;
            }
        }
        lhs
    }

    pub fn atom(&mut self) -> Value<S> {
        let lhs = self.primaryatom();
        self.subatom(lhs, 0)
    }
}

fn reinterpret_u32(n: u32) -> i32 {
    unsafe { std::mem::transmute::<u32, i32>(n) }
}

fn reinterpret_u64(n: u64) -> i64 {
    unsafe { std::mem::transmute::<u64, i64>(n) }
}
