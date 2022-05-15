
use locals::Flow;
use registry::Registry;
use types::itemref::{ItemRef, HeapType, StackType};
use types::{IdentPair, Value};
use types::expression::Expression;
use types::typ::{VOID, Type, UNREACHABLE, I32, U32, I8, U8, U16, I16, U64, I64};
use w_codegen::{Serializer};
use w_errors::Message;
use w_lexer::token::{AmbiguousOp, BinOp, BinOpVariant, Token};
use w_lexer::Lexer;

pub mod handler;
mod primaryatom;
mod simpleatom;
mod toplevel;
mod types;
mod symbol_stack;
mod locals;
mod registry;

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
	symbols: SymbolStack<'ast>,
    flow: Flow,
    registry: Registry<'ast>
}

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
			symbols: SymbolStack::default(),
            flow: Flow::default(),
            registry: Registry::default()
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
			symbols: SymbolStack::default(),
            flow: Flow::default(),
            registry: Registry::default()
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

    pub(crate) fn unreachable_expr(&mut self) -> Expression<S> {
        Expression(self.module.unreachable(), UNREACHABLE)
    }

    pub(crate) fn unreachable(&mut self) -> Value<S> {
        Value::Expression(self.unreachable_expr())
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

    pub fn make_load(&mut self, ptr: Expression<S>) -> Value<S> {
        let Expression(ptr, typ) = ptr;
        Value::Expression(if let Some(meta) = typ.meta.deref() { // TODO: 64 bit loads
            Expression(self.module.i32_load(0, ptr), Type { meta, item: typ.item })
        } else {
            match typ.item {
                ItemRef::Void => todo!(),
                ItemRef::Unreachable => todo!(),
                ItemRef::HeapType(t) => match t {
                    HeapType::I8 => Expression(self.module.i32_load8_s(0, ptr), typ),
                    HeapType::U8 => Expression(self.module.i32_load8_u(0, ptr), typ),
                    HeapType::I16 => Expression(self.module.i32_load16_s(0, ptr), typ),
                    HeapType::U16 => Expression(self.module.i32_load16_u(0, ptr), typ),
                },
                ItemRef::StackType(t) => match t {
                    StackType::I32 | StackType::U32 => Expression(self.module.i32_load(0, ptr), typ),
                    StackType::I64 | StackType::U64 => Expression(self.module.i64_load(0, ptr), typ),
                    StackType::F32 => Expression(self.module.f32_load(0, ptr), typ),
                    StackType::F64 => Expression(self.module.f64_load(0, ptr), typ),
                },
                ItemRef::ItemRef(_) => todo!(), // struct loads?
            }
        })
    }

    pub fn operate_values(&mut self, lhs: Value<S>, rhs: Value<S>, op: BinOp) -> Value<S> {
        match (lhs, rhs) {
            (Value::Constant(l), Value::Expression(Expression(_, t))) => {
                if let Some(x) = l.compile(self.module, t) {
                    self.operate_values(
                        Value::Expression(x), 
                        rhs,
                        op
                    )
                } else {
                    self.error(Message::NeedType, self.span());
                    self.unreachable()
                }
            },
            (Value::Expression(Expression(_, t)), Value::Constant(r)) => {
                if let Some(x) = r.compile(self.module, t) {
                    self.operate_values(
                        lhs, 
                        Value::Expression(x),
                        op
                    )
                } else {
                    self.error(Message::NeedType, self.span());
                    self.unreachable()
                }
            },
            (Value::Expression(l), Value::Expression(r)) => {
                if l.1 == UNREACHABLE || r.1 == UNREACHABLE {
                    return self.unreachable();
                }
                match l.operate(self.module, r, op) {
                    Some(x) => Value::Expression(x),
                    None => {
                        self.error(Message::InvalidOperation, self.span());
                        self.unreachable()
                    },
                }
            }
            (Value::Constant(left), Value::Constant(right)) => {
                match left.operate(right, op) {
                    Some(x) => Value::Constant(x),
                    None => {
                        self.error(Message::InvalidOperation, self.span());
                        self.unreachable()
                    },
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
        let typ = VOID;

        if let Some(Token::AmbiguousOp(AmbiguousOp::Ampersand)) = self.tk {
            self.next();
            typ.meta = typ.meta.set_reference();
            if let Some(Token::Mut) = self.tk {
                self.next();
            } else {
                self.error(Message::MustBeMut, self.span())
            }
        }

        let mut asterisk_overflow_start = None;
        loop {
            match self.tk {
                Some(Token::AmbiguousOp(AmbiguousOp::Asterisk)) => {
                    let mutable = match self.tk {
                        Some(Token::Mut) => {
                            self.next();
                            true
                        }
                        _ => false,
                    };

                    if let Some(meta) = typ.meta.ref_(mutable) {
                        typ.meta = meta;
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
                }
                Some(ref ch @ (Token::Union | Token::Struct)) => {
                    let is_struct = ch == &Token::Struct;
                    self.next();
                    let body = self.type_body(true);
                    //let end = body.1.end;
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
                            let item_ref = if let Some(t) = ItemRef::from_str(s) {
                                t
                            } else if let Some(index) = self.registry.resolve(s) {
                                ItemRef::ItemRef(index)
                            } else {
                                this.error(Message::UnresolvedType, this.span());
                                return Next(None)
                            };
                            Next(Some(Type { meta: typ.meta, item: item_ref }))
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

    fn mutable(&mut self) -> bool {
        match self.tk {
            Some(Token::Mut) => {
                self.next();
                true
            }
            _ => false,
        }
    }

    fn parse_decl(&mut self) -> Option<(&'ast str, Value<S>)> {
        let start = self.start;
        let mutable = self.mutable();
        let ident = self.expect_ident(&Some(Token::Colon))?;
        let typ = match self.tk {
            Some(Token::Colon) => {
                self.next();
                self.parse_type()          
            }
            _ => None
        };

        match self.tk {
            Some(Token::BinOp(BinOp(true, BinOpVariant::Id))) => {
                self.next();
                let v = self.atom();
                if let Some(typ) = typ {
                    match v {
                        Value::Expression(Expression(_, ref mut xt)) => {
                            if *xt != typ {
                                self.error(Message::TypeMismatch, self.span())
                            }

                            if mutable {
                                xt.meta = xt.meta.set_mutable();
                            } else {
                                xt.meta = xt.meta.unset_mutable();
                            }
                        },
                        Value::Constant(_) => todo!(),
                    }
                }

                Some((ident, v))
            }
            _ => todo!(),
        }
    }

    fn ident_type_pair(&mut self) -> Option<IdentPair<'ast>> {
        let mutable = self.mutable();
        let ident = self.expect_ident(&Some(Token::Colon))?;
        let t = match self.tk {
            Some(Token::Colon) => {
                self.next();
                self.parse_type()?
            }
            _ => {
                // mut ident ...
                //          ^
                self.error(Message::MissingType, self.span());
                return None;
            }
        };

        if mutable {
            t.meta = t.meta.set_mutable();
        }

        Some(IdentPair { ident, typ: t })
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