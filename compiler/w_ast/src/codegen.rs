use crate::{Decl, ReferenceKind, Spanned, TopLevel, TypeBody};

use super::{Atom, IdentPair, IncDec, Program, Type, TypeVariant};
use w_lexer::token::UnOp;

use std::fmt::{Display, Formatter, Result};

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.fmt(f)
    }
}

impl<'ast> Display for Program<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for toplevel in &self.0 {
            write!(f, "{}\n\n", toplevel)?;
        }
        Ok(())
    }
}

impl<'ast> Display for TopLevel<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TopLevel::Fn {
                name,
                params,
                atom,
                t,
                exported,
            } => {
                if *exported {
                    write!(f, "export ")?;
                }

                write!(f, "fn {}(", name)?;

                let l = params.len();
                for (i, pair) in params.iter().enumerate() {
                    write!(f, "{}", pair)?;
                    if i + 1 < l {
                        write!(f, ", ")?;
                    }
                }

                if let Some(t) = t {
                    write!(f, "): {} {}", t, atom)
                } else {
                    write!(f, ") {}", atom)
                }
            }

            TopLevel::Enum { name, fields } => {
                writeln!(f, "enum {} {{", name)?;
                let mut v: Vec<_> = fields.0.iter().collect();
                v.sort_by_key(|k| k.1);
                let mut d = 0;
                for (s, &v) in v {
                    if d == v {
                        writeln!(f, "\t{},", s)?;
                        d += 1;
                    } else {
                        writeln!(f, "\t{} = {},", s, v)?;
                        d = v + 1;
                    }
                }
                write!(f, "}}")
            }

            TopLevel::Static(decl) => {
                write!(f, "static {};", decl)
            }

            TopLevel::Struct(name, body) => {
                write!(f, "struct {} ", name)?;
                verbose_type_body(f, &body.0)
            }

            TopLevel::Union(name, body) => {
                write!(f, "union {} ", name)?;
                verbose_type_body(f, &body.0)
            }
        }
    }
}

fn verbose_type_body(f: &mut Formatter<'_>, body: &TypeBody) -> Result {
    writeln!(f, "{{")?;
    for ident in &body.0 {
        writeln!(f, "\t{};", ident)?;
    }
    write!(f, "}}")
}

fn inline_type_body(f: &mut Formatter<'_>, body: &TypeBody) -> Result {
    write!(f, "{{ ")?;

    let l = body.0.len();
    for (i, ident) in body.0.iter().enumerate() {
        write!(f, "{}", ident)?;

        if i + 1 < l {
            write!(f, "; ")?;
        }
    }

    write!(f, " }}")
}

impl<'ast> Display for Type<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.refkind {
            ReferenceKind::None => {}
            ReferenceKind::Immutable => write!(f, "&")?,
            ReferenceKind::Mutable => write!(f, "&mut ")?,
        }
        write!(f, "{}{}", self.indir, self.v)
    }
}

impl<'ast> Display for Decl<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.rhs {
            Some(rhs) => write!(f, "{} = {}", self.pair, rhs),
            None => write!(f, "{}", self.pair),
        }
    }
}

impl<'ast> Display for TypeVariant<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TypeVariant::Void => write!(f, "void"),
            TypeVariant::I32 => write!(f, "i32"),
            TypeVariant::I64 => write!(f, "i64"),
            TypeVariant::U32 => write!(f, "u32"),
            TypeVariant::U64 => write!(f, "u64"),
            TypeVariant::F32 => write!(f, "f32"),
            TypeVariant::F64 => write!(f, "f64"),
            TypeVariant::Struct(body) => {
                write!(f, "struct ")?;
                inline_type_body(f, &body.0)
            }
            TypeVariant::Union(body) => {
                write!(f, "union ")?;
                inline_type_body(f, &body.0)
            }
            TypeVariant::Unresolved(s) => write!(f, "{}", s),
        }
    }
}

impl<'ast> Display for Atom<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Atom::Integer(i) => write!(f, "{}", i),
            Atom::UInteger(u) => write!(f, "{}", u),
            Atom::Float(n) => write!(f, "{}", n),
            Atom::Ident(t) => write!(f, "{}", t),
            Atom::String(s) => write!(f, "\"{}\"", s.replace("\"", "\\\"")),
            Atom::Char(c) => write!(f, "'{}'", c),
            Atom::Paren(a) => write!(f, "({})", a),
            Atom::BinOp(lhs, op, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            Atom::Access(lhs, ident) => write!(f, "{}.{}", lhs, ident),
            Atom::Index(lhs, rhs) => write!(f, "{}[{}]", lhs, rhs),
            Atom::PostIncDec(lhs, incdec) => match incdec {
                IncDec::Inc => write!(f, "{}++", lhs),
                IncDec::Dec => write!(f, "{}--", lhs),
            },
            Atom::Reinterpret(t, rhs) => write!(f, "<{}!>{}", t, rhs),
            Atom::Cast(t, rhs) => write!(f, "<{}>{}", t, rhs),
            Atom::UnOp(o, rhs) => match o {
                UnOp::Deref => write!(f, "*{}", rhs),
                UnOp::AddrOf => write!(f, "&{}", rhs),
                UnOp::Minus => write!(f, "-{}", rhs),
                UnOp::Plus => write!(f, "+{}", rhs),
                UnOp::Inc => write!(f, "++{}", rhs),
                UnOp::Dec => write!(f, "--{}", rhs),
                UnOp::BNot => write!(f, "~{}", rhs),
                UnOp::LNot => write!(f, "!{}", rhs),
            },
            Atom::Block { label, blocks, ret } => {
                if let Some(label) = label {
                    writeln!(f, "${}: {{", label)?;
                } else {
                    writeln!(f, "{{")?;
                }

                for atom in blocks {
                    writeln!(f, "\t{};", format!("{}", atom).replace("\n", "\n\t"))?;
                }

                match ret {
                    None => write!(f, "}}"),
                    Some(l) => write!(f, "\t{}\n}}", format!("{}", l).replace("\n", "\n\t")),
                }
            }
            Atom::Call(lhs, args) => {
                write!(f, "{}(", lhs)?;
                let l = args.len();
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;

                    if i + 1 < l {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            }
            Atom::Let(decl) => write!(f, "let {}", decl),
            Atom::If {
                cond,
                true_branch,
                false_branch,
            } => match false_branch {
                Some(e) => write!(f, "if {} {} else {}", cond, true_branch, e),
                None => write!(f, "if {} {}", cond, true_branch),
            },
            Atom::Loop {
                label,
                binding,
                block,
            } => {
                if let Some(label) = label {
                    write!(f, "${}: loop ", label)?;
                } else {
                    write!(f, "loop ")?;
                }

                if let Some(binding) = binding {
                    write!(f, "{} ", binding)?;
                }

                write!(f, "{}", block)
            }
            Atom::Br { ret, label, cond } => {
                write!(f, "br")?;
                if let Some(ret) = ret {
                    write!(f, " {}", ret)?;
                }
                if let Some(label) = label {
                    write!(f, " -> ${}", label)?;
                }
                if let Some(cond) = cond {
                    write!(f, " if {}", cond)?;
                }
                Ok(())
            }
            Atom::Return(Some(ret)) => write!(f, "return {}", ret),
            Atom::Return(None) => write!(f, "return"),
        }
    }
}

impl<'ast> Display for IdentPair<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.mutable.is_some() {
            write!(f, "mut ")?;
        }

        write!(f, "{}", self.ident)?;

        match &self.t {
            Some(t) => write!(f, ": {}", t)?,
            None => {}
        }

        Ok(())
    }
}
