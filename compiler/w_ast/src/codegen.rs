use crate::{Spanned, WEnum, WStruct, WUnion};

use super::{Atom, IdentPair, IncDec, Program, Type, TypeVariant, WFn};
use w_lexer::token::UnOp;

use std::fmt::{Display, Result};

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        self.0.fmt(f)
    }
}

impl<'ast> Display for Program<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        for wenum in &self.enums {
            write!(f, "{}\n\n", wenum)?;
        }
        for wstruct in &self.structs {
            write!(f, "{}\n\n", wstruct)?;
        }
        for wunion in &self.unions {
            write!(f, "{}\n\n", wunion)?;
        }
        for wfn in &self.fns {
            write!(f, "{}\n\n", wfn)?;
        }
        Ok(())
    }
}

impl<'ast> Display for WFn<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        if self.exported {
            write!(f, "export ")?;
        }

        write!(f, "fn {}(", self.name)?;

        let l = self.params.len() - 1;
        for (i, pair) in self.params.iter().enumerate() {
            write!(f, "{}", pair)?;
            if i < l {
                write!(f, ", ")?;
            }
        }

        if let Some(t) = &self.t {
            write!(f, "): {} {}", t, self.atom)
        } else {
            write!(f, ") {}", self.atom)
        }
    }
}

impl<'ast> Display for WStruct<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        writeln!(f, "struct {} {{", self.name)?;
        for ident in &self.fields.0 {
            writeln!(f, "\t{};", ident)?;
        }
        write!(f, "}}")
    }
}

impl<'ast> Display for WUnion<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        writeln!(f, "union {} {{", self.name)?;
        for ident in &self.fields.0 {
            writeln!(f, "\t{};", ident)?;
        }
        write!(f, "}}")
    }
}

impl<'ast> Display for WEnum<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        writeln!(f, "enum {} {{", self.name)?;
        let mut v: Vec<_> = self.fields.0.iter().collect();
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
}

impl<'ast> Display for Type<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        write!(f, "{}{}", self.indir, self.v)
    }
}

fn type_struct_like(
    f: &mut std::fmt::Formatter<'_>,
    v: &Spanned<Vec<Spanned<IdentPair>>>,
) -> Result {
    let l = v.0.len() - 1;
    for (i, ident) in v.0.iter().enumerate() {
        write!(f, "{}", ident)?;

        if i < l {
            write!(f, "; ")?;
        }
    }
    write!(f, " }}")
}

impl<'ast> Display for TypeVariant<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self {
            TypeVariant::Void => write!(f, "void"),
            TypeVariant::I32 => write!(f, "i32"),
            TypeVariant::I64 => write!(f, "i64"),
            TypeVariant::U32 => write!(f, "u32"),
            TypeVariant::U64 => write!(f, "u64"),
            TypeVariant::F32 => write!(f, "f32"),
            TypeVariant::F64 => write!(f, "f64"),
            TypeVariant::Struct(v) => {
                write!(f, "struct {{ ")?;
                type_struct_like(f, v)
            }
            TypeVariant::Union(v) => {
                write!(f, "union {{ ")?;
                type_struct_like(f, v)
            }
            TypeVariant::Unresolved(s) => write!(f, "{}", s),
        }
    }
}

impl<'ast> Display for Atom<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
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
                let l = args.len() - 1;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;

                    if i < l {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            }
            Atom::Let(pair, rhs) => {
                write!(f, "let {}", pair)?;
                if let Some(rhs) = rhs {
                    write!(f, " = {}", rhs)?;
                }
                Ok(())
            }
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
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
