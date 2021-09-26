use crate::lexer::{BinOp, BinOpVariant, UnOp};

use super::{
    ast::{AtomVariant, Program, Type, TypeVariant, WFn},
    Atom,
};

use std::fmt::Display;

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for wfn in &self.fns {
            write!(f, "{}", wfn)?;
        }
        Ok(())
    }
}

impl Display for WFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;

        let l = self.params.len();
        for (i, (s, t)) in self.params.iter().enumerate() {
            write!(f, "{}: {}", s, t)?;
            if i + 1 < l {
                write!(f, ", ")?;
            }
        }

        write!(f, "): {} {}", self.t, self.atom)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.indir, self.v)
    }
}

impl Display for TypeVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeVariant::Auto => write!(f, "<inferred>"),
            TypeVariant::Void => write!(f, "void"),
            TypeVariant::I32 => write!(f, "i32"),
            TypeVariant::I64 => write!(f, "i64"),
            TypeVariant::U32 => write!(f, "u32"),
            TypeVariant::U64 => write!(f, "u64"),
            TypeVariant::F32 => write!(f, "f32"),
            TypeVariant::F64 => write!(f, "f64"),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.v {
            AtomVariant::Integer(i) => write!(f, "{}", i),
            AtomVariant::Float(n) => write!(f, "{}", n),
            AtomVariant::Ident(t) => write!(f, "{}", t),
            AtomVariant::String(s) => write!(f, "\"{}\"", s.replace("\"", "\\\"")),
            AtomVariant::Char(c) => write!(f, "'{}'", c),
            AtomVariant::Paren(a) => write!(f, "({})", a),
            AtomVariant::BinOp(lhs, op, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            AtomVariant::UnOp(o, rhs) => match o {
                UnOp::Deref => write!(f, "*{}", rhs),
                UnOp::AddrOf => write!(f, "&{}", rhs),
                UnOp::Minus => write!(f, "-{}", rhs),
                UnOp::Plus => write!(f, "+{}", rhs),
                UnOp::Inc => write!(f, "++{}", rhs),
                UnOp::Dec => write!(f, "--{}", rhs),
                UnOp::Reinterpret => write!(f, "<{}!>{}", self.t, rhs),
                UnOp::Cast => write!(f, "<{}>{}", self.t, rhs),
                UnOp::BNot => write!(f, "~{}", rhs),
                UnOp::LNot => write!(f, "!{}", rhs),
            },
            AtomVariant::Block(a, l) => {
                writeln!(f, "{{")?;
                for atom in a {
                    writeln!(f, "\t{};", format!("{}", atom).replace("\n", "\n\t"))?
                }
                match l {
                    None => write!(f, "}}"),
                    Some(l) => write!(f, "\t{}\n}}", format!("{}", l).replace("\n", "\n\t")),
                }
            }
            AtomVariant::Let(mutable, v) => {
                match mutable {
                    true => write!(f, "let mut ")?,
                    false => write!(f, "let ")?,
                }

                let l = v.len();
                for (i, v) in v.iter().enumerate() {
                    match &v.t.v {
                        TypeVariant::Auto => write!(f, "{} = {}", v.lvalue, v.rvalue),
                        _ => write!(f, "{}: {} = {}", v.lvalue, v.t, v.rvalue),
                    }?;

                    if i + 1 < l {
                        write!(f, ",\n\t")?
                    }
                }
                Ok(())
            }
            AtomVariant::If(cond, body, else_) => match else_ {
                Some(e) => write!(f, "if {} {} else {}", cond, body, e),
                None => write!(f, "if {} {}", cond, body),
            },
            AtomVariant::Return(r) => write!(f, "return {}", r),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Compound(BinOpVariant::Id) => write!(f, "="),
            BinOp::Compound(v) => write!(f, "{}=", v),
            BinOp::Regular(v) => write!(f, "{}", v),
        }
    }
}

impl Display for BinOpVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOpVariant::Id => panic!(),
            BinOpVariant::Acs => write!(f, "."),
            BinOpVariant::Lt => write!(f, "<"),
            BinOpVariant::Le => write!(f, "<="),
            BinOpVariant::Gt => write!(f, ">"),
            BinOpVariant::Ge => write!(f, ">="),
            BinOpVariant::EqC => write!(f, "=="),
            BinOpVariant::Neq => write!(f, "!="),
            BinOpVariant::Add => write!(f, "+"),
            BinOpVariant::Sub => write!(f, "-"),
            BinOpVariant::Mul => write!(f, "*"),
            BinOpVariant::Div => write!(f, "/"),
            BinOpVariant::Mod => write!(f, "%"),
            BinOpVariant::Xor => write!(f, "^"),
            BinOpVariant::And => write!(f, "&"),
            BinOpVariant::Or => write!(f, "|"),
            BinOpVariant::Rsh => write!(f, ">>"),
            BinOpVariant::Lsh => write!(f, "<<"),
        }
    }
}
