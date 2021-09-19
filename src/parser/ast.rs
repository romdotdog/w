use crate::lexer::Op;

#[derive(Debug)]
pub enum TopLevel {
    Fn(String, Atom, Type),
}

type BAtom = Box<Atom>;

#[derive(Debug)]
pub enum Atom {
    Integer(i64),
    Float(f64),
    Ident(String),
    Null,

    Paren(BAtom),
    BinOp(BAtom, Op, BAtom),
    UnOp(Op, BAtom),

    Reinterpret(Type, BAtom),
    Cast(Type, BAtom),

    Block(Vec<Atom>, BAtom),
    If(BAtom, BAtom, Option<BAtom>),
}

#[derive(Debug)]
pub enum Type {
    Auto,
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    User(String),
}

impl From<String> for Type {
    fn from(s: String) -> Self {
        match s.as_str() {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => Type::User(s),
        }
    }
}
