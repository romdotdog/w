use std::fmt::Display;

//#[cfg(not(wasm))]
//mod for_humans;

macro_rules! errors {
	($($n: ident => $s: expr), *) => {
		#[derive(PartialEq, Eq)]
		pub enum Message {
			$($n), *
		}

		impl Display for Message {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				match self {
					$(Message::$n => write!(f, $s)), *
				}
			}
		}
	};
}

errors!(
    UnexpectedToken => "unexpected token",
    FileNotFound => "file not found",
    RecursiveInclude => "cannot include recursively",
    MissingSemicolon => "missing a semicolon or closing brace",
    MissingIdentifier => "missing identifier here",
    MalformedIdentifier => "invalid identifier here",
    MissingType => "missing type here",
    MalformedType => "found malformed type",
    DuplicateEnumField => "redeclaration of enumerator",
    LabelIsNotIdentifier => "labels cannot be used as identifiers",
    IdentifierIsNotLabel => "identifiers cannot be used as labels",
    IntegerNoFit => "literal does not fit inside an `i64`",
    LiteralTooLarge => "numeric literal too large",
    MissingInteger => "integer expected here",
    MissingLabel => "missing label here",
    CannotFollowLabel => "only a loop or block can follow a label",
    MissingOpeningBracket => "'{{' expected here",
    MissingOpeningParen => "'(' expected here",
    MissingClosingParen => "')' expected here",
    MissingClosingBracket => "'}}' expected here",
    MissingClosingAngleBracket => "'>' expected here",
    MissingClosingSqBracket => "']' expected here",
    MissingColon => "':' expected here",
    InvalidTopLevel => "only functions, globals and directives are allowed here",
    TooMuchIndirection => "at most only 5 levels of indirection are allowed",
    LoopBodyBlock => "loop body may only be a block",
    InvalidCoercion => "invalid coercion",
    InvalidOperation => "invalid operation",
    ReferenceCoercion => "cannot coerce to reference",
    UncoercedFxx => "literal is of type fxx, must coerce to either f32 or f64",
    UncoercedU31 => "literal is of type u31, must coerce to either i32, i64, u32, u64 or f64",
    UncoercedU63 => "literal is of type u63, must coerce to either i64 or u64",
    BranchesSameTypes => "branches must return the same types",
    IfCannotReturn => "if statement cannot have a result"
);
