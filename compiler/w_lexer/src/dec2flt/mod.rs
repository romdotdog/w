//! Converting decimal strings into IEEE 754 binary floating point numbers.
//!
//! # Problem statement
//!
//! We are given a decimal string such as `12.34e56`. This string consists of integral (`12`),
//! fractional (`34`), and exponent (`56`) parts. All parts are optional and interpreted as zero
//! when missing.
//!
//! We seek the IEEE 754 floating point number that is closest to the exact value of the decimal
//! string. It is well-known that many decimal strings do not have terminating representations in
//! base two, so we round to 0.5 units in the last place (in other words, as well as possible).
//! Ties, decimal values exactly half-way between two consecutive floats, are resolved with the
//! half-to-even strategy, also known as banker's rounding.
//!
//! Needless to say, this is quite hard, both in terms of implementation complexity and in terms
//! of CPU cycles taken.
//!
//! # Implementation
//!
//! First, we ignore signs. Or rather, we remove it at the very beginning of the conversion
//! process and re-apply it at the very end. This is correct in all edge cases since IEEE
//! floats are symmetric around zero, negating one simply flips the first bit.
//!
//! Then we remove the decimal point by adjusting the exponent: Conceptually, `12.34e56` turns
//! into `1234e54`, which we describe with a positive integer `f = 1234` and an integer `e = 54`.
//! The `(f, e)` representation is used by almost all code past the parsing stage.
//!
//! We then try a long chain of progressively more general and expensive special cases using
//! machine-sized integers and small, fixed-sized floating point numbers (first `f32`/`f64`, then
//! a type with 64 bit significand). The extended-precision algorithm
//! uses the Eisel-Lemire algorithm, which uses a 128-bit (or 192-bit)
//! representation that can accurately and quickly compute the vast majority
//! of floats. When all these fail, we bite the bullet and resort to using
//! a large-decimal representation, shifting the digits into range, calculating
//! the upper significant bits and exactly round to the nearest representation.
//!
//! Another aspect that needs attention is the ``RawFloat`` trait by which almost all functions
//! are parametrized. One might think that it's enough to parse to `f64` and cast the result to
//! `f32`. Unfortunately this is not the world we live in, and this has nothing to do with using
//! base two or half-to-even rounding.
//!
//! Consider for example two types `d2` and `d4` representing a decimal type with two decimal
//! digits and four decimal digits each and take "0.01499" as input. Let's use half-up rounding.
//! Going directly to two decimal digits gives `0.01`, but if we round to four digits first,
//! we get `0.0150`, which is then rounded up to `0.02`. The same principle applies to other
//! operations as well, if you want 0.5 ULP accuracy you need to do *everything* in full precision
//! and round *exactly once, at the end*, by considering all truncated bits at once.
//!
//! Primarily, this module and its children implement the algorithms described in:
//! "Number Parsing at a Gigabyte per Second", available online:
//! <https://arxiv.org/abs/2101.11408>.
//!
//! # Other
//!
//! The conversion should *never* panic. There are assertions and explicit panics in the code,
//! but they should never be triggered and only serve as internal sanity checks. Any panics should
//! be considered a bug.
//!
//! There are unit tests but they are woefully inadequate at ensuring correctness, they only cover
//! a small percentage of possible errors. Far more extensive tests are located in the directory
//! `src/etc/test-float-parse` as a Python script.
//!
//! A note on integer overflow: Many parts of this file perform arithmetic with the decimal
//! exponent `e`. Primarily, we shift the decimal point around: Before the first decimal digit,
//! after the last decimal digit, and so on. This could overflow if done carelessly. We rely on
//! the parsing submodule to only hand out sufficiently small exponents, where "sufficient" means
//! "such that the exponent +/- the number of decimal digits fits into a 64 bit integer".
//! Larger exponents are accepted, but we don't do arithmetic with them, they are immediately
//! turned into {positive,negative} {zero,infinity}.

use std::cmp::Ordering;
use std::fmt;

mod common;
mod decimal;
mod fpu;
mod slow;
mod table;
// float is used in flt2dec, and all are used in unit tests.
pub mod float;
pub mod lemire;
pub mod number;
pub mod parse;

use self::common::{BiasedFp, ByteSlice};
use self::float::RawFloat;
use self::lemire::compute_float;
use self::number::Number;
use self::slow::parse_long_mantissa;
use super::{token::Token, Lexer};
use std::convert::TryFrom;

pub const POSITIVE_MIN_I64: u64 = 9_223_372_036_854_775_808;

/// Converts a `BiasedFp` to the closest machine float type.
fn biased_fp_to_float<T: RawFloat>(x: BiasedFp) -> T {
    let mut word = x.f;
    word |= (x.e as u64) << T::MANTISSA_EXPLICIT_BITS;
    T::from_u64_bits(word)
}

pub fn convert_sign_and_mantissa<'ast>(negative: bool, mantissa: u64) -> Token<'ast> {
    if negative {
        match mantissa.cmp(&POSITIVE_MIN_I64) {
            #[allow(clippy::cast_possible_wrap)]
            Ordering::Less => Token::Integer(-(mantissa as i64)),
            Ordering::Equal => Token::Integer(i64::MIN),
            Ordering::Greater => Token::Overflown,
        }
    } else if let Ok(m) = i64::try_from(mantissa) {
        Token::Integer(m)
    } else {
        Token::UInteger(mantissa)
    }
}

impl<'ast> Lexer<'ast> {
    pub(crate) fn load_number(&mut self, n: (Option<Number>, usize)) -> Option<Token<'ast>> {
        let (o, len) = n;
        self.pos += len;
        unsafe { self.step_by(len) };
        if let Some(num) = o {
            if num.exponent == 0 {
                match convert_sign_and_mantissa(num.negative, num.mantissa) {
                    Token::Overflown => {}
                    t => return Some(t),
                }
            }

            if let Some(v) = num.try_fast_path::<f64>() {
                return Some(Token::Float(v));
            }

            // If significant digits were truncated, then we can have rounding error
            // only if `mantissa + 1` produces a different result. We also avoid
            // redundantly using the Eisel-Lemire algorithm if it was unable to
            // correctly round on the first pass.
            let mut fp = compute_float::<f64>(num.exponent, num.mantissa);
            if num.many_digits
                && fp.e >= 0
                && fp != compute_float::<f64>(num.exponent, num.mantissa + 1)
            {
                fp.e = -1;
            }
            // Unable to correctly round the float using the Eisel-Lemire algorithm.
            // Fallback to a slower, but always correct algorithm.
            if fp.e < 0 {
                fp = parse_long_mantissa::<f64>(self.buffer);
            }

            let mut float = biased_fp_to_float::<f64>(fp);
            if num.negative {
                float = -float;
            }

            return Some(Token::Float(float));
        }
        None
    }
}
