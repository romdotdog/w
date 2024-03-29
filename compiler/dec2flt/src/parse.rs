//! Functions to parse floating-point numbers.

use super::common::{is_8digits, AsciiStr, ByteSlice};
use super::number::Number;

const MIN_19DIGIT_INT: u64 = 100_0000_0000_0000_0000;

/// Parse 8 digits, loaded as bytes in little-endian order.
///
/// This uses the trick where every digit is in [0x030, 0x39],
/// and therefore can be parsed in 3 multiplications, much
/// faster than the normal 8.
///
/// This is based off the algorithm described in "Fast numeric string to
/// int", available here: <https://johnnylee-sde.github.io/Fast-numeric-string-to-int/>.
fn parse_8digits(mut v: u64) -> u64 {
    const MASK: u64 = 0x0000_00FF_0000_00FF;
    const MUL1: u64 = 0x000F_4240_0000_0064;
    const MUL2: u64 = 0x0000_2710_0000_0001;
    v -= 0x3030_3030_3030_3030;
    v = (v * 10) + (v >> 8); // will not overflow, fits in 63 bits
    let v1 = (v & MASK).wrapping_mul(MUL1);
    let v2 = ((v >> 16) & MASK).wrapping_mul(MUL2);
    ((v1.wrapping_add(v2) >> 32) as u32) as u64
}

/// Parse digits until a non-digit character is found.
fn try_parse_digits(s: &mut AsciiStr<'_>, x: &mut u64) {
    // may cause overflows, to be handled later
    s.parse_digits(|digit| {
        *x = x.wrapping_mul(10).wrapping_add(digit as _);
    });
}

/// Parse up to 19 digits (the max that can be stored in a 64-bit integer).
fn try_parse_19digits(s: &mut AsciiStr<'_>, x: &mut u64) {
    while *x < MIN_19DIGIT_INT {
        if let Some(&c) = s.as_ref().first() {
            let digit = c.wrapping_sub(b'0');
            if digit < 10 {
                *x = (*x * 10) + digit as u64; // no overflows here
                                               // SAFETY: cannot be empty
                unsafe {
                    s.step();
                }
            } else {
                break;
            }
        } else {
            break;
        }
    }
}

/// Try to parse 8 digits at a time, using an optimized algorithm.
fn try_parse_8digits(s: &mut AsciiStr<'_>, x: &mut u64) {
    // may cause overflows, to be handled later
    if let Some(v) = s.read_u64() {
        if is_8digits(v) {
            *x = x.wrapping_mul(1_0000_0000).wrapping_add(parse_8digits(v));
            // SAFETY: already ensured the buffer was >= 8 bytes in read_u64.
            unsafe {
                s.step_by(8);
            }
            if let Some(v) = s.read_u64() {
                if is_8digits(v) {
                    *x = x.wrapping_mul(1_0000_0000).wrapping_add(parse_8digits(v));
                    // SAFETY: already ensured the buffer was >= 8 bytes in try_read_u64.
                    unsafe {
                        s.step_by(8);
                    }
                }
            }
        }
    }
}

/// Parse the scientific notation component of a float.
fn parse_scientific(s: &mut AsciiStr<'_>) -> Option<i64> {
    let mut exponent = 0_i64;
    let mut negative = false;
    if let Some(&c) = s.as_ref().get(0) {
        negative = c == b'-';
        if c == b'-' || c == b'+' {
            // SAFETY: s cannot be empty
            unsafe {
                s.step();
            }
        }
    }
    if s.first_isdigit() {
        s.parse_digits(|digit| {
            // no overflows here, saturate well before overflow
            if exponent < 0x10000 {
                exponent = 10 * exponent + digit as i64;
            }
        });
        if negative {
            Some(-exponent)
        } else {
            Some(exponent)
        }
    } else {
        None
    }
}

/// Parse a partial, non-special floating point number.
///
/// This creates a representation of the float as the
/// significant digits and the decimal exponent.
fn finish_number(
    mut s: AsciiStr,
    negative: bool,
    mut mantissa: u64,
    mut exponent: i64,
    start: AsciiStr,
    int_end: AsciiStr,
    mut n_digits: isize,
) -> (Option<Number>, usize) {
    // handle scientific format
    let mut exp_number = 0_i64;
    if s.first_is2(b'e', b'E') {
        // SAFETY: s cannot be empty
        unsafe {
            s.step();
        }
        // If None, we have no trailing digits after exponent, or an invalid float.
        exp_number = match parse_scientific(&mut s) {
            Some(n) => n,
            None => {
                let len = s.offset_from(&start) as _;
                return (None, len);
            }
        };
        exponent += exp_number;
    }

    let len = s.offset_from(&start) as _;

    // handle uncommon case with many digits
    if n_digits <= 19 {
        return (
            Some(Number {
                exponent,
                mantissa,
                negative,
                many_digits: false,
            }),
            len,
        );
    }

    n_digits -= 19;
    let mut many_digits = false;
    let mut p = start;
    while p.first_is2(b'0', b'.') {
        // SAFETY: p cannot be empty due to first_is2
        unsafe {
            // '0' = b'.' + 2
            n_digits -= p.first_unchecked().saturating_sub(b'0' - 1) as isize;
            p.step();
        }
    }
    if n_digits > 0 {
        // at this point we have more than 19 significant digits, let's try again
        many_digits = true;
        mantissa = 0;
        let mut s = start;
        try_parse_19digits(&mut s, &mut mantissa);
        exponent = if mantissa >= MIN_19DIGIT_INT {
            // big int
            int_end.offset_from(&s)
        } else {
            // SAFETY: the next byte must be present and be '.'
            // We know this is true because we had more than 19
            // digits previously, so we overflowed a 64-bit integer,
            // but parsing only the integral digits produced less
            // than 19 digits. That means we must have a decimal
            // point, and at least 1 fractional digit.
            unsafe { s.step() };
            let before = s;
            try_parse_19digits(&mut s, &mut mantissa);
            -s.offset_from(&before)
        } as i64;
        // add back the explicit part
        exponent += exp_number;
    }

    (
        Some(Number {
            exponent,
            mantissa,
            negative,
            many_digits,
        }),
        len,
    )
}

fn parse_after_period(s: &mut AsciiStr, mantissa: &mut u64) -> isize {
    // SAFETY: s cannot be empty due to first_is
    let before = s.clone();
    try_parse_8digits(s, mantissa);
    try_parse_digits(s, mantissa);
    s.offset_from(&before)
}

pub fn parse_decimal(s: &[u8], negative: bool) -> (Option<Number>, usize) {
    let mut s = AsciiStr::new(s);
    let start = s;
    debug_assert!(!s.is_empty());

    unsafe { s.step() };
    let mut mantissa = 0_u64;
    let n_after_dot = parse_after_period(&mut s, &mut mantissa);
    let exponent = -(n_after_dot as i64);
    finish_number(s, negative, mantissa, exponent, start, start, n_after_dot)
}

pub fn parse_number(s: &[u8], negative: bool) -> (Option<Number>, usize) {
    let mut s = AsciiStr::new(s);
    let start = s;
    debug_assert!(!s.is_empty());

    // parse initial digits before dot
    let mut mantissa = 0_u64;
    let digits_start = s;
    try_parse_digits(&mut s, &mut mantissa);
    let int_end = s;
    let mut n_digits = s.offset_from(&digits_start);

    let mut n_after_dot = 0;
    let mut exponent = 0_i64;
    if s.first_is(b'.') {
        unsafe { s.step() };
        n_after_dot = parse_after_period(&mut s, &mut mantissa);
        exponent = -(n_after_dot as i64);
    }

    n_digits += n_after_dot;
    finish_number(s, negative, mantissa, exponent, start, int_end, n_digits)
}
