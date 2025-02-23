use std::ops::{Add, Rem};

use half::f16;

/// Spec-compliant modulo implementation
#[inline]
pub fn modulo<T>(a: T, b: T) -> T
where
    T: Add<Output = T> + Rem<Output = T> + Copy,
{
    ((a % b) + b) % b
}

/// Round a value up to the nearest multiple of a given number, where given number must be a power
/// of two.
#[inline]
pub fn round_to_power_of_two(value: usize, power_of_two: usize) -> usize {
    let mask = power_of_two - 1;
    (value + mask) & !mask
}

/// The minimum f64 value that will be converted to f16::INFINITY, represented as raw bits.
const F64_TO_F16_INFINITY_THRESHOLD: u64 = (1023 + 16) << 52;

/// The minimum f64 value that will be converted to a normal f16, represented as raw bits.
const F64_TO_F16_NORMAL_THRESHOLD: u64 = (1023 - 14) << 52;

/// A magic value assisting in converting denormal f64s to f16s, represented as raw bits. Taken from
/// the DoubleToFloat16 algorithm in V8.
const F64_TO_F16_DENORMAL_MAGIC: u64 = (1023 - 14 + 52 - 10) << 52;

/// A magic value assisting in converting normal f64s to f16s, represented as raw bits. Taken from
/// the DoubleToFloat16 algorithm in V8.
const F64_TO_F16_NORMAL_MAGIC: u64 =
    (15u64.wrapping_sub(1023) << 52) + ((1 << ((52 - 10) - 1)) - 1);

/// Convert an f64 to an f16, rounding to nearest with ties rounded to even. Converts precisely
/// instead of first converting to an f32, which is the default behavior of f16::from_f64 but loses
/// precision.
///
/// Based on the DoubleToFloat16 algorithm in V8, which is itself adapted from
/// https://gist.github.com/rygorous/2156668.
pub fn f64_to_f16(value: f64) -> f16 {
    let mut input = value.to_bits();

    // Extract sign from input
    let f64_sign = input & (1 << 63);
    input ^= f64_sign;

    let result = if input > f64::INFINITY.to_bits() {
        f16::NAN.to_bits()
    } else if input >= F64_TO_F16_INFINITY_THRESHOLD {
        f16::INFINITY.to_bits()
    } else if input < F64_TO_F16_NORMAL_THRESHOLD {
        let adjusted_input = f64::from_bits(input) + f64::from_bits(F64_TO_F16_DENORMAL_MAGIC);
        (adjusted_input.to_bits() - F64_TO_F16_DENORMAL_MAGIC) as u16
    } else {
        let is_mantissa_odd = (input >> (52 - 10)) & 1;

        input += F64_TO_F16_NORMAL_MAGIC;
        input += is_mantissa_odd;

        (input >> (52 - 10)) as u16
    };

    // Convert sign bit to f16 location then reapply to value
    let f16_sign = (f64_sign >> 48) as u16;
    f16::from_bits(result | f16_sign)
}
