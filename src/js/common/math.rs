use std::ops::{Add, Rem};

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
