use std::ops::{Add, Rem};

/// Spec-compliant modulo implementation
#[inline]
pub fn modulo<T>(a: T, b: T) -> T
where
    T: Add<Output = T> + Rem<Output = T> + Copy,
{
    ((a % b) + b) % b
}
