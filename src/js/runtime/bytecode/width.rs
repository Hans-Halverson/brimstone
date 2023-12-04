use std::{fmt, mem::size_of};

pub trait Width {
    /// Representative unsigned type for this width.
    type UInt: UnsignedWidthRepr<Self::SInt>;

    /// Representative signed type for this width.
    type SInt: SignedWidthRepr<Self::UInt>;

    const ENUM: WidthEnum;

    const NUM_BYTES: usize = size_of::<Self::UInt>();

    const SIGNED_MAX: isize = Self::SInt::MAX;
    const SIGNED_MIN: isize = Self::SInt::MIN;
    const UNSIGNED_MAX: usize = Self::UInt::MAX;
    const UNSIGNED_MIN: usize = Self::UInt::MIN;

    fn write_into(bytes: &mut Vec<u8>, value: Self::UInt);
}

pub trait UnsignedWidthRepr<S: SignedWidthRepr<Self>>: Copy + fmt::Display + PartialEq {
    const MAX: usize;
    const MIN: usize;

    fn to_usize(&self) -> usize;

    fn from_usize(value: usize) -> Self;

    fn as_signed(&self) -> S;
}

pub trait SignedWidthRepr<U: UnsignedWidthRepr<Self>>: Copy + fmt::Display + PartialEq {
    const MAX: isize;
    const MIN: isize;

    fn to_i32(&self) -> i32;

    fn to_isize(&self) -> isize;

    fn from_isize(value: isize) -> Self;

    fn as_unsigned(&self) -> U;
}

pub struct Narrow;

impl Width for Narrow {
    type UInt = u8;
    type SInt = i8;

    const ENUM: WidthEnum = WidthEnum::Narrow;

    fn write_into(bytes: &mut Vec<u8>, value: u8) {
        bytes.push(value);
    }
}

pub struct Wide;

impl Width for Wide {
    type UInt = u16;
    type SInt = i16;

    const ENUM: WidthEnum = WidthEnum::Wide;

    fn write_into(bytes: &mut Vec<u8>, value: u16) {
        bytes.extend_from_slice(&value.to_ne_bytes());
    }
}

pub struct ExtraWide;

impl Width for ExtraWide {
    type UInt = u32;
    type SInt = i32;

    const ENUM: WidthEnum = WidthEnum::ExtraWide;

    fn write_into(bytes: &mut Vec<u8>, value: u32) {
        bytes.extend_from_slice(&value.to_ne_bytes());
    }
}

macro_rules! impl_width_repr_pair {
    ($unsigned:ident, $signed:ident) => {
        impl UnsignedWidthRepr<$signed> for $unsigned {
            const MAX: usize = $unsigned::MAX as usize;
            const MIN: usize = $unsigned::MIN as usize;

            #[inline]
            fn to_usize(&self) -> usize {
                *self as usize
            }

            #[inline]
            fn from_usize(value: usize) -> $unsigned {
                value as $unsigned
            }

            #[inline]
            fn as_signed(&self) -> $signed {
                *self as $signed
            }
        }

        impl SignedWidthRepr<$unsigned> for $signed {
            const MAX: isize = $signed::MAX as isize;
            const MIN: isize = $signed::MIN as isize;

            #[inline]
            fn to_i32(&self) -> i32 {
                *self as i32
            }

            #[inline]
            fn to_isize(&self) -> isize {
                *self as isize
            }

            #[inline]
            fn from_isize(value: isize) -> $signed {
                value as $signed
            }

            #[inline]
            fn as_unsigned(&self) -> $unsigned {
                *self as $unsigned
            }
        }
    };
}

impl_width_repr_pair!(u8, i8);
impl_width_repr_pair!(u16, i16);
impl_width_repr_pair!(u32, i32);

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
pub enum WidthEnum {
    Narrow,
    Wide,
    ExtraWide,
}

impl WidthEnum {
    pub fn num_bytes(&self) -> usize {
        match self {
            WidthEnum::Narrow => Narrow::NUM_BYTES,
            WidthEnum::Wide => Wide::NUM_BYTES,
            WidthEnum::ExtraWide => ExtraWide::NUM_BYTES,
        }
    }
}
