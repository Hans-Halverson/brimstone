use num_traits::AsPrimitive;

/// Utility trait for primitive Rust numeric types. Marks additional trait bounds as well as
/// adds const min/max values and const type identification.
pub trait Numeric:
    AsPrimitive<f64>
    + AsPrimitive<i32>
    + AsPrimitive<u32>
    + AsPrimitive<u64>
    + AsPrimitive<i64>
    + AsPrimitive<usize>
    + AsPrimitive<isize>
{
    const CONST_MIN: Self;
    const CONST_MAX: Self;

    const MAX_AS_F64: f64;
    const MIN_AS_F64: f64;

    const IS_I8: bool;
    const IS_U8: bool;
    const IS_I16: bool;
    const IS_U16: bool;
    const IS_I32: bool;
    const IS_U32: bool;
    const IS_I64: bool;
    const IS_U64: bool;
    const IS_I128: bool;
    const IS_U128: bool;
    const IS_ISIZE: bool;
    const IS_USIZE: bool;
    const IS_F32: bool;
    const IS_F64: bool;

    /// Numbers where all values can be losslessly represented as a smi (aka i32).
    const IS_SAFE_SMI: bool;
}

macro_rules! impl_numeric {
    ($t:ty, $kind:path) => {
        impl Numeric for $t {
            const CONST_MIN: Self = <$t>::MIN;
            const CONST_MAX: Self = <$t>::MAX;

            const MAX_AS_F64: f64 = <$t>::MAX as f64;
            const MIN_AS_F64: f64 = <$t>::MIN as f64;

            const IS_I8: bool = matches!($kind, NumberKind::I8);
            const IS_U8: bool = matches!($kind, NumberKind::U8);
            const IS_I16: bool = matches!($kind, NumberKind::I16);
            const IS_U16: bool = matches!($kind, NumberKind::U16);
            const IS_I32: bool = matches!($kind, NumberKind::I32);
            const IS_U32: bool = matches!($kind, NumberKind::U32);
            const IS_I64: bool = matches!($kind, NumberKind::I64);
            const IS_U64: bool = matches!($kind, NumberKind::U64);
            const IS_I128: bool = matches!($kind, NumberKind::I128);
            const IS_U128: bool = matches!($kind, NumberKind::U128);
            const IS_ISIZE: bool = matches!($kind, NumberKind::Isize);
            const IS_USIZE: bool = matches!($kind, NumberKind::Usize);
            const IS_F32: bool = matches!($kind, NumberKind::F32);
            const IS_F64: bool = matches!($kind, NumberKind::F64);

            const IS_SAFE_SMI: bool = matches!(
                $kind,
                NumberKind::I8
                    | NumberKind::U8
                    | NumberKind::I16
                    | NumberKind::U16
                    | NumberKind::I32
            );
        }
    };
}

impl_numeric!(i8, NumberKind::I8);
impl_numeric!(u8, NumberKind::U8);
impl_numeric!(i16, NumberKind::I16);
impl_numeric!(u16, NumberKind::U16);
impl_numeric!(i32, NumberKind::I32);
impl_numeric!(u32, NumberKind::U32);
impl_numeric!(i64, NumberKind::I64);
impl_numeric!(u64, NumberKind::U64);
impl_numeric!(i128, NumberKind::I128);
impl_numeric!(u128, NumberKind::U128);
impl_numeric!(isize, NumberKind::Isize);
impl_numeric!(usize, NumberKind::Usize);
impl_numeric!(f32, NumberKind::F32);
impl_numeric!(f64, NumberKind::F64);

/// Each numeric type in an enum that can be matched at const time.
enum NumberKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    Isize,
    Usize,
    F32,
    F64,
}

// Max safe integer has magnitude 2^53 - 1
pub const MAX_SAFE_INTEGER_F64: f64 = 9007199254740991.0;
pub const MIN_SAFE_INTEGER_F64: f64 = -9007199254740991.0;

pub const MIN_POSITIVE_SUBNORMAL_F64: f64 = 5e-324;

pub const MAX_U8_PLUS_ONE_AS_F64: f64 = (u8::MAX as u16 + 1) as f64;
pub const MAX_U16_PLUS_ONE_AS_F64: f64 = (u16::MAX as u32 + 1) as f64;
pub const MAX_U32_PLUS_ONE_AS_F64: f64 = (u32::MAX as u64 + 1) as f64;
pub const MAX_I32_PLUS_ONE_AS_F64: f64 = (i32::MAX as i64 + 1) as f64;

pub const MAX_SAFE_INTEGER_U64: u64 = 9007199254740991;
