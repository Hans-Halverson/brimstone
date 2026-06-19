// Max safe integer has magnitude 2^53 - 1
pub const MAX_SAFE_INTEGER_F64: f64 = 9007199254740991.0;
pub const MIN_SAFE_INTEGER_F64: f64 = -9007199254740991.0;

pub const MIN_POSITIVE_SUBNORMAL_F64: f64 = 5e-324;

pub const MAX_U8_AS_F64: f64 = u8::MAX as f64;
pub const MAX_U8_PLUS_ONE_AS_F64: f64 = (u8::MAX as u16 + 1) as f64;

pub const MAX_U16_AS_F64: f64 = u16::MAX as f64;
pub const MAX_U16_PLUS_ONE_AS_F64: f64 = (u16::MAX as u32 + 1) as f64;

pub const MAX_U32_AS_F64: f64 = u32::MAX as f64;
pub const MAX_U32_PLUS_ONE_AS_F64: f64 = (u32::MAX as u64 + 1) as f64;

pub const MIN_I8_AS_F64: f64 = i8::MIN as f64;
pub const MAX_I8_AS_F64: f64 = i8::MAX as f64;

pub const MIN_I16_AS_F64: f64 = i16::MIN as f64;
pub const MAX_I16_AS_F64: f64 = i16::MAX as f64;

pub const MIN_I32_AS_F64: f64 = i32::MIN as f64;
pub const MAX_I32_AS_F64: f64 = i32::MAX as f64;

pub const MAX_I32_PLUS_ONE_AS_F64: f64 = (i32::MAX as i64 + 1) as f64;

pub const MAX_SAFE_INTEGER_U64: u64 = 9007199254740991;
