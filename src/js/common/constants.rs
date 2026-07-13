/// Number of bytes in a megabyte.
pub const MEGABYTE_BYTES: usize = 1024 * 1024;

/// Number of bytes in a gigabyte.
pub const GIGABYTE_BYTES: usize = 1024 * 1024 * 1024;

/// Default minimum size of the heap in bytes.
#[cfg(target_pointer_width = "64")]
pub const DEFAULT_MIN_HEAP_SIZE: usize = 64 * MEGABYTE_BYTES;
#[cfg(target_pointer_width = "32")]
pub const DEFAULT_MIN_HEAP_SIZE: usize = 16 * MEGABYTE_BYTES;

/// Default maximum size of the heap in bytes.
#[cfg(target_pointer_width = "64")]
pub const DEFAULT_MAX_HEAP_SIZE: usize = GIGABYTE_BYTES;
#[cfg(target_pointer_width = "32")]
pub const DEFAULT_MAX_HEAP_SIZE: usize = 128 * MEGABYTE_BYTES;

/// The minimum size the heap can ever be in bytes.
pub const MIN_HEAP_SIZE: usize = MEGABYTE_BYTES;

/// The maximum size the heap can ever be in bytes.
#[cfg(target_pointer_width = "64")]
pub const MAX_HEAP_SIZE: usize = 4 * GIGABYTE_BYTES;
#[cfg(target_pointer_width = "32")]
pub const MAX_HEAP_SIZE: usize = 256 * MEGABYTE_BYTES;

/// Maximum allowed string length in number of code units for strings in the JS runtime.
///
/// Also used as max allowed UTF-8 source size. This guarantees:
/// - Source positions can be represented with a u32.
/// - Every string from the source is within the runtime string length limit.
/// - On 32-bit platforms a string's widest (two-byte) byte size must fit in a usize.
#[cfg(target_pointer_width = "64")]
pub const MAX_STRING_LENGTH: u32 = u32::MAX - 2;
#[cfg(target_pointer_width = "32")]
pub const MAX_STRING_LENGTH: u32 = 1 << 28;

/// Total number of nanoseconds in a millisecond.
pub const NANOSECONDS_IN_ONE_MILLISECOND: u32 = 1_000_000;
