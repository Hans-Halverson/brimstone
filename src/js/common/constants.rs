/// Number of bytes in a megabyte.
pub const MEGABYTE_BYTES: usize = 1024 * 1024;

/// Number of bytes in a gigabyte.
pub const GIGABYTE_BYTES: usize = 1024 * 1024 * 1024;

/// Default minimum size of the heap in bytes.
pub const DEFAULT_MIN_HEAP_SIZE: usize = 64 * MEGABYTE_BYTES;

/// Default maximum size of the heap in bytes.
pub const DEFAULT_MAX_HEAP_SIZE: usize = GIGABYTE_BYTES;

/// The minimum size the heap can ever be in bytes.
pub const MIN_HEAP_SIZE: usize = MEGABYTE_BYTES;

/// The maximum size the heap can ever be in bytes.
pub const MAX_HEAP_SIZE: usize = 4 * GIGABYTE_BYTES;

/// Maximum allowed string length in number of code units for strings in the JS runtime.
///
/// Also used as max allowed UTF-8 source size. This guarantees:
/// - Source positions can be represented with a u32.
/// - Every string from the source is within the runtime string length limit.
///
pub const MAX_STRING_LENGTH: u32 = u32::MAX - 2;
