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
