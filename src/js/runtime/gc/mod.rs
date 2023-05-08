mod handle;
mod heap;
mod heap_trait_object;
mod pointer;

pub use handle::{Handle, HandleValue, HeapPtr};
pub use heap::{Heap, HeapInfo};
pub use pointer::{Gc, GcDeref};
