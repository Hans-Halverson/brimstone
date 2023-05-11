mod handle;
mod heap;
mod heap_trait_object;
mod pointer;

pub use handle::Handle;
pub use heap::{Heap, HeapInfo, IsHeapObject};
pub use pointer::HeapPtr;
