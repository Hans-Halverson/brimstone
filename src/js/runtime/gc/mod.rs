mod garbage_collector;
mod handle;
mod heap;
mod heap_item;
mod heap_object;
mod heap_serializer;
mod heap_trait_object;
mod pointer;

pub use garbage_collector::{GarbageCollector, GcType};
pub use handle::{
    Escapable, Handle, HandleContents, HandleScope, HandleScopeGuard, ToHandleContents,
};
pub use heap::{Heap, HeapInfo};
pub use heap_item::HeapItem;
pub use heap_object::{HeapObject, HeapVisitor, IsHeapObject};
#[allow(unused)]
pub use heap_serializer::{HeapRootsDeserializer, HeapSerializer};
pub use pointer::HeapPtr;
