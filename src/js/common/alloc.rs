#[cfg(all(feature = "jemalloc", not(feature = "no_jemalloc"), not(target_env = "msvc")))]
mod jemalloc {
    use tikv_jemallocator::Jemalloc;

    #[global_allocator]
    static GLOBAL: Jemalloc = Jemalloc;
}

use allocator_api2::alloc::Allocator;

pub use allocator_api2::{vec, vec::Vec};

pub fn slice_to_alloc_vec<T: Clone, A: Allocator>(slice: &[T], alloc: A) -> Vec<T, A> {
    let mut vec = Vec::with_capacity_in(slice.len(), alloc);
    for element in slice {
        vec.push(element.clone());
    }

    vec
}
