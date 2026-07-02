use crate::{
    impl_vec_instance,
    runtime::{
        Context, HeapPtr, Value,
        alloc_error::AllocResult,
        collections::VecInstance,
        gc::{HeapItem, HeapVisitor},
    },
    set_uninit,
};

// A growable array of weakly held values.
//
// Garbage collector is aware of this type and compresses it in place during collection.
impl_vec_instance!(WeakValueVec, Value, WeakValueVecExtraStorage);

/// Extra header data for a weak value vec, storing the intrusive list of weak vecs for GC.
pub struct WeakValueVecExtraStorage {
    /// Holds the address of the next weak vec that has been visited during garbage collection.
    /// Unused outside of garbage collection.
    next_weak_vec: Option<HeapPtr<WeakValueVec>>,
}

impl WeakValueVec {
    /// Create a new WeakValueVec with the given capacity.
    ///
    /// This is the only WeakValueVec creation function that should be used.
    #[allow(unused)]
    pub fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        let mut vec = <Self as VecInstance>::new(cx, capacity)?;

        set_uninit!(vec.extra_data_mut().next_weak_vec, None);

        Ok(vec)
    }

    pub fn next_weak_vec(&self) -> Option<HeapPtr<WeakValueVec>> {
        self.extra_data().next_weak_vec
    }

    pub fn set_next_weak_vec(&mut self, next_weak_vec: Option<HeapPtr<WeakValueVec>>) {
        self.extra_data_mut().next_weak_vec = next_weak_vec;
    }
}

impl HeapItem for WeakValueVec {
    fn byte_size(weak_vec: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(weak_vec.capacity())
    }

    fn visit_pointers(mut weak_vec: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        weak_vec.visit_vec_pointers(visitor);

        for weak_value in weak_vec.as_mut_slice() {
            visitor.visit_weak_value(weak_value);
        }
    }
}
