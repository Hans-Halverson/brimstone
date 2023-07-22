#[macro_export]
macro_rules! heap_trait_object {
    ($trait:ident, $stack_object:ident, $heap_object:ident, $into_dyn:ident) => {
        /// A custom trait object to the heap, containing both a pointer to an object on the heap along with
        /// the object's vtable for the trait.
        ///
        /// Differs from a true rust trait object in that the data pointer contains the receiver value
        /// directly instead of a pointer to the receiver.
        #[derive(Clone, Copy)]
        #[repr(C)]
        pub struct $stack_object {
            pub data: $crate::js::runtime::Handle<$crate::js::runtime::object_value::ObjectValue>,
            vtable: *const (),
        }

        /// The same custom trait object, but stored on the heap.
        #[derive(Clone, Copy)]
        #[repr(C)]
        pub struct $heap_object {
            data: $crate::js::runtime::HeapPtr<$crate::js::runtime::object_value::ObjectValue>,
            vtable: *const (),
        }

        impl<T> $crate::js::runtime::Handle<T>
        where
            $crate::js::runtime::Handle<T>: $trait,
        {
            #[inline]
            pub fn $into_dyn(&self) -> $stack_object
            where
                Self: Sized,
            {
                let vtable = extract_trait_vtable::<Self>();
                $stack_object { data: self.cast(), vtable }
            }
        }

        impl $heap_object {
            pub fn uninit() -> $heap_object {
                $heap_object {
                    data: $crate::js::runtime::HeapPtr::uninit(),
                    vtable: std::ptr::null(),
                }
            }
        }

        impl $stack_object {
            pub fn ptr_eq(&self, other: &Self) -> bool {
                self.data.get_().ptr_eq(&other.data.get_())
            }

            #[inline]
            pub fn to_heap(&self) -> $heap_object {
                $heap_object { data: self.data.get_(), vtable: self.vtable }
            }

            #[inline]
            pub fn from_heap(heap_object: &$heap_object) -> $stack_object {
                $stack_object {
                    data: heap_object.data.to_handle(),
                    vtable: heap_object.vtable,
                }
            }
        }

        #[repr(C)]
        struct RustTraitObject {
            data: *const (),
            vtable: *const (),
        }

        // Implicitly deref to a true rust trait object by constructing a true trait object with a pointer
        // to the receiver value, with the same vtable.
        impl std::ops::Deref for $stack_object {
            type Target = dyn $trait;

            fn deref(&self) -> &Self::Target {
                let data = &self.data as *const _ as *const ();
                let trait_object = RustTraitObject { data, vtable: self.vtable };
                unsafe { std::mem::transmute::<RustTraitObject, &dyn $trait>(trait_object) }
            }
        }

        impl std::ops::DerefMut for $stack_object {
            fn deref_mut(&mut self) -> &mut Self::Target {
                let data = &self.data as *const _ as *const ();
                let trait_object = RustTraitObject { data, vtable: self.vtable };
                unsafe { std::mem::transmute::<RustTraitObject, &mut dyn $trait>(trait_object) }
            }
        }

        /// Compile time shenanigans to extract the trait object vtable for a particular type that
        /// implements the trait so that we can construct our own trait objects manually.
        const fn extract_trait_vtable<T: $trait>() -> *const () {
            unsafe {
                let example_ptr: *const T = std::ptr::NonNull::dangling().as_ptr();
                let example_trait_object: *const dyn $trait = example_ptr;
                let trait_object =
                    std::mem::transmute::<*const dyn $trait, $stack_object>(example_trait_object);

                trait_object.vtable
            }
        }
    };
}
