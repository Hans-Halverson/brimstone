use std::collections::HashMap;

use crate::{
    js::runtime::{
        gc::{GcDeref, Handle},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        value::SymbolValue,
        Context, HeapPtr,
    },
    set_uninit,
};

/// 6.2.11 Private Name
/// Private names have a globally unique id and string description. String description must currently
/// be known from context so it is elided.
///
/// Always stored on stack.
pub type PrivateName = Handle<SymbolValue>;

/// A PrivateName that is stored on the heap.
pub type HeapPrivateName = HeapPtr<SymbolValue>;

// 9.2 Private Environment Record
#[repr(C)]
pub struct PrivateEnvironment {
    descriptor: HeapPtr<ObjectDescriptor>,
    names: HashMap<String, HeapPrivateName>,
    outer: Option<HeapPtr<PrivateEnvironment>>,
}

impl GcDeref for PrivateEnvironment {}

impl PrivateEnvironment {
    // 9.2.1.1 NewPrivateEnvironment
    pub fn new(
        cx: &mut Context,
        outer: Option<Handle<PrivateEnvironment>>,
    ) -> Handle<PrivateEnvironment> {
        let mut env = cx.heap.alloc_uninit::<PrivateEnvironment>();

        set_uninit!(env.descriptor, cx.base_descriptors.get(ObjectKind::PrivateEnvironment));
        set_uninit!(env.names, HashMap::new());
        set_uninit!(env.outer, outer.map(|p| p.get_()));

        Handle::from_heap(env)
    }

    pub fn outer_ptr(&self) -> Option<HeapPtr<PrivateEnvironment>> {
        self.outer
    }

    // 9.2.1.2 ResolvePrivateIdentifier
    pub fn resolve_private_identifier<'a>(&self, name: &str) -> PrivateName {
        match self.names.get(name) {
            Some(private_name) => PrivateName::from_heap(*private_name),
            None => self.outer.unwrap().resolve_private_identifier(name),
        }
    }

    pub fn has_private_name(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }

    pub fn get_private_name(&self, name: &str) -> PrivateName {
        PrivateName::from_heap(*self.names.get(name).unwrap())
    }

    #[inline]
    pub fn iter_names_gc_unsafe<F: FnMut(&String)>(&self, mut f: F) {
        for name in self.names.keys() {
            f(name)
        }
    }
}

impl Handle<PrivateEnvironment> {
    pub fn add_private_name(&mut self, cx: &mut Context, description: String) {
        let symbol_name = SymbolValue::new(cx, None);
        self.names.insert(description, symbol_name.get_());
    }
}
