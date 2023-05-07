use std::collections::HashMap;

use crate::{
    js::runtime::{
        gc::{Gc, GcDeref, Handle},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        value::SymbolValue,
        Context,
    },
    set_uninit,
};

/// 6.2.11 Private Name
/// Private names have a globally unique id and string description. String description must currently
/// be known from context so it is elided.
///
/// Always stored on stack.
pub type PrivateName = Gc<SymbolValue>;

/// A PrivateName that is stored on the heap.
pub struct HeapPrivateName {
    inner: PrivateName,
}

impl PrivateName {
    pub fn to_heap(&self) -> HeapPrivateName {
        HeapPrivateName { inner: *self }
    }

    pub fn from_heap_(heap_private_name: &HeapPrivateName) -> PrivateName {
        heap_private_name.inner
    }
}

// 9.2 Private Environment Record
#[repr(C)]
pub struct PrivateEnvironment {
    descriptor: Gc<ObjectDescriptor>,
    names: HashMap<String, HeapPrivateName>,
    outer: Option<Gc<PrivateEnvironment>>,
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

        env
    }

    pub fn outer(&self) -> Option<Gc<PrivateEnvironment>> {
        self.outer
    }

    // 9.2.1.2 ResolvePrivateIdentifier
    pub fn resolve_private_identifier<'a>(&self, name: &str) -> PrivateName {
        match self.names.get(name) {
            Some(private_name) => PrivateName::from_heap_(private_name),
            None => self.outer.unwrap().resolve_private_identifier(name),
        }
    }

    pub fn add_private_name(&mut self, cx: &mut Context, description: String) {
        let symbol_name = SymbolValue::new(cx, None);
        self.names.insert(description, symbol_name.to_heap());
    }

    pub fn has_private_name(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }

    pub fn get_private_name(&self, name: &str) -> PrivateName {
        PrivateName::from_heap_(self.names.get(name).unwrap())
    }

    #[inline]
    pub fn iter_names_gc_unsafe<F: FnMut(&String)>(&self, mut f: F) {
        for name in self.names.keys() {
            f(name)
        }
    }
}
