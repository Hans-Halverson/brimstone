use crate::{
    js::runtime::{
        collections::{BsHashMap, BsHashMapContainer},
        gc::{Handle, IsHeapObject},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        string_value::{FlatString, StringValue},
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
    names: HeapPtr<PrivateNameMap>,
    outer: Option<HeapPtr<PrivateEnvironment>>,
}

type PrivateNameMap = BsHashMap<HeapPtr<FlatString>, HeapPrivateName>;

impl IsHeapObject for PrivateEnvironment {}

impl PrivateEnvironment {
    // 9.2.1.1 NewPrivateEnvironment
    pub fn new(
        cx: &mut Context,
        outer: Option<Handle<PrivateEnvironment>>,
    ) -> Handle<PrivateEnvironment> {
        // Allocate and place behind handle before allocating environment
        let names_map = Self::new_names_map(cx);

        let mut env = cx.heap.alloc_uninit::<PrivateEnvironment>();

        set_uninit!(env.descriptor, cx.base_descriptors.get(ObjectKind::PrivateEnvironment));
        set_uninit!(env.names, names_map);
        set_uninit!(env.outer, outer.map(|p| p.get_()));

        env.to_handle()
    }

    fn new_names_map(cx: &mut Context) -> HeapPtr<PrivateNameMap> {
        Self::new_map(cx)
    }

    pub fn outer_ptr(&self) -> Option<HeapPtr<PrivateEnvironment>> {
        self.outer
    }

    // 9.2.1.2 ResolvePrivateIdentifier
    pub fn resolve_private_identifier<'a>(&self, name: Handle<StringValue>) -> PrivateName {
        match self.names.get(&name.flatten().get_()) {
            Some(private_name) => private_name.to_handle(),
            None => self.outer.unwrap().resolve_private_identifier(name),
        }
    }

    pub fn has_private_name(&self, name: Handle<StringValue>) -> bool {
        self.names.contains_key(&name.flatten().get_())
    }

    pub fn get_private_name(&self, name: Handle<StringValue>) -> PrivateName {
        self.names.get(&name.flatten().get_()).unwrap().to_handle()
    }

    #[inline]
    pub fn iter_names_gc_unsafe<F: FnMut(HeapPtr<FlatString>)>(&self, mut f: F) {
        for name in self.names.keys_gc_unsafe() {
            f(name)
        }
    }
}

impl Handle<PrivateEnvironment> {
    pub fn add_private_name(&mut self, cx: &mut Context, description: Handle<StringValue>) {
        let map_container = self.clone();

        let symbol_name = SymbolValue::new(cx, None);
        self.names.to_handle().insert(
            cx,
            map_container,
            description.flatten().get_(),
            symbol_name.get_(),
        );
    }
}

impl BsHashMapContainer<HeapPtr<FlatString>, HeapPrivateName> for PrivateEnvironment {
    const KIND: ObjectKind = ObjectKind::PrivateEnvironmentNameMap;

    fn set_map(&mut self, map: HeapPtr<PrivateNameMap>) {
        self.names = map;
    }
}
