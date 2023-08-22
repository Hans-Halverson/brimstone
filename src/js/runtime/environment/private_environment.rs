use std::mem::size_of;

use crate::{
    js::runtime::{
        collections::{BsHashMap, BsHashMapField},
        gc::{Handle, HeapObject, HeapVisitor},
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

impl PrivateEnvironment {
    // 9.2.1.1 NewPrivateEnvironment
    pub fn new(
        cx: &mut Context,
        outer: Option<Handle<PrivateEnvironment>>,
    ) -> Handle<PrivateEnvironment> {
        // Allocate and place behind handle before allocating environment
        let names_map =
            PrivateNameMap::new_initial(cx, ObjectKind::PrivateEnvironmentNameMap).to_handle();

        let mut env = cx.heap.alloc_uninit::<PrivateEnvironment>();

        set_uninit!(env.descriptor, cx.base_descriptors.get(ObjectKind::PrivateEnvironment));
        set_uninit!(env.names, names_map.get_());
        set_uninit!(env.outer, outer.map(|p| p.get_()));

        env.to_handle()
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
    fn names_field(&self) -> PrivateEnvironmentNamesField {
        PrivateEnvironmentNamesField(*self)
    }

    pub fn add_private_name(&mut self, cx: &mut Context, description: Handle<StringValue>) {
        let symbol_name = SymbolValue::new(cx, None);
        let flat_description = description.flatten();
        self.names_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(flat_description.get_(), symbol_name.get_());
    }
}

pub struct PrivateEnvironmentNamesField(Handle<PrivateEnvironment>);

impl BsHashMapField<HeapPtr<FlatString>, HeapPrivateName> for PrivateEnvironmentNamesField {
    fn new(&self, cx: &mut Context, capacity: usize) -> HeapPtr<PrivateNameMap> {
        PrivateNameMap::new(cx, ObjectKind::PrivateEnvironmentNameMap, capacity)
    }

    fn get(&self, _: &mut Context) -> HeapPtr<PrivateNameMap> {
        self.0.names
    }

    fn set(&mut self, _: &mut Context, map: HeapPtr<PrivateNameMap>) {
        self.0.names = map;
    }
}

impl HeapObject for HeapPtr<PrivateEnvironment> {
    fn byte_size(&self) -> usize {
        size_of::<PrivateEnvironment>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.names);
        visitor.visit_pointer_opt(&mut self.outer);
    }
}

impl PrivateEnvironmentNamesField {
    pub fn byte_size(map: &HeapPtr<PrivateNameMap>) -> usize {
        PrivateNameMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<PrivateNameMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (key, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(key);
            visitor.visit_pointer(value);
        }
    }
}
