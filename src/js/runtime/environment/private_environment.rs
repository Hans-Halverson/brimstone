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

// A private environment for the legacy interpreter.
//
// 9.2 Private Environment Record
#[repr(C)]
pub struct LegacyPrivateEnvironment {
    descriptor: HeapPtr<ObjectDescriptor>,
    names: HeapPtr<LegacyPrivateNameMap>,
    outer: Option<HeapPtr<LegacyPrivateEnvironment>>,
}

type LegacyPrivateNameMap = BsHashMap<HeapPtr<FlatString>, HeapPrivateName>;

impl LegacyPrivateEnvironment {
    // 9.2.1.1 NewPrivateEnvironment
    pub fn new(
        cx: Context,
        outer: Option<Handle<LegacyPrivateEnvironment>>,
    ) -> Handle<LegacyPrivateEnvironment> {
        // Allocate and place behind handle before allocating environment
        let names_map =
            LegacyPrivateNameMap::new_initial(cx, ObjectKind::LegacyPrivateEnvironmentNameMap)
                .to_handle();

        let mut env = cx.alloc_uninit::<LegacyPrivateEnvironment>();

        set_uninit!(
            env.descriptor,
            cx.base_descriptors
                .get(ObjectKind::LegacyPrivateEnvironment)
        );
        set_uninit!(env.names, names_map.get_());
        set_uninit!(env.outer, outer.map(|p| p.get_()));

        env.to_handle()
    }

    #[inline]
    pub fn outer_ptr(&self) -> Option<HeapPtr<LegacyPrivateEnvironment>> {
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

impl Handle<LegacyPrivateEnvironment> {
    fn names_field(&self) -> LegacyPrivateEnvironmentNamesField {
        LegacyPrivateEnvironmentNamesField(*self)
    }

    pub fn add_private_name(&mut self, cx: Context, name: Handle<StringValue>) {
        let symbol_name = SymbolValue::new(cx, Some(name), /* is_private */ true);
        let flat_name = name.flatten();
        self.names_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(flat_name.get_(), symbol_name.get_());
    }
}

pub struct LegacyPrivateEnvironmentNamesField(Handle<LegacyPrivateEnvironment>);

impl BsHashMapField<HeapPtr<FlatString>, HeapPrivateName> for LegacyPrivateEnvironmentNamesField {
    fn new(&self, cx: Context, capacity: usize) -> HeapPtr<LegacyPrivateNameMap> {
        LegacyPrivateNameMap::new(cx, ObjectKind::LegacyPrivateEnvironmentNameMap, capacity)
    }

    fn get(&self, _: Context) -> HeapPtr<LegacyPrivateNameMap> {
        self.0.names
    }

    fn set(&mut self, _: Context, map: HeapPtr<LegacyPrivateNameMap>) {
        self.0.names = map;
    }
}

impl HeapObject for HeapPtr<LegacyPrivateEnvironment> {
    fn byte_size(&self) -> usize {
        size_of::<LegacyPrivateEnvironment>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.names);
        visitor.visit_pointer_opt(&mut self.outer);
    }
}

impl LegacyPrivateEnvironmentNamesField {
    pub fn byte_size(map: &HeapPtr<LegacyPrivateNameMap>) -> usize {
        LegacyPrivateNameMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<LegacyPrivateNameMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (key, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(key);
            visitor.visit_pointer(value);
        }
    }
}
