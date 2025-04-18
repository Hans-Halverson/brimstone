use std::mem::size_of;

use brimstone_macros::wrap_ordinary_object;

use crate::{
    extend_object, field_offset, must,
    parser::scope_tree::SHADOWED_SCOPE_SLOT_NAME,
    runtime::{interned_strings::InternedStrings, ordinary_object::object_create_with_size},
    set_uninit,
};

use super::{
    abstract_operations::{create_data_property_or_throw, define_property_or_throw},
    bytecode::function::Closure,
    collections::InlineArray,
    gc::{Handle, HeapObject, HeapVisitor},
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::{
        object_create, ordinary_define_own_property, ordinary_delete, ordinary_get,
        ordinary_get_own_property, ordinary_set,
    },
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    rust_vtables::extract_virtual_object_vtable,
    scope::Scope,
    type_utilities::same_object_value_handles,
    Context, EvalResult, HeapPtr, Value,
};

// An unmapped arguments that is identical to an ordinary object, but has an arguments object
// descriptor. This emulates an ordinary object with a [[ParameterMap]] slot described in spec.
extend_object! {
    pub struct UnmappedArgumentsObject {}
}

impl UnmappedArgumentsObject {
    pub fn new(cx: Context) -> Handle<ObjectValue> {
        let object = object_create::<ObjectValue>(
            cx,
            ObjectKind::UnmappedArgumentsObject,
            Intrinsic::ObjectPrototype,
        );

        object.to_handle()
    }
}

// A mapped arguments exotic argument used in the bytecode VM. Contains a reference to the scope
// where the arguments are stored so that they can be referenced directly.
//
// Only some parameters are mapped, and this can change dynamically due to user action. Keep an
// inline array of booleans noting which parameters are mapped to the scope.
//
// Arguments Exotic Objects (https://tc39.es/ecma262/#sec-arguments-exotic-objects)
extend_object! {
    pub struct MappedArgumentsObject {
        // Scope where the arguments are stored.
        scope: HeapPtr<Scope>,
        // Whether each parameter is mapped to the value in the scope.
        mapped_parameters: InlineArray<bool>,
    }
}

impl MappedArgumentsObject {
    pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();

    pub fn new(
        cx: Context,
        callee: Handle<Closure>,
        arguments: &[Handle<Value>],
        scope: Handle<Scope>,
        num_parameters: usize,
    ) -> Handle<MappedArgumentsObject> {
        let shadowed_name = InternedStrings::alloc_wtf8_str(cx, &SHADOWED_SCOPE_SLOT_NAME);

        let size = Self::calculate_size_in_bytes(num_parameters);
        let mut object = object_create_with_size::<MappedArgumentsObject>(
            cx,
            size,
            ObjectKind::MappedArgumentsObject,
            Intrinsic::ObjectPrototype,
        );

        set_uninit!(object.scope, *scope);

        // An parameter is mapped if it has not been shadowed, which we know due to the special
        // shadowed scope slot name.
        let scope_names = scope.scope_names_ptr();

        object.mapped_parameters.init_with_uninit(num_parameters);
        for i in 0..num_parameters {
            let is_mapped = !scope_names.get_slot_name(i).ptr_eq(&*shadowed_name);
            object.mapped_parameters.as_mut_slice()[i] = is_mapped;
        }

        let object = object.to_handle();

        Self::init_properties(cx, object, callee, arguments);

        object
    }

    fn init_properties(
        cx: Context,
        object: Handle<MappedArgumentsObject>,
        callee: Handle<Closure>,
        arguments: &[Handle<Value>],
    ) {
        // Property key is shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        // Set indexed argument properties
        for (i, argument) in arguments.iter().enumerate() {
            index_key.replace(PropertyKey::array_index(cx, i as u32));
            must!(create_data_property_or_throw(cx, object.into(), index_key, *argument));
        }

        // Set length property
        let length_value = Value::from(arguments.len()).to_handle(cx);
        let length_desc = PropertyDescriptor::data(length_value, true, false, true);
        must!(define_property_or_throw(cx, object.into(), cx.names.length(), length_desc));

        // Set @@iterator to Array.prototype.values
        let iterator_key = cx.well_known_symbols.iterator();
        let iterator_value = cx.get_intrinsic(Intrinsic::ArrayPrototypeValues);
        let iterator_desc = PropertyDescriptor::data(iterator_value.into(), true, false, true);
        must!(define_property_or_throw(cx, object.into(), iterator_key, iterator_desc));

        // Set callee property to the enclosing function
        let callee_desc = PropertyDescriptor::data(callee.into(), true, false, true);
        must!(define_property_or_throw(cx, object.into(), cx.names.callee(), callee_desc));
    }

    const MAPPED_PARAMETERS_OFFSET: usize = field_offset!(MappedArgumentsObject, mapped_parameters);

    fn calculate_size_in_bytes(len: usize) -> usize {
        Self::MAPPED_PARAMETERS_OFFSET + InlineArray::<bool>::calculate_size_in_bytes(len)
    }

    /// If this key corresponds to the index of a mapped parameter, return the index in the scope
    /// where that argument is stored.
    #[inline]
    fn get_mapped_scope_index_for_key(&self, key: Handle<PropertyKey>) -> Option<usize> {
        if key.is_array_index() {
            let key_index = key.as_array_index() as usize;
            if key_index < self.mapped_parameters.len() {
                if *self.mapped_parameters.get_unchecked(key_index) {
                    return Some(key_index);
                }
            }
        }

        None
    }

    fn unmap_argument(&mut self, index: usize) {
        self.mapped_parameters.as_mut_slice()[index] = false;
    }

    fn get_mapped_argument(&self, cx: Context, index: usize) -> Handle<Value> {
        self.scope.get_slot(index).to_handle(cx)
    }

    fn set_mapped_argument(&mut self, index: usize, value: Handle<Value>) {
        self.scope.set_slot(index, *value);
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<MappedArgumentsObject> {
    /// [[GetOwnProperty]] (https://tc39.es/ecma262/#sec-arguments-exotic-objects-getownproperty-p)
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        let mut desc = ordinary_get_own_property(cx, self.as_object(), key);
        if let Some(desc) = &mut desc {
            if let Some(scope_index) = self.get_mapped_scope_index_for_key(key) {
                desc.value = Some(self.get_mapped_argument(cx, scope_index));
            }
        } else {
            return Ok(None);
        }

        Ok(desc)
    }

    /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-arguments-exotic-objects-defineownproperty-p-desc)
    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        let scope_index = self.get_mapped_scope_index_for_key(key);
        let mut new_arg_desc = desc;

        if let Some(scope_index) = scope_index {
            if desc.is_data_descriptor() {
                if let Some(false) = desc.is_writable {
                    if desc.value.is_none() {
                        new_arg_desc.value = Some(self.get_mapped_argument(cx, scope_index));
                    }
                }
            }
        }

        if !must!(ordinary_define_own_property(cx, self.as_object(), key, new_arg_desc)) {
            return Ok(false);
        }

        if let Some(scope_index) = scope_index {
            if desc.is_accessor_descriptor() {
                self.unmap_argument(scope_index);
            } else {
                if let Some(value) = desc.value {
                    self.set_mapped_argument(scope_index, value);
                }

                if let Some(false) = desc.is_writable {
                    self.unmap_argument(scope_index);
                }
            }
        }

        Ok(true)
    }

    /// [[Get]] (https://tc39.es/ecma262/#sec-arguments-exotic-objects-get-p-receiver)
    fn get(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
        receiver: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        if let Some(scope_index) = self.get_mapped_scope_index_for_key(key) {
            Ok(self.get_mapped_argument(cx, scope_index))
        } else {
            ordinary_get(cx, self.as_object(), key, receiver)
        }
    }

    /// [[Set]] (https://tc39.es/ecma262/#sec-arguments-exotic-objects-set-p-v-receiver)
    fn set(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
        receiver: Handle<Value>,
    ) -> EvalResult<bool> {
        if receiver.is_object() && same_object_value_handles(self.as_object(), receiver.as_object())
        {
            if let Some(scope_index) = self.get_mapped_scope_index_for_key(key) {
                self.set_mapped_argument(scope_index, value);
            }
        }

        ordinary_set(cx, self.as_object(), key, value, receiver)
    }

    /// [[Delete]] (https://tc39.es/ecma262/#sec-arguments-exotic-objects-delete-p)
    fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        let scope_index = self.get_mapped_scope_index_for_key(key);

        let result = ordinary_delete(cx, self.as_object(), key)?;

        if result {
            if let Some(scope_index) = scope_index {
                self.unmap_argument(scope_index);
            }
        }

        Ok(result)
    }
}

/// CreateUnmappedArgumentsObject (https://tc39.es/ecma262/#sec-createunmappedargumentsobject)
pub fn create_unmapped_arguments_object(cx: Context, arguments: &[Handle<Value>]) -> Handle<Value> {
    let object = UnmappedArgumentsObject::new(cx);

    // Set length property
    let length_value = cx.smi(arguments.len() as i32);
    let length_desc = PropertyDescriptor::data(length_value, true, false, true);
    must!(define_property_or_throw(cx, object, cx.names.length(), length_desc));

    // Property key is shared between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);

    // Set indexed argument properties
    for (i, argument) in arguments.iter().enumerate() {
        index_key.replace(PropertyKey::array_index(cx, i as u32));
        must!(create_data_property_or_throw(cx, object, index_key, *argument));
    }

    // Set @@iterator to Array.prototype.values
    let iterator_key = cx.well_known_symbols.iterator();
    let iterator_value = cx.get_intrinsic(Intrinsic::ArrayPrototypeValues);
    let iterator_desc = PropertyDescriptor::data(iterator_value.into(), true, false, true);
    must!(define_property_or_throw(cx, object, iterator_key, iterator_desc));

    // Set callee to throw a type error when accessed
    let throw_type_error = cx.get_intrinsic(Intrinsic::ThrowTypeError);
    let callee_desc =
        PropertyDescriptor::accessor(Some(throw_type_error), Some(throw_type_error), false, false);
    must!(define_property_or_throw(cx, object, cx.names.callee(), callee_desc));

    object.into()
}

impl HeapObject for HeapPtr<MappedArgumentsObject> {
    fn byte_size(&self) -> usize {
        MappedArgumentsObject::calculate_size_in_bytes(self.mapped_parameters.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.scope);
    }
}

impl HeapObject for HeapPtr<UnmappedArgumentsObject> {
    fn byte_size(&self) -> usize {
        size_of::<UnmappedArgumentsObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
