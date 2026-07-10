use std::mem::size_of;

use brimstone_macros::wrap_ordinary_object;

use crate::{
    extend_object, must,
    parser::scope_tree::SHADOWED_SCOPE_SLOT_NAME,
    runtime::{
        Context, EvalResult, HeapItemKind, HeapPtr, Value,
        abstract_operations::{create_data_property_or_throw, define_property_or_throw},
        alloc_error::AllocResult,
        bitmap::ValueBitmap,
        bytecode::function::ClosureObject,
        gc::{Handle, HeapItem, HeapVisitor},
        interned_strings::InternedStrings,
        intrinsics::intrinsics::Intrinsic,
        object_value::{ObjectValue, VirtualObject},
        ordinary_object::{
            object_create, ordinary_define_own_property, ordinary_delete, ordinary_get,
            ordinary_get_own_property, ordinary_set,
        },
        property::Property,
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        rust_vtables::extract_virtual_object_vtable,
        scope::Scope,
        type_utilities::same_object_value_handles,
    },
    set_uninit,
};

extend_object! {
    /// An unmapped arguments object that is identical to an ordinary object, but has an arguments
    /// object shape. This emulates an ordinary object with a [[ParameterMap]] slot described
    /// in spec.
    pub struct UnmappedArgumentsObject {}
}

impl UnmappedArgumentsObject {
    pub fn new(cx: Context) -> AllocResult<Handle<ObjectValue>> {
        Ok(object_create::<ObjectValue>(
            cx,
            HeapItemKind::UnmappedArgumentsObject,
            Intrinsic::ObjectPrototype,
        )?
        .to_handle())
    }
}

extend_object! {
    /// A mapped arguments exotic argument used in the bytecode VM. Contains a reference to the
    /// scope where the arguments are stored so that they can be referenced directly.
    ///
    /// Only some parameters are mapped, and this can change dynamically due to user action. Stored
    /// as a bitmap.
    ///
    /// Arguments Exotic Objects (https://tc39.es/ecma262/#sec-arguments-exotic-objects)
    pub struct MappedArgumentsObject {
        /// Scope where the arguments are stored.
        scope: HeapPtr<Scope>,
        /// Bitmap of which parameters are mapped to the scope.
        mapped_parameters: Value,
    }
}

impl MappedArgumentsObject {
    pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();

    pub fn new(
        cx: Context,
        callee: Handle<ClosureObject>,
        arguments: &[Handle<Value>],
        scope: Handle<Scope>,
        num_parameters: usize,
    ) -> EvalResult<Handle<MappedArgumentsObject>> {
        let shadowed_name = InternedStrings::alloc_static_wtf8_str(cx, &SHADOWED_SCOPE_SLOT_NAME)?;

        let mut object = object_create::<MappedArgumentsObject>(
            cx,
            HeapItemKind::MappedArgumentsObject,
            Intrinsic::ObjectPrototype,
        )?;

        set_uninit!(object.scope, *scope);
        // Placeholder before the bitmap is created
        set_uninit!(object.mapped_parameters, Value::smi(0));

        let mut object = object.to_handle();

        // A parameter is mapped if it has not been shadowed, which we know due to the special
        // shadowed scope slot name. May allocate.
        object.mapped_parameters = ValueBitmap::new(cx, num_parameters, |i| {
            !scope
                .scope_names_ptr()
                .get_slot_name(i)
                .ptr_eq(&*shadowed_name)
        })?;

        Self::init_properties(cx, object, callee, arguments)?;

        Ok(object)
    }

    fn init_properties(
        cx: Context,
        object: Handle<MappedArgumentsObject>,
        callee: Handle<ClosureObject>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<()> {
        // Property key is shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        // Set indexed argument properties
        for (i, argument) in arguments.iter().enumerate() {
            index_key.replace(PropertyKey::array_index(cx, i as u32)?);
            must!(create_data_property_or_throw(cx, object.into(), index_key, *argument));
        }

        // Set length property
        let length_value = cx.number(arguments.len());
        let length_desc = PropertyDescriptor::data(length_value, true, false, true);
        must!(define_property_or_throw(cx, object.into(), cx.names.length(), length_desc));

        // Set @@iterator to Array.prototype.values
        let iterator_key = cx.symbols.iterator();
        let iterator_value = cx.get_intrinsic(Intrinsic::ArrayPrototypeValues);
        let iterator_desc = PropertyDescriptor::data(iterator_value.into(), true, false, true);
        must!(define_property_or_throw(cx, object.into(), iterator_key, iterator_desc));

        // Set callee property to the enclosing function
        let callee_desc = PropertyDescriptor::data(callee.into(), true, false, true);
        must!(define_property_or_throw(cx, object.into(), cx.names.callee(), callee_desc));

        Ok(())
    }

    /// If this key corresponds to the index of a mapped parameter, return the index in the scope
    /// where that argument is stored.
    #[inline]
    fn get_mapped_scope_index_for_key(&self, key: Handle<PropertyKey>) -> Option<usize> {
        if key.is_array_index() {
            let key_index = key.as_array_index() as usize;
            if ValueBitmap::from_value(self.mapped_parameters).get(key_index) {
                return Some(key_index);
            }
        }

        None
    }

    fn unmap_argument(&mut self, index: usize) {
        self.mapped_parameters = ValueBitmap::from_value(self.mapped_parameters).clear(index);
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
    ) -> EvalResult<Option<Property>> {
        let mut property = match ordinary_get_own_property(cx, self.as_object(), key) {
            Some(property) => property,
            None => return Ok(None),
        };

        if let Some(scope_index) = self.get_mapped_scope_index_for_key(key) {
            property.set_value(self.get_mapped_argument(cx, scope_index));
        }

        Ok(Some(property))
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
pub fn create_unmapped_arguments_object(
    cx: Context,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    let object = UnmappedArgumentsObject::new(cx)?;

    // Set length property
    let length_value = cx.number(arguments.len());
    let length_desc = PropertyDescriptor::data(length_value, true, false, true);
    must!(define_property_or_throw(cx, object, cx.names.length(), length_desc));

    // Property key is shared between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);

    // Set indexed argument properties
    for (i, argument) in arguments.iter().enumerate() {
        index_key.replace(PropertyKey::array_index(cx, i as u32)?);
        must!(create_data_property_or_throw(cx, object, index_key, *argument));
    }

    // Set @@iterator to Array.prototype.values
    let iterator_key = cx.symbols.iterator();
    let iterator_value = cx.get_intrinsic(Intrinsic::ArrayPrototypeValues);
    let iterator_desc = PropertyDescriptor::data(iterator_value.into(), true, false, true);
    must!(define_property_or_throw(cx, object, iterator_key, iterator_desc));

    // Set callee to throw a type error when accessed
    let throw_type_error = cx.get_intrinsic(Intrinsic::ThrowTypeError);
    let callee_desc =
        PropertyDescriptor::accessor(Some(throw_type_error), Some(throw_type_error), false, false);
    must!(define_property_or_throw(cx, object, cx.names.callee(), callee_desc));

    Ok(object.into())
}

impl HeapItem for MappedArgumentsObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<MappedArgumentsObject>()
    }

    fn visit_pointers(mut mapped_arguments_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        mapped_arguments_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut mapped_arguments_object.scope);
        visitor.visit_value(&mut mapped_arguments_object.mapped_parameters);
    }
}

impl HeapItem for UnmappedArgumentsObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<UnmappedArgumentsObject>()
    }

    fn visit_pointers(
        mut unmapped_arguments_object: HeapPtr<Self>,
        visitor: &mut impl HeapVisitor,
    ) {
        unmapped_arguments_object.visit_object_pointers(visitor);
    }
}
