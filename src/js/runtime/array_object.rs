use std::mem::size_of;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{extend_object, js::runtime::type_utilities::is_array, maybe, must, set_uninit};

use super::{
    abstract_operations::{construct, create_data_property_or_throw, get_function_realm},
    error::{range_error, type_error},
    gc::{HeapObject, HeapVisitor},
    get,
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::{
        object_create_with_proto, ordinary_define_own_property, ordinary_delete,
        ordinary_filtered_own_indexed_property_keys, ordinary_get_own_property,
        ordinary_own_string_symbol_property_keys,
    },
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::{is_constructor_value, same_object_value, to_number, to_uint32},
    Context, EvalResult, Handle, HeapPtr, Realm, Value,
};

// Array Exotic Objects (https://tc39.es/ecma262/#sec-array-exotic-objects)
extend_object! {
    pub struct ArrayObject {
        // Length property is backed by length of object's ArrayProperties. There is no explicit
        // property descriptor stored, so attributes here.
        is_length_writable: bool,
    }
}

impl ArrayObject {
    pub fn new(cx: Context, proto: Handle<ObjectValue>) -> Handle<ArrayObject> {
        let mut array = object_create_with_proto::<ArrayObject>(cx, ObjectKind::ArrayObject, proto);

        set_uninit!(array.is_length_writable, true);

        array.to_handle()
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<ArrayObject> {
    /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-array-exotic-objects-defineownproperty-p-desc)
    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            if array_index >= self.as_object().array_properties_length() && !self.is_length_writable
            {
                return Ok(false);
            }

            if !must!(ordinary_define_own_property(cx, self.as_object(), key, desc)) {
                return Ok(false);
            }

            Ok(true)
        } else if key.is_string() && key.as_string().eq(&cx.names.length().as_string()) {
            array_set_length(cx, *self, desc)
        } else {
            ordinary_define_own_property(cx, self.as_object(), key, desc)
        }
    }

    // Not part of spec, but needed to return custom length property
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        if key.is_string() && key.as_string().eq(&cx.names.length().as_string()) {
            let length_value =
                Value::from(self.as_object().array_properties_length()).to_handle(cx);
            return Ok(Some(PropertyDescriptor::data(
                length_value,
                self.is_length_writable,
                false,
                false,
            )));
        }

        Ok(ordinary_get_own_property(cx, self.as_object(), key))
    }

    // Not part of spec, but needed to handle attempts to delete custom length property
    fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        if key.is_string() && key.as_string().eq(&cx.names.length().as_string()) {
            return Ok(false);
        }

        ordinary_delete(cx, self.as_object(), key)
    }

    // Not part of spec, but needed to add custom length property
    fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        let mut keys: Vec<Handle<Value>> = vec![];

        ordinary_filtered_own_indexed_property_keys(cx, self.as_object(), &mut keys, |_| true);

        // Insert length as the first non-index property
        keys.push(cx.names.length().as_string().into());

        ordinary_own_string_symbol_property_keys(self.as_object(), &mut keys);

        Ok(keys)
    }
}

/// ArrayCreate (https://tc39.es/ecma262/#sec-arraycreate)
pub fn array_create(
    cx: Context,
    length: u64,
    proto: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<ArrayObject>> {
    let realm = cx.current_realm();
    array_create_in_realm(cx, realm, length, proto)
}

pub fn array_create_in_realm(
    cx: Context,
    realm: Handle<Realm>,
    length: u64,
    proto: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<ArrayObject>> {
    if length > (u32::MAX as u64) {
        return range_error(cx, "array length out of range");
    }

    let proto = proto.unwrap_or_else(|| realm.get_intrinsic(Intrinsic::ArrayPrototype));

    let mut array_object = ArrayObject::new(cx, proto);

    let length_value = Value::from(length as u32).to_handle(cx);
    let length_desc = PropertyDescriptor::data(length_value, true, false, false);
    must!(array_object.define_own_property(cx, cx.names.length(), length_desc));

    Ok(array_object)
}

/// ArraySpeciesCreate (https://tc39.es/ecma262/#sec-arrayspeciescreate)
pub fn array_species_create(
    cx: Context,
    original_array: Handle<ObjectValue>,
    length: u64,
) -> EvalResult<Handle<ObjectValue>> {
    if !maybe!(is_array(cx, original_array.into())) {
        let array_object = maybe!(array_create(cx, length, None)).as_object();
        return Ok(array_object);
    }

    let mut constructor = maybe!(get(cx, original_array, cx.names.constructor()));
    if is_constructor_value(constructor) {
        let this_realm_ptr = cx.current_realm_ptr();
        let constructor_realm = maybe!(get_function_realm(cx, constructor.as_object()));

        if !this_realm_ptr.ptr_eq(&constructor_realm)
            && same_object_value(
                constructor.as_object().get_(),
                constructor_realm.get_intrinsic_ptr(Intrinsic::ArrayConstructor),
            )
        {
            constructor = cx.undefined();
        }
    }

    if constructor.is_object() {
        let species_key = cx.well_known_symbols.species();
        constructor = maybe!(get(cx, constructor.as_object(), species_key));

        if constructor.is_null() {
            constructor = cx.undefined();
        }
    }

    if constructor.is_undefined() {
        let array_object = maybe!(array_create(cx, length, None)).as_object();
        return Ok(array_object);
    }

    if !is_constructor_value(constructor) {
        return type_error(cx, "expected array constructor");
    }

    let length_value = Value::from(length).to_handle(cx);
    construct(cx, constructor.as_object(), &[length_value], None)
}

/// ArraySetLength (https://tc39.es/ecma262/#sec-arraysetlength)
// Modified from spec to use custom length property.
fn array_set_length(
    cx: Context,
    mut array: Handle<ArrayObject>,
    desc: PropertyDescriptor,
) -> EvalResult<bool> {
    let mut new_len = array.as_object().array_properties_length();

    if let Some(value) = desc.value {
        new_len = maybe!(to_uint32(cx, value));
        let number_len = maybe!(to_number(cx, value));

        if <u32 as Into<f64>>::into(new_len) != number_len.as_number() {
            return range_error(cx, "invalid array length");
        }
    }

    if let Some(true) = desc.is_configurable {
        return Ok(false);
    } else if let Some(true) = desc.is_enumerable {
        return Ok(false);
    } else if desc.is_accessor_descriptor() {
        return Ok(false);
    }

    if !array.is_length_writable {
        if let Some(true) = desc.is_writable {
            return Ok(false);
        } else if new_len != array.as_object().array_properties_length() {
            return Ok(false);
        }
    }

    let has_delete_succeeded = array.as_object().set_array_properties_length(cx, new_len);

    if let Some(false) = desc.is_writable {
        array.is_length_writable = false;
    }

    Ok(has_delete_succeeded)
}

/// CreateArrayFromList (https://tc39.es/ecma262/#sec-createarrayfromlist)
pub fn create_array_from_list(cx: Context, elements: &[Handle<Value>]) -> Handle<ArrayObject> {
    let array = must!(array_create(cx, 0, None));

    // Property key is shared between iterations
    let mut key = PropertyKey::uninit().to_handle(cx);

    for (index, element) in elements.iter().enumerate() {
        // TODO: Handle keys out of u32 range
        key.replace(PropertyKey::array_index(cx, index as u32));
        must!(create_data_property_or_throw(cx, array.into(), key, *element));
    }

    array
}

impl HeapObject for HeapPtr<ArrayObject> {
    fn byte_size(&self) -> usize {
        size_of::<ArrayObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}
