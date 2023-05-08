use wrap_ordinary_object::wrap_ordinary_object;

use crate::{extend_object, js::runtime::type_utilities::is_array, maybe, must, set_uninit};

use super::{
    abstract_operations::{construct, create_data_property_or_throw, get_function_realm},
    error::{range_error_, type_error_},
    gc::HandleValue,
    get,
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::{
        object_create_with_proto, ordinary_define_own_property, ordinary_delete,
        ordinary_get_own_property, ordinary_own_property_keys,
    },
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::{is_constructor, same_object_value, to_number, to_uint32},
    Context, EvalResult, Gc, Handle, Value,
};

// 10.4.2 Array Exotic Objects
extend_object! {
    pub struct ArrayObject {
        // Length property is backed by length of object's ArrayProperties. There is no explicit
        // property descriptor stored, so attributes here.
        is_length_writable: bool,
    }
}

impl ArrayObject {
    pub fn new(cx: &mut Context, proto: Handle<ObjectValue>) -> Handle<ArrayObject> {
        let mut array = object_create_with_proto::<ArrayObject>(cx, ObjectKind::ArrayObject, proto);

        set_uninit!(array.is_length_writable, true);

        Handle::from_heap(array)
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<ArrayObject> {
    // 10.4.2.1 [[DefineOwnProperty]]
    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            if array_index >= self.object().array_properties_length() && !self.is_length_writable {
                return false.into();
            }

            if !must!(ordinary_define_own_property(cx, self.object(), key, desc)) {
                return false.into();
            }

            true.into()
        } else if key.is_string() && key.as_string() == cx.names.length().as_string() {
            array_set_length(cx, *self, desc)
        } else {
            ordinary_define_own_property(cx, self.object(), key, desc)
        }
    }

    // Not part of spec, but needed to return custom length property
    fn get_own_property(
        &self,
        cx: &mut Context,
        key: PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        if key.is_string() && key.as_string() == cx.names.length().as_string() {
            let length_value = self.object().array_properties_length();
            return Some(PropertyDescriptor::data(
                Value::number(length_value.into()),
                self.is_length_writable,
                false,
                false,
            ))
            .into();
        }

        ordinary_get_own_property(self.object(), key).into()
    }

    // Not part of spec, but needed to handle attempts to delete custom length property
    fn delete(&mut self, cx: &mut Context, key: PropertyKey) -> EvalResult<bool> {
        if key.is_string() && key.as_string() == cx.names.length().as_string() {
            return false.into();
        }

        ordinary_delete(cx, self.object(), key)
    }

    // Not part of spec, but needed to add custom length property
    fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<HandleValue>> {
        let mut property_keys = ordinary_own_property_keys(cx, self.object());

        // Insert length property after all the array index properies
        property_keys.push(cx.names.length.as_string().into());

        property_keys.into()
    }
}

// 10.4.2.2 ArrayCreate
pub fn array_create(
    cx: &mut Context,
    length: u64,
    proto: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<ArrayObject>> {
    if length > (u32::MAX as u64) {
        return range_error_(cx, "array length out of range");
    }

    let proto = proto.unwrap_or_else(|| cx.get_intrinsic(Intrinsic::ArrayPrototype));

    let mut array_object = ArrayObject::new(cx, proto);

    let length_value = Value::number((length as u32).into());
    let length_desc = PropertyDescriptor::data(length_value, true, false, false);
    must!(array_object.define_own_property(cx, cx.names.length(), length_desc));

    array_object.into()
}

// 10.4.2.3 ArraySpeciesCreate
pub fn array_species_create(
    cx: &mut Context,
    original_array: Handle<ObjectValue>,
    length: u64,
) -> EvalResult<Handle<ObjectValue>> {
    if !maybe!(is_array(cx, original_array.into())) {
        let array_object: Handle<ObjectValue> = maybe!(array_create(cx, length, None)).into();
        return array_object.into();
    }

    let mut constructor = maybe!(get(cx, original_array, cx.names.constructor()));
    if is_constructor(constructor) {
        let this_realm_ptr = cx.current_realm_ptr();
        let constructor_realm = maybe!(get_function_realm(cx, constructor.as_object()));

        if !this_realm_ptr.ptr_eq(&constructor_realm)
            && same_object_value(
                constructor.as_object(),
                constructor_realm.get_intrinsic_ptr(Intrinsic::ArrayConstructor),
            )
        {
            constructor = Value::undefined();
        }
    }

    if constructor.is_object() {
        let species_key = cx.well_known_symbols.species();
        constructor = maybe!(get(cx, constructor.as_object(), species_key));

        if constructor.is_null() {
            constructor = Value::undefined();
        }
    }

    if constructor.is_undefined() {
        let array_object: Handle<ObjectValue> = maybe!(array_create(cx, length, None)).into();
        return array_object.into();
    }

    if !is_constructor(constructor) {
        return type_error_(cx, "expected array constructor");
    }

    let length_value = Value::from(length);
    construct(cx, constructor.as_object(), &[length_value], None)
}

// 10.4.2.4 ArraySetLength
// Modified from spec to use custom length property.
fn array_set_length(
    cx: &mut Context,
    mut array: Handle<ArrayObject>,
    desc: PropertyDescriptor,
) -> EvalResult<bool> {
    let mut new_len = array.object().array_properties_length();

    if let Some(value) = desc.value {
        new_len = maybe!(to_uint32(cx, value));
        let number_len = maybe!(to_number(cx, value));

        if <u32 as Into<f64>>::into(new_len) != number_len.as_number() {
            return range_error_(cx, "invalid array length");
        }
    }

    if let Some(true) = desc.is_configurable {
        return false.into();
    } else if let Some(true) = desc.is_enumerable {
        return false.into();
    } else if desc.is_accessor_descriptor() {
        return false.into();
    }

    if !array.is_length_writable {
        if let Some(true) = desc.is_writable {
            return false.into();
        } else if new_len != array.object().array_properties_length() {
            return false.into();
        }
    }

    let has_delete_succeeded = array.object().set_array_properties_length(cx, new_len);

    if let Some(false) = desc.is_writable {
        array.is_length_writable = false;
    }

    return has_delete_succeeded.into();
}

// 7.3.18 CreateArrayFromList
pub fn create_array_from_list(cx: &mut Context, elements: &[HandleValue]) -> Handle<ArrayObject> {
    let array = must!(array_create(cx, 0, None));

    for (index, element) in elements.iter().enumerate() {
        // TODO: Handle keys out of u32 range
        let key = PropertyKey::array_index(cx, index as u32);
        must!(create_data_property_or_throw(cx, array.into(), key, *element));
    }

    array
}
