use wrap_ordinary_object::wrap_ordinary_object;

use crate::{impl_gc_into, maybe, must};

use super::{
    abstract_operations::create_data_property_or_throw,
    environment::private_environment::PrivateNameId,
    error::range_error_,
    gc::GcDeref,
    intrinsics::intrinsics::Intrinsic,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    ordinary_object::{
        ordinary_define_own_property, ordinary_delete, ordinary_get_own_property,
        ordinary_object_create, ordinary_own_property_keys, OrdinaryObject,
    },
    property::{PrivateProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::{to_number, to_uint32},
    Context, EvalResult, Gc, Value,
};

// 10.4.2 Array Exotic Objects
pub struct ArrayObject {
    _vtable: ObjectValueVtable,
    pub object: OrdinaryObject,
    // Length property is backed by length of object's ArrayProperties. There is no explicit
    // property descriptor stored, so attributes here.
    is_length_writable: bool,
}

impl GcDeref for ArrayObject {}

impl_gc_into!(ArrayObject, ObjectValue);

impl ArrayObject {
    const VTABLE: *const () = extract_object_vtable::<ArrayObject>();

    pub fn new(object: OrdinaryObject) -> ArrayObject {
        ArrayObject {
            _vtable: ArrayObject::VTABLE,
            object,
            is_length_writable: true,
        }
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }
}

#[wrap_ordinary_object]
impl Object for ArrayObject {
    fn is_array(&self) -> bool {
        true
    }

    // 10.4.2.1 [[DefineOwnProperty]]
    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            if array_index >= self.object.array_properties_length() && !self.is_length_writable {
                return false.into();
            }

            if !must!(ordinary_define_own_property(cx, self.into(), key, desc)) {
                return false.into();
            }

            true.into()
        } else if key.is_string() && key.as_string().str() == "length" {
            array_set_length(cx, self, desc)
        } else {
            ordinary_define_own_property(cx, self.into(), key, desc)
        }
    }

    // Not part of spec, but needed to return custom length property
    fn get_own_property(
        &self,
        cx: &mut Context,
        key: &PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        if key.is_string() && key.as_string().str() == "length" {
            let length_value = self.object.array_properties_length();
            return Some(PropertyDescriptor::data(
                Value::number(length_value.into()),
                self.is_length_writable,
                false,
                false,
            ))
            .into();
        }

        ordinary_get_own_property(&self.object, key).into()
    }

    // Not part of spec, but needed to handle attempts to delete custom length property
    fn delete(&mut self, cx: &mut Context, key: &PropertyKey) -> EvalResult<bool> {
        if key.is_string() && key.as_string().str() == "length" {
            return false.into();
        }

        ordinary_delete(cx, self.into(), key)
    }

    // Not part of spec, but needed to add custom length property
    fn own_property_keys(&self, cx: &mut Context) -> Vec<Value> {
        let mut property_keys = ordinary_own_property_keys(cx, &self.object);

        // Insert length property after all the array index properies
        // TODO: Correctly order all properties
        property_keys.push(cx.names.length.as_string().into());

        property_keys
    }
}

// 10.4.2.2 ArrayCreate
pub fn array_create(
    cx: &mut Context,
    length: u64,
    proto: Option<Gc<ObjectValue>>,
) -> EvalResult<Gc<ArrayObject>> {
    if length > (u32::MAX as u64) {
        return range_error_(cx, "array length out of range");
    }

    let proto =
        proto.unwrap_or_else(|| cx.current_realm().get_intrinsic(Intrinsic::ArrayPrototype));

    let object = ordinary_object_create(proto);

    let mut array_object = ArrayObject::new(object);

    let length_value = Value::number((length as u32).into());
    let length_desc = PropertyDescriptor::data(length_value, true, false, false);
    must!(array_object.define_own_property(cx, &cx.names.length(), length_desc));

    cx.heap.alloc(array_object).into()
}

// 10.4.2.4 ArraySetLength
// Modified from spec to use custom length property.
fn array_set_length(
    cx: &mut Context,
    array: &mut ArrayObject,
    desc: PropertyDescriptor,
) -> EvalResult<bool> {
    let mut new_len = array.object.array_properties_length();

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
        } else if new_len != array.object.array_properties_length() {
            return false.into();
        }
    }

    // TODO: Resize array_properties
    let has_delete_succeeded = array.object.set_array_properties_length(new_len);

    if let Some(false) = desc.is_writable {
        array.is_length_writable = false;
    }

    return has_delete_succeeded.into();
}

// 7.3.18 CreateArrayFromList
pub fn create_array_from_list(cx: &mut Context, elements: &[Value]) -> Gc<ArrayObject> {
    let array = must!(array_create(cx, 0, None));

    for (index, element) in elements.iter().enumerate() {
        // TODO: Handle keys out of u32 range
        let key = PropertyKey::array_index(index as u32);
        must!(create_data_property_or_throw(cx, array.into(), &key, *element));
    }

    array
}
