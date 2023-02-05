use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{ordinary_object_create, OrdinaryObject},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        value::{StringValue, Value},
        Context,
    },
};

use super::{
    intrinsics::intrinsics::Intrinsic,
    ordinary_object::{
        is_compatible_property_descriptor, ordinary_define_own_property,
        ordinary_filtered_own_indexed_property_keys, ordinary_get_own_property,
        ordinary_own_string_symbol_property_keys,
    },
    type_utilities::canonical_numeric_index_string,
};

// 10.4.3 String Exotic Objects
#[repr(C)]
pub struct StringObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    // The string value wrapped by this object
    string_data: Gc<StringValue>,
}

impl GcDeref for StringObject {}

impl_gc_into!(StringObject, ObjectValue);

impl StringObject {
    const VTABLE: *const () = extract_object_vtable::<StringObject>();

    pub fn new(
        cx: &mut Context,
        mut object: OrdinaryObject,
        string_data: Gc<StringValue>,
    ) -> StringObject {
        // String objects have an immutable length property
        let length = string_data.str().len();

        object.set_property(
            &cx.names.length(),
            Property::data((length as f64).into(), false, false, false),
        );

        StringObject { _vtable: Self::VTABLE, object, string_data }
    }

    pub fn new_from_value(cx: &mut Context, string_data: Gc<StringValue>) -> Gc<StringObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::StringPrototype);
        let object = ordinary_object_create(proto);

        let string_object = StringObject::new(cx, object, string_data);
        cx.heap.alloc(string_object)
    }

    pub fn string_data(&self) -> Gc<StringValue> {
        self.string_data
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }

    // 10.4.3.5 StringGetOwnProperty
    fn string_get_own_property(
        &self,
        cx: &mut Context,
        key: &PropertyKey,
    ) -> Option<PropertyDescriptor> {
        if key.as_symbol().is_some() {
            return None;
        }

        let index = if let Some(index) = canonical_numeric_index_string(key) {
            index
        } else {
            return None;
        };

        let str = self.string_data.str();
        if index as usize >= str.len() {
            return None;
        }

        let char_string = String::from(str.as_bytes()[index as usize] as char);
        let char_value = cx.heap.alloc_string(char_string).into();

        Some(PropertyDescriptor::data(char_value, false, true, false))
    }
}

#[wrap_ordinary_object]
impl Object for StringObject {
    fn is_string_object(&self) -> bool {
        true
    }

    // 10.4.3.1 [[GetOwnProperty]]
    fn get_own_property(
        &self,
        cx: &mut Context,
        key: &PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        let desc = ordinary_get_own_property(&self.object, key);
        if desc.is_none() {
            self.string_get_own_property(cx, key).into()
        } else {
            desc.into()
        }
    }

    // 10.4.3.2 [[DefineOwnProperty]]
    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        let string_desc = self.string_get_own_property(cx, key);
        if string_desc.is_some() {
            let is_extensible = self.object.is_extensible_raw();
            is_compatible_property_descriptor(cx, is_extensible, desc, string_desc).into()
        } else {
            ordinary_define_own_property(cx, self.into(), key, desc)
        }
    }

    // 10.4.3.3 [[OwnPropertyKeys]]
    fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<Value>> {
        let mut keys = vec![];

        let length = self.string_data.str().len();
        for i in 0..length {
            let index_string = cx.heap.alloc_string(i.to_string());
            keys.push(Value::string(index_string));
        }

        ordinary_filtered_own_indexed_property_keys(cx, &self.object, &mut keys, &|index| {
            index >= length
        });

        ordinary_own_string_symbol_property_keys(cx, &self.object, &mut keys);

        keys.into()
    }
}
