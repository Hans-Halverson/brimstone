use std::mem::size_of;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::runtime::{
        completion::EvalResult,
        gc::{Handle, HeapPtr},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::{ObjectValue, VirtualObject},
        ordinary_object::{
            is_compatible_property_descriptor, object_create, object_create_from_constructor,
            object_create_with_proto, ordinary_define_own_property,
            ordinary_filtered_own_indexed_property_keys, ordinary_get_own_property,
            ordinary_own_string_symbol_property_keys,
        },
        property_descriptor::PropertyDescriptor,
        string_value::StringValue,
        type_utilities::canonical_numeric_string_index_string,
        value::Value,
        Context, PropertyKey,
    },
    maybe, set_uninit,
};

use super::{
    gc::{HeapObject, HeapVisitor},
    property::Property,
    string_value::FlatString,
};

// String Exotic Objects (https://tc39.es/ecma262/#sec-string-exotic-objects)
extend_object! {
    pub struct StringObject {
        // The string value wrapped by this object
        string_data: HeapPtr<StringValue>,
    }
}

impl StringObject {
    pub fn new_from_value(
        cx: Context,
        string_data_handle: Handle<StringValue>,
    ) -> Handle<StringObject> {
        let mut object =
            object_create::<StringObject>(cx, ObjectKind::StringObject, Intrinsic::StringPrototype);

        let string_data = string_data_handle.get_();
        let string_length = string_data.len();

        set_uninit!(object.string_data, string_data);

        let object = object.to_handle();

        Self::set_length_property(object, cx, string_length);

        object
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        string_data_handle: Handle<StringValue>,
    ) -> EvalResult<Handle<StringObject>> {
        let mut object = maybe!(object_create_from_constructor::<StringObject>(
            cx,
            constructor,
            ObjectKind::StringObject,
            Intrinsic::StringPrototype
        ));

        let string_data = string_data_handle.get_();
        let string_length = string_data.len();

        set_uninit!(object.string_data, string_data);

        let object = object.to_handle();

        Self::set_length_property(object, cx, string_length);

        object.into()
    }

    pub fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        string_data_handle: Handle<StringValue>,
    ) -> Handle<StringObject> {
        let mut object =
            object_create_with_proto::<StringObject>(cx, ObjectKind::StringObject, proto);

        let string_data = string_data_handle.get_();
        let string_length = string_data.len();

        set_uninit!(object.string_data, string_data);

        let object = object.to_handle();

        Self::set_length_property(object, cx, string_length);

        object
    }

    fn set_length_property(string: Handle<StringObject>, cx: Context, length: u32) {
        // String objects have an immutable length property
        let length_value = Value::from(length).to_handle(cx);
        string.object().set_property(
            cx,
            cx.names.length(),
            Property::data(length_value, false, false, false),
        );
    }

    pub fn string_data(&self) -> Handle<StringValue> {
        self.string_data.to_handle()
    }
}

impl StringObject {
    /// StringGetOwnProperty (https://tc39.es/ecma262/#sec-stringgetownproperty)
    fn string_get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> Option<PropertyDescriptor> {
        if key.is_symbol() {
            return None;
        }

        let string = self.string_data();
        let index = canonical_numeric_string_index_string(cx, key, string.len())??;
        let code_unit = string.code_unit_at(index);

        let char_string = FlatString::from_code_unit(cx, code_unit).as_string();

        Some(PropertyDescriptor::data(char_string.into(), false, true, false))
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<StringObject> {
    /// [[GetOwnProperty]] (https://tc39.es/ecma262/#sec-string-exotic-objects-getownproperty-p)
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        let desc = ordinary_get_own_property(cx, self.object(), key);
        if desc.is_none() {
            self.string_get_own_property(cx, key).into()
        } else {
            desc.into()
        }
    }

    /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-string-exotic-objects-defineownproperty-p-desc)
    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        let string_desc = self.string_get_own_property(cx, key);
        if string_desc.is_some() {
            let is_extensible = self.object().is_extensible_field();
            is_compatible_property_descriptor(cx, is_extensible, desc, string_desc).into()
        } else {
            ordinary_define_own_property(cx, self.object(), key, desc)
        }
    }

    /// [[OwnPropertyKeys]] (https://tc39.es/ecma262/#sec-string-exotic-objects-ownpropertykeys)
    fn own_property_keys(&self, mut cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        let mut keys: Vec<Handle<Value>> = vec![];

        let length = self.string_data.len();
        for i in 0..length {
            let index_string = cx.alloc_string(&i.to_string());
            keys.push(index_string.into());
        }

        ordinary_filtered_own_indexed_property_keys(cx, self.object(), &mut keys, |index| {
            index >= (length as usize)
        });

        ordinary_own_string_symbol_property_keys(self.object(), &mut keys);

        keys.into()
    }
}

impl HeapObject for HeapPtr<StringObject> {
    fn byte_size(&self) -> usize {
        size_of::<StringObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.string_data);
    }
}
