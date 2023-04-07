use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::runtime::{
        completion::EvalResult,
        gc::Gc,
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::{ObjectValue, VirtualObject},
        ordinary_object::{
            is_compatible_property_descriptor, object_ordinary_init,
            object_ordinary_init_from_constructor, ordinary_define_own_property,
            ordinary_filtered_own_indexed_property_keys, ordinary_get_own_property,
            ordinary_own_string_symbol_property_keys,
        },
        property::Property,
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        string_value::StringValue,
        type_utilities::canonical_numeric_index_string,
        value::Value,
        Context,
    },
    maybe,
};

// 10.4.3 String Exotic Objects
extend_object! {
    pub struct StringObject {
        // The string value wrapped by this object
        string_data: Gc<StringValue>,
    }
}

impl StringObject {
    pub fn new(
        cx: &mut Context,
        proto: Gc<ObjectValue>,
        string_data: Gc<StringValue>,
    ) -> Gc<StringObject> {
        let mut object = cx.heap.alloc_uninit::<StringObject>();
        object.descriptor = cx.base_descriptors.get(ObjectKind::StringObject);

        object_ordinary_init(object.object(), proto);

        object.string_data = string_data;

        // String objects have an immutable length property
        object.object().set_property(
            &cx.names.length(),
            Property::data((string_data.len() as f64).into(), false, false, false),
        );

        object
    }

    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Gc<ObjectValue>,
        string_data: Gc<StringValue>,
    ) -> EvalResult<Gc<StringObject>> {
        let mut object = cx.heap.alloc_uninit::<StringObject>();
        object.descriptor = cx.base_descriptors.get(ObjectKind::StringObject);

        maybe!(object_ordinary_init_from_constructor(
            cx,
            object.object(),
            constructor,
            Intrinsic::StringPrototype
        ));

        object.string_data = string_data;

        // String objects have an immutable length property
        object.object().set_property(
            &cx.names.length(),
            Property::data((string_data.len() as f64).into(), false, false, false),
        );

        object.into()
    }

    pub fn new_from_value(cx: &mut Context, string_data: Gc<StringValue>) -> Gc<StringObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::StringPrototype);

        StringObject::new(cx, proto, string_data)
    }

    pub fn string_data(&self) -> Gc<StringValue> {
        self.string_data
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

        let string = self.string_data;
        if index as usize >= string.len() {
            return None;
        }

        let char_string = StringValue::from_code_unit(cx, string.code_unit_at(index as usize));

        Some(PropertyDescriptor::data(char_string.into(), false, true, false))
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Gc<StringObject> {
    // 10.4.3.1 [[GetOwnProperty]]
    fn get_own_property(
        &self,
        cx: &mut Context,
        key: &PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        let desc = ordinary_get_own_property(self.object(), key);
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
            let is_extensible = self.object().is_extensible_field();
            is_compatible_property_descriptor(cx, is_extensible, desc, string_desc).into()
        } else {
            ordinary_define_own_property(cx, self.object(), key, desc)
        }
    }

    // 10.4.3.3 [[OwnPropertyKeys]]
    fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<Value>> {
        let mut keys = vec![];

        let length = self.string_data.len();
        for i in 0..length {
            let index_string = cx.heap.alloc_string(i.to_string());
            keys.push(Value::string(index_string));
        }

        ordinary_filtered_own_indexed_property_keys(cx, self.object(), &mut keys, &|index| {
            index >= length
        });

        ordinary_own_string_symbol_property_keys(cx, self.object(), &mut keys);

        keys.into()
    }
}
