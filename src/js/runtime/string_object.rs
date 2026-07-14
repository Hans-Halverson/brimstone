use std::mem::size_of;

use brimstone_macros::wrap_ordinary_object;

use crate::{
    extend_object,
    runtime::{
        Context, PropertyKey,
        alloc_error::AllocResult,
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapPtr, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::{ObjectValue, VirtualObject},
        ordinary_object::{
            ObjectBuilder, is_compatible_property_descriptor, ordinary_define_own_property,
            ordinary_filtered_own_indexed_property_keys, ordinary_get_own_property,
            ordinary_own_string_symbol_property_keys,
        },
        property::{Property, PropertyFlags},
        property_descriptor::PropertyDescriptor,
        rust_vtables::extract_virtual_object_vtable,
        string_value::{FlatString, StringValue},
        type_utilities::canonical_numeric_string_index_string,
        value::Value,
    },
    set_uninit,
};

extend_object! {
    /// String Exotic Objects (https://tc39.es/ecma262/#sec-string-exotic-objects)
    pub struct StringObject {
        /// The string value wrapped by this object
        string_data: HeapPtr<StringValue>,
    }
}

impl StringObject {
    pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();

    pub fn new_from_value(
        cx: Context,
        string_data_handle: Handle<StringValue>,
    ) -> AllocResult<Handle<StringObject>> {
        let mut object = ObjectBuilder::<StringObject>::new(cx)
            .intrinsic_proto(Intrinsic::StringPrototype)
            .build()?;

        let string_data = *string_data_handle;
        let string_length = string_data.len();

        set_uninit!(object.string_data, string_data);

        let object = object.to_handle();

        Self::set_length_property(object, cx, string_length)?;

        Ok(object)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        string_data_handle: Handle<StringValue>,
    ) -> EvalResult<Handle<StringObject>> {
        let mut object = ObjectBuilder::<StringObject>::new(cx)
            .constructor_proto(constructor, Intrinsic::StringPrototype)?
            .build()?;

        let string_data = *string_data_handle;
        let string_length = string_data.len();

        set_uninit!(object.string_data, string_data);

        let object = object.to_handle();

        Self::set_length_property(object, cx, string_length)?;

        Ok(object)
    }

    pub fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        string_data_handle: Handle<StringValue>,
    ) -> AllocResult<Handle<StringObject>> {
        let mut object = ObjectBuilder::<StringObject>::new(cx)
            .proto(proto)
            .build()?;

        let string_data = *string_data_handle;
        let string_length = string_data.len();

        set_uninit!(object.string_data, string_data);

        let object = object.to_handle();

        Self::set_length_property(object, cx, string_length)?;

        Ok(object)
    }

    fn set_length_property(
        string: Handle<StringObject>,
        cx: Context,
        length: u32,
    ) -> AllocResult<()> {
        // String objects have an immutable length property
        let length_value = cx.number(length);
        string
            .as_object()
            .set_property(cx, cx.names.length(), Property::frozen(length_value))
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
    ) -> AllocResult<Option<Property>> {
        if key.is_symbol() {
            return Ok(None);
        }

        let string = self.string_data();
        let Some(index) = canonical_numeric_string_index_string(cx, key, string.len())? else {
            return Ok(None);
        };

        let code_unit = string.code_unit_at(index)?;
        let char_string = FlatString::from_code_unit(cx, code_unit)?.as_string();

        Ok(Some(Property::data(char_string.into(), PropertyFlags::empty().enumerable())))
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<StringObject> {
    /// [[GetOwnProperty]] (https://tc39.es/ecma262/#sec-string-exotic-objects-getownproperty-p)
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<Property>> {
        if let Some(property) = ordinary_get_own_property(cx, self.as_object(), key) {
            return Ok(Some(property));
        }

        Ok(self.string_get_own_property(cx, key)?)
    }

    /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-string-exotic-objects-defineownproperty-p-desc)
    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        let string_property = self.string_get_own_property(cx, key)?;
        if string_property.is_some() {
            let is_extensible = self.shape_ptr().is_extensible();
            Ok(is_compatible_property_descriptor(cx, is_extensible, desc, string_property)?)
        } else {
            ordinary_define_own_property(cx, self.as_object(), key, desc)
        }
    }

    /// [[OwnPropertyKeys]] (https://tc39.es/ecma262/#sec-string-exotic-objects-ownpropertykeys)
    fn own_property_keys(&self, mut cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        let mut keys: Vec<Handle<Value>> = vec![];

        let length = self.string_data.len();
        for i in 0..length {
            let index_string = cx.alloc_string(&i.to_string())?;
            keys.push(index_string.into());
        }

        ordinary_filtered_own_indexed_property_keys(cx, self.as_object(), &mut keys, |index| {
            index >= (length as usize)
        })?;

        ordinary_own_string_symbol_property_keys(self.as_object(), &mut keys);

        Ok(keys)
    }
}

impl HeapItem for StringObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<StringObject>()
    }

    fn visit_pointers(mut string_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        string_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut string_object.string_data);
    }
}
