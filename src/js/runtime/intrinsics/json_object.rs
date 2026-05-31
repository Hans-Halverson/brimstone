use std::collections::HashSet;

use crate::{
    common::wtf_8::Wtf8String,
    must,
    runtime::{
        abstract_operations::{
            call_object, create_data_property, create_data_property_or_throw,
            enumerable_own_property_names, length_of_array_like, KeyOrValue,
        },
        alloc_error::AllocResult,
        error::syntax_error,
        function::get_argument,
        get,
        intrinsics::rust_runtime::RuntimeFunction,
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        property::Property,
        string_parsing::StringLexer,
        to_string,
        type_utilities::{is_array, is_callable, to_integer_or_infinity, to_number},
        Context, EvalResult, Handle, PropertyKey, Realm, Value,
    },
};

use super::{intrinsics::Intrinsic, json_parser::parse_json, json_serializer::JSONSerializer};

/// The JSON Object (https://tc39.es/ecma262/#sec-json-object)
pub struct JSONObject;

impl JSONObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        object.intrinsic_func(cx, cx.names.parse(), RuntimeFunction::JSONObject_parse, 2, realm)?;
        object.intrinsic_func(
            cx,
            cx.names.stringify(),
            RuntimeFunction::JSONObject_stringify,
            3,
            realm,
        )?;

        // JSON [ @@toStringTag ] (https://tc39.es/ecma262/#sec-json-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let math_name_value = cx.names.json().as_string().into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(math_name_value, false, false, true),
        )?;

        Ok(object)
    }

    /// JSON.parse (https://tc39.es/ecma262/#sec-json.parse)
    pub fn parse(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let text_arg = get_argument(cx, arguments, 0);
        let text_string = to_string(cx, text_arg)?;

        let mut lexer = StringLexer::new(text_string)?;

        // First parse the entire JSON, does not allocate
        let json_value = if let Some(json_value) = parse_json(&mut lexer) {
            json_value
        } else {
            return syntax_error(cx, "JSON.parse parsed invalid JSON");
        };

        // Then convert to a JS value, which may allocate
        let value = json_value.to_js_value(cx)?;

        let reviver_arg = get_argument(cx, arguments, 1);
        if is_callable(reviver_arg) {
            let reviver = reviver_arg.as_object();

            let root = ordinary_object_create(cx)?;
            let root_name = cx.names.empty_string();
            must!(create_data_property_or_throw(cx, root, root_name, value));

            return Self::internalize_json_property(cx, root, root_name, reviver);
        }

        Ok(value)
    }

    /// InternalizeJSONProperty (https://tc39.es/ecma262/#sec-internalizejsonproperty)
    fn internalize_json_property(
        cx: Context,
        holder: Handle<ObjectValue>,
        holder_key: Handle<PropertyKey>,
        reviver: Handle<ObjectValue>,
    ) -> EvalResult<Handle<Value>> {
        let value = get(cx, holder, holder_key)?;

        if value.is_object() {
            let mut value = value.as_object();

            // Key is shared between iterations
            let mut key = PropertyKey::uninit().to_handle(cx);

            if is_array(cx, value.into())? {
                let length = length_of_array_like(cx, value)?;

                for i in 0..length {
                    key.replace(PropertyKey::from_u64(cx, i)?);

                    let new_element = Self::internalize_json_property(cx, value, key, reviver)?;

                    if new_element.is_undefined() {
                        value.delete(cx, key)?;
                    } else {
                        create_data_property(cx, value, key, new_element)?;
                    }
                }
            } else {
                let key_values = enumerable_own_property_names(cx, value, KeyOrValue::Key)?;
                for key_value in key_values {
                    key.replace(PropertyKey::from_value(cx, key_value)?);

                    let new_element = Self::internalize_json_property(cx, value, key, reviver)?;

                    if new_element.is_undefined() {
                        value.delete(cx, key)?;
                    } else {
                        create_data_property(cx, value, key, new_element)?;
                    }
                }
            }
        }

        let holder_key_value = holder_key.to_value(cx)?;

        call_object(cx, reviver, holder.into(), &[holder_key_value, value])
    }

    /// JSON.stringify (https://tc39.es/ecma262/#sec-json.stringify)
    pub fn stringify(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // Replacer arg may be a function or property list
        let replacer_arg = get_argument(cx, arguments, 1);

        let mut replacer_function = None;
        let mut property_list = None;

        if replacer_arg.is_object() {
            if is_callable(replacer_arg) {
                replacer_function = Some(replacer_arg.as_object())
            } else if is_array(cx, replacer_arg)? {
                // Use set and array to collect property keys but preserve insertion order
                let mut property_keys_set = HashSet::new();
                let mut property_keys = vec![];

                // Share key between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);

                let replacer_array = replacer_arg.as_object();
                let length = length_of_array_like(cx, replacer_array)?;

                for i in 0..length {
                    key.replace(PropertyKey::from_u64(cx, i)?);

                    // Property may be a number of string primitive
                    let array_element = get(cx, replacer_array, key)?;
                    if array_element.is_number() {
                        let string = must!(to_string(cx, array_element));
                        let property_key = PropertyKey::string_handle(cx, string)?;

                        if property_keys_set.insert(property_key) {
                            property_keys.push(property_key);
                        }
                    } else if array_element.is_string() {
                        let property_key =
                            PropertyKey::string_handle(cx, array_element.as_string())?;

                        if property_keys_set.insert(property_key) {
                            property_keys.push(property_key);
                        }
                    } else if array_element.is_object() {
                        // Property may be a number or string object
                        let array_element_object = array_element.as_object();
                        if array_element_object.is_number_object()
                            || array_element_object.is_string_object()
                        {
                            let string = to_string(cx, array_element_object.into())?;
                            let property_key = PropertyKey::string_handle(cx, string)?;

                            if property_keys_set.insert(property_key) {
                                property_keys.push(property_key);
                            }
                        }
                    }
                }

                let property_keys = property_keys.into_iter().collect();
                property_list = Some(property_keys);
            }
        }

        // Convert number and string object to primitive values
        let space_arg = get_argument(cx, arguments, 2);

        let space_value = if space_arg.is_object() {
            let space_object = space_arg.as_object();
            if space_object.is_number_object() {
                to_number(cx, space_object.into())?
            } else if space_object.is_string_object() {
                to_string(cx, space_object.into())?.into()
            } else {
                space_arg
            }
        } else {
            space_arg
        };

        // Convert space arg to a string gap
        let gap = if space_value.is_number() {
            let space_number = to_integer_or_infinity(cx, space_value)?;
            let space_number = f64::min(space_number, 10.0);
            if space_number < 1.0 {
                Wtf8String::new()
            } else {
                Wtf8String::from_string(" ".repeat(space_number as usize))
            }
        } else if space_value.is_string() {
            let space_string = space_value.as_string();
            if space_string.len() <= 10 {
                space_string.to_wtf8_string()?
            } else {
                space_string.substring(cx, 0, 10)?.to_wtf8_string()
            }
        } else {
            Wtf8String::new()
        };

        let wrapper = ordinary_object_create(cx)?;

        let value = get_argument(cx, arguments, 0);
        must!(create_data_property_or_throw(cx, wrapper, cx.names.empty_string(), value));

        let mut serializer = JSONSerializer::new(replacer_function, property_list, gap);
        if !serializer.serialize_json_property(cx, cx.names.empty_string(), wrapper)? {
            return Ok(cx.undefined());
        }

        Ok(serializer.build(cx)?.as_value())
    }
}
