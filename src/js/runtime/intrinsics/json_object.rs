use std::collections::HashSet;

use crate::runtime::intrinsics::number_object::NumberObject;
use crate::runtime::string_object::StringObject;
use crate::{
    common::{
        unicode::{CodePoint, CodeUnit, is_ascii_lowercase_alphabetic, is_decimal_digit},
        wtf_8::Wtf8String,
    },
    intrinsic_methods, must,
    runtime::{
        Context, EvalResult, Handle, PropertyKey, Realm, Value,
        abstract_operations::{
            KeyOrValue, call_object, create_data_property, create_data_property_or_throw,
            enumerable_own_property_names, length_of_array_like,
        },
        alloc_error::AllocResult,
        error::syntax_error,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            json_parser::{JSONParseRecord, JSONParseRecordChildren, parse_json},
            json_serializer::JSONSerializer,
            raw_json_object::RawJSONObject,
        },
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        string_value::FlatString,
        to_string,
        type_utilities::{is_array, is_callable, same_value, to_integer_or_infinity, to_number},
    },
    runtime_fn,
};

/// The JSON Object (https://tc39.es/ecma262/#sec-json-object)
pub struct JSONObject;

impl JSONObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        intrinsic_methods!(cx, builder, {
            is_raw_json JSONObject_is_raw_json (1),
            parse       JSONObject_parse       (2),
            raw_json    JSONObject_raw_json    (1),
            stringify   JSONObject_stringify   (3),
        });

        // JSON [ @@toStringTag ] (https://tc39.es/ecma262/#sec-json-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.json())?;

        builder.build()
    }

    runtime_fn! {
    /// JSON.isRawJSON (https://tc39.es/ecma262/#sec-json.israwjson)
    fn is_raw_json(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let is_raw_json = argument.is::<RawJSONObject>();

        Ok(cx.bool(is_raw_json))
    }}

    runtime_fn! {
    /// JSON.parse (https://tc39.es/ecma262/#sec-json.parse)
    fn parse(cx, _, arguments) {
        let text_arg = arguments.get(cx, 0);
        let reviver_arg = arguments.get(cx, 1);

        json_parse(cx, text_arg, reviver_arg)
    }}

    runtime_fn! {
    /// JSON.rawJSON (https://tc39.es/ecma262/#sec-json.rawjson)
    fn raw_json(cx, _, arguments) {
        let text_argument = arguments.get(cx, 0);
        let text_string = to_string(cx, text_argument)?;

        if text_string.is_empty() {
            return syntax_error(cx, "JSON.rawJSON argument must not be the empty string");
        }

        let first_code_unit = text_string.code_unit_at(0)?;
        let last_code_unit = text_string.code_unit_at(text_string.len() - 1)?;

        if !Self::is_raw_json_start_code_unit(first_code_unit) {
            return syntax_error(cx, "JSON.rawJSON argument has unexpected first code unit");
        }

        if !Self::is_raw_json_end_code_unit(last_code_unit) {
            return syntax_error(cx, "JSON.rawJSON argument has unexpected last code unit");
        }

        let has_successful_parse = parse_json(text_string)?.is_some();

        if !has_successful_parse {
            return syntax_error(cx, "JSON.rawJSON argument is not valid JSON");
        }

        let raw_json_object = RawJSONObject::new(cx, text_string)?;

        Ok(raw_json_object.as_value())
    }}

    fn is_raw_json_start_code_unit(code_unit: CodeUnit) -> bool {
        is_ascii_lowercase_alphabetic(code_unit as CodePoint)
            || is_decimal_digit(code_unit as CodePoint)
            || code_unit == '\"' as CodeUnit
            || code_unit == '-' as CodeUnit
    }

    fn is_raw_json_end_code_unit(code_unit: CodeUnit) -> bool {
        is_ascii_lowercase_alphabetic(code_unit as CodePoint)
            || is_decimal_digit(code_unit as CodePoint)
            || code_unit == '\"' as CodeUnit
    }

    runtime_fn! {
    /// JSON.stringify (https://tc39.es/ecma262/#sec-json.stringify)
    fn stringify(cx, _, arguments) {
        // Replacer arg may be a function or property list
        let replacer_arg = arguments.get(cx, 1);

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
                    } else if array_element.is::<NumberObject>()
                        || array_element.is::<StringObject>()
                    {
                        // Property may be a number or string object
                        let string = to_string(cx, array_element)?;
                        let property_key = PropertyKey::string_handle(cx, string)?;

                        if property_keys_set.insert(property_key) {
                            property_keys.push(property_key);
                        }
                    }
                }

                let property_keys = property_keys.into_iter().collect();
                property_list = Some(property_keys);
            }
        }

        // Convert number and string object to primitive values
        let space_arg = arguments.get(cx, 2);

        let space_value = if space_arg.is::<NumberObject>() {
            to_number(cx, space_arg)?
        } else if space_arg.is::<StringObject>() {
            to_string(cx, space_arg)?.into()
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

        let value = arguments.get(cx, 0);
        must!(create_data_property_or_throw(cx, wrapper, cx.names.empty_string(), value));

        let mut serializer = JSONSerializer::new(replacer_function, property_list, gap);
        if !serializer.serialize_json_property(cx, cx.names.empty_string(), wrapper)? {
            return Ok(cx.undefined());
        }

        Ok(serializer.build(cx)?.as_value())
    }}
}

/// Implementation of JSON.parse with the given text and reviver argument, which may be undefined.
pub fn json_parse(
    cx: Context,
    text: Handle<Value>,
    reviver: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let text_string = to_string(cx, text)?;

    // Parse JSON into an AST, not yet allocated on heap
    let json_value = if let Some(json_value) = parse_json(text_string)? {
        json_value
    } else {
        return syntax_error(cx, "JSON.parse parsed invalid JSON");
    };

    // If reviver argument is provided then we must create a JSONParseRecord so that the
    // initially parsed structure can be associated with the resulting values.
    if is_callable(reviver) {
        let reviver = reviver.as_object();

        let parse_record = json_value.to_json_parse_record(cx)?;

        let root = ordinary_object_create(cx)?;
        let root_name = cx.names.empty_string();
        must!(create_data_property_or_throw(cx, root, root_name, parse_record.value));

        let flat_text_string = text_string.flatten()?;

        return internalize_json_property(
            cx,
            root,
            root_name,
            reviver,
            flat_text_string,
            Some(&parse_record),
        );
    }

    // Otherwise reviver not needed so create JSON value on the heap
    json_value.to_js_value(cx)
}

/// InternalizeJSONProperty (https://tc39.es/ecma262/#sec-internalizejsonproperty)
fn internalize_json_property(
    cx: Context,
    holder: Handle<ObjectValue>,
    holder_key: Handle<PropertyKey>,
    reviver: Handle<ObjectValue>,
    source_text: Handle<FlatString>,
    parse_record: Option<&JSONParseRecord>,
) -> EvalResult<Handle<Value>> {
    let value = get(cx, holder, holder_key)?;

    let context = ordinary_object_create(cx)?;

    // Literal values attach source text to the context object to be passed to the reviver
    if let Some(parse_record) = &parse_record {
        if !value.is_object() {
            if same_value(value, parse_record.value)? {
                let loc = parse_record.loc;

                // Source text was stored as byte indices in the parse record
                let source_string = source_text
                    .substring_by_byte_index(cx, loc.start, loc.end)?
                    .to_handle();
                let source = source_string.as_string().as_value();

                must!(create_data_property_or_throw(cx, context, cx.names.source(), source));
            }
        }
    }

    if value.is_object() {
        let mut value = value.as_object();

        // Key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        if is_array(cx, value.into())? {
            // Find matching element records if present so they can be passed down
            let mut element_records: &[JSONParseRecord] = &[];
            if let Some(parse_record) = &parse_record {
                if let Some(JSONParseRecordChildren::Elements(elements)) = &parse_record.children {
                    element_records = elements.as_slice();
                }
            }

            let length = length_of_array_like(cx, value)?;

            for i in 0..length {
                key.replace(PropertyKey::from_u64(cx, i)?);

                // Find the matching element record for this index, if in bounds
                let element_record = element_records.get(i as usize);

                let new_element = internalize_json_property(
                    cx,
                    value,
                    key,
                    reviver,
                    source_text,
                    element_record,
                )?;

                if new_element.is_undefined() {
                    value.delete(cx, key)?;
                } else {
                    create_data_property(cx, value, key, new_element)?;
                }
            }
        } else {
            // Find matching property records if present so they can be passed down
            let mut property_records: &[(Handle<PropertyKey>, JSONParseRecord)] = &[];
            if let Some(parse_record) = &parse_record {
                if let Some(JSONParseRecordChildren::Properties(properties)) =
                    &parse_record.children
                {
                    property_records = properties.as_slice();
                }
            }

            let key_values = enumerable_own_property_names(cx, value, KeyOrValue::Key)?;
            for key_value in key_values {
                key.replace(PropertyKey::from_value(cx, key_value)?);

                // Find the matching property record value for this key, if one exists
                let property_record =
                    property_records
                        .iter()
                        .find_map(|(record_key, record_value)| {
                            if *record_key == key {
                                Some(record_value)
                            } else {
                                None
                            }
                        });

                let new_element = internalize_json_property(
                    cx,
                    value,
                    key,
                    reviver,
                    source_text,
                    property_record,
                )?;

                if new_element.is_undefined() {
                    value.delete(cx, key)?;
                } else {
                    create_data_property(cx, value, key, new_element)?;
                }
            }
        }
    }

    let holder_key_value = holder_key.to_value(cx)?;

    call_object(cx, reviver, holder.into(), &[holder_key_value, value, context.as_value()])
}
