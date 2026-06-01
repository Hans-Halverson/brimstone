use crate::{
    common::{unicode::is_surrogate_code_point, wtf_8::Wtf8String},
    must,
    runtime::{
        abstract_operations::{
            call, call_object, enumerable_own_property_names, get_v, length_of_array_like,
            KeyOrValue,
        },
        alloc_error::AllocResult,
        error::type_error,
        get,
        object_value::ObjectValue,
        string_value::StringValue,
        to_string,
        type_utilities::{is_array, is_callable, number_to_string, to_number},
        Context, EvalResult, Handle, PropertyKey,
    },
};

pub struct JSONSerializer {
    /// String that we are building
    builder: Wtf8String,

    stack: Vec<Handle<ObjectValue>>,
    replacer_function: Option<Handle<ObjectValue>>,
    property_list: Option<Vec<Handle<PropertyKey>>>,
    indent: u32,
    gap: Wtf8String,
}

impl JSONSerializer {
    pub fn new(
        replacer_function: Option<Handle<ObjectValue>>,
        property_list: Option<Vec<Handle<PropertyKey>>>,
        gap: Wtf8String,
    ) -> Self {
        JSONSerializer {
            builder: Wtf8String::new(),
            stack: vec![],
            replacer_function,
            property_list,
            indent: 0,
            gap,
        }
    }

    pub fn build(self, mut cx: Context) -> EvalResult<Handle<StringValue>> {
        Ok(cx.alloc_wtf8_string(&self.builder)?.as_string())
    }

    /// SerializeJSONProperty (https://tc39.es/ecma262/#sec-serializejsonproperty)
    ///
    /// Returns whether the property was successfully serialized. False is equivalent to returning
    /// undefined in the spec.
    pub fn serialize_json_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        holder: Handle<ObjectValue>,
    ) -> EvalResult<bool> {
        let mut value = get(cx, holder, key)?;

        // Call toJSON method if one is present
        if value.is_object() || value.is_bigint() {
            let to_json = get_v(cx, value, cx.names.to_json())?;
            if is_callable(to_json) {
                let key_value = key.to_value(cx)?;
                value = call(cx, to_json, value, &[key_value])?;
            }
        }

        // Call replacer function if one was provided
        if let Some(replacer_function) = self.replacer_function {
            let key_value = key.to_value(cx)?;
            value = call_object(cx, replacer_function, holder.into(), &[key_value, value])?;
        }

        // Convert primitive wrapper objects to their underlying primitive value
        if value.is_object() {
            let value_object = value.as_object();
            if let Some(raw_json_object) = value_object.as_raw_json_object() {
                // Raw JSON objects have the stored `rawJSON` property used as-is. Object is frozen
                // so we know that the `rawJSON` property will be a string, but assert it anyways
                // for safety.
                let raw_json = must!(get(cx, raw_json_object.as_object(), cx.names.raw_json()));

                assert!(raw_json.is_string());
                let raw_string = raw_json.as_string();

                self.builder.push_wtf8_str(&raw_string.to_wtf8_string()?);

                // We know the exact raw JSON for this entire subtree, so immediately return it
                return Ok(true);
            } else if value_object.is_number_object() {
                value = to_number(cx, value)?;
            } else if value_object.is_string_object() {
                value = to_string(cx, value)?.into();
            } else if let Some(boolean_object) = value_object.as_boolean_object() {
                value = cx.bool(boolean_object.boolean_data());
            } else if let Some(bigint_object) = value_object.as_bigint_object() {
                value = bigint_object.bigint_data().into()
            }
        }

        if value.is_null() {
            self.builder.push_str("null");
        } else if value.is_bool() {
            if value.as_bool() {
                self.builder.push_str("true");
            } else {
                self.builder.push_str("false");
            }
        } else if value.is_string() {
            self.serialize_json_string(value.as_string())?;
        } else if value.is_number() {
            let number = value.as_number();
            if number.is_finite() {
                // Inlined ToString on number
                self.builder.push_str(&number_to_string(number));
            } else {
                self.builder.push_str("null");
            }
        } else if value.is_bigint() {
            return type_error(cx, "BigInt value cannot be serialized to JSON");
        } else if value.is_object() && !is_callable(value) {
            if is_array(cx, value)? {
                self.serialize_json_array(cx, value.as_object())?;
            } else {
                self.serialize_json_object(cx, value.as_object())?;
            }
        } else {
            return Ok(false);
        }

        Ok(true)
    }

    /// QuoteJSONString (https://tc39.es/ecma262/#sec-quotejsonstring)
    fn serialize_json_string(&mut self, string: Handle<StringValue>) -> AllocResult<()> {
        self.builder.push_char('"');

        for code_point in string.iter_code_points()? {
            // Escape characters
            if code_point == '\x08' as u32 {
                self.builder.push_str("\\b")
            } else if code_point == '\x0C' as u32 {
                self.builder.push_str("\\f")
            } else if code_point == '\t' as u32 {
                self.builder.push_str("\\t")
            } else if code_point == '\n' as u32 {
                self.builder.push_str("\\n")
            } else if code_point == '\r' as u32 {
                self.builder.push_str("\\r")
            } else if code_point == '\"' as u32 {
                self.builder.push_str("\\\"")
            } else if code_point == '\\' as u32 {
                self.builder.push_str("\\\\")
            } else if code_point < 0x20 || is_surrogate_code_point(code_point) {
                // Must be serialized as a unicode escape
                self.builder.push_str(&format!("\\u{code_point:04x}"))
            } else {
                self.builder.push(code_point);
            }
        }

        self.builder.push_char('"');

        Ok(())
    }

    fn indent(&mut self) {
        for _ in 0..self.indent {
            self.builder.push_wtf8_str(&self.gap);
        }
    }

    fn check_for_cycle(&mut self, cx: Context, value: Handle<ObjectValue>) -> EvalResult<()> {
        let value_ptr = *value;
        let has_cycle = self
            .stack
            .iter()
            .any(|ancestor| ancestor.ptr_eq(&value_ptr));

        if has_cycle {
            type_error(cx, "cyclic object cannot be serialized to JSON")
        } else {
            Ok(())
        }
    }

    /// SerializeJSONObject (https://tc39.es/ecma262/#sec-serializejsonobject)
    fn serialize_json_object(
        &mut self,
        cx: Context,
        object: Handle<ObjectValue>,
    ) -> EvalResult<()> {
        self.check_for_cycle(cx, object)?;

        let mut keys = vec![];
        if let Some(property_list) = &self.property_list {
            keys.extend_from_slice(property_list);
        } else {
            let property_key_values = enumerable_own_property_names(cx, object, KeyOrValue::Key)?;
            for property_key_value in property_key_values {
                let property_key = PropertyKey::from_value(cx, property_key_value)?;
                keys.push(property_key.to_handle(cx));
            }
        };

        self.builder.push_char('{');

        self.stack.push(object);
        self.indent += 1;

        let mut has_property = false;

        for key in keys {
            let string_key = key.to_value(cx)?;
            debug_assert!(string_key.is_string());

            // Mark the string position to revert to if this property should not be added
            let undo_length = self.builder.len();

            // Insert a comma and indentations before all properties but the first
            if has_property {
                if self.gap.is_empty() {
                    self.builder.push_char(',');
                } else {
                    self.builder.push_str(",\n");
                    self.indent();
                }
            } else if !self.gap.is_empty() {
                // There may be a newline before the first property
                self.builder.push_char('\n');
                self.indent();
            }

            // Serialize the key and value separated by a colon
            self.serialize_json_string(string_key.as_string())?;

            self.builder.push_char(':');
            if !self.gap.is_empty() {
                self.builder.push_char(' ');
            }

            if self.serialize_json_property(cx, key, object)? {
                has_property = true;
            } else {
                // If we cannot serialize property, undo back to before this property was added
                self.builder.truncate(undo_length);
            }
        }

        self.indent -= 1;
        self.stack.pop();

        // There may be a newline after the last property
        if has_property && !self.gap.is_empty() {
            self.builder.push_char('\n');
            self.indent();
        }

        self.builder.push_char('}');

        Ok(())
    }

    /// SerializeJSONArray (https://tc39.es/ecma262/#sec-serializejsonarray)
    fn serialize_json_array(&mut self, cx: Context, array: Handle<ObjectValue>) -> EvalResult<()> {
        self.check_for_cycle(cx, array)?;

        self.builder.push_char('[');

        self.stack.push(array);
        self.indent += 1;

        // Share key between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        let length = length_of_array_like(cx, array)?;
        for i in 0..length {
            key.replace(PropertyKey::from_u64(cx, i)?);

            // Insert a comma and indentations before all elements but the first
            if i != 0 {
                if self.gap.is_empty() {
                    self.builder.push_char(',');
                } else {
                    self.builder.push_str(",\n");
                    self.indent();
                }
            } else if !self.gap.is_empty() {
                // There may be a newline before the first element
                self.builder.push_char('\n');
                self.indent();
            }

            // Serialize each element, replacing with null if it cannot be serialized
            if !self.serialize_json_property(cx, key, array)? {
                self.builder.push_str("null");
            }
        }

        self.indent -= 1;
        self.stack.pop();

        // There may be a newline after the last element
        if length != 0 && !self.gap.is_empty() {
            self.builder.push_char('\n');
            self.indent();
        }

        self.builder.push_char(']');

        Ok(())
    }
}
