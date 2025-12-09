use std::collections::HashSet;

use crate::{
    common::{unicode::is_surrogate_code_point, wtf_8::Wtf8String},
    must, must_a,
    runtime::{
        abstract_operations::{
            call, call_object, create_data_property, create_data_property_or_throw,
            enumerable_own_property_names, get_v, length_of_array_like, KeyOrValue,
        },
        alloc_error::AllocResult,
        array_object::array_create,
        error::{syntax_error, type_error},
        function::get_argument,
        get,
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        property::Property,
        string_parsing::{parse_signed_decimal_literal, StringLexer},
        string_value::StringValue,
        to_string,
        type_utilities::{
            is_array, is_callable, number_to_string, to_integer_or_infinity, to_number,
        },
        Context, EvalResult, Handle, PropertyKey, Realm, Value,
    },
};

use super::intrinsics::Intrinsic;

/// The JSON Object (https://tc39.es/ecma262/#sec-json-object)
pub struct JSONObject;

impl JSONObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        object.intrinsic_func(cx, cx.names.parse(), Self::parse, 2, realm)?;
        object.intrinsic_func(cx, cx.names.stringify(), Self::stringify, 3, realm)?;

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
            return syntax_error(cx, "JSON.parse: Invalid JSON");
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

enum JSONValue {
    Object(Vec<(Wtf8String, JSONValue)>),
    Array(Vec<JSONValue>),
    String(Wtf8String),
    Number(f64),
    Boolean(bool),
    Null,
}

fn parse_json(lexer: &mut StringLexer) -> Option<JSONValue> {
    // Could have leading whitespace
    skip_json_whitespace(lexer);

    let value = parse_json_value(lexer)?;

    // Could have trailing whitespace
    skip_json_whitespace(lexer);

    // Must be end of input otherwise is invalid JSON
    if !lexer.is_end() {
        return None;
    }

    Some(value)
}

fn parse_json_value(lexer: &mut StringLexer) -> Option<JSONValue> {
    if lexer.eat('{') {
        parse_json_object(lexer)
    } else if lexer.eat('[') {
        parse_json_array(lexer)
    } else if lexer.eat('"') {
        let string_value = parse_json_string(lexer)?;
        Some(JSONValue::String(string_value))
    } else if lexer.eat('t') {
        lexer.expect('r')?;
        lexer.expect('u')?;
        lexer.expect('e')?;

        Some(JSONValue::Boolean(true))
    } else if lexer.eat('f') {
        lexer.expect('a')?;
        lexer.expect('l')?;
        lexer.expect('s')?;
        lexer.expect('e')?;

        Some(JSONValue::Boolean(false))
    } else if lexer.eat('n') {
        lexer.expect('u')?;
        lexer.expect('l')?;
        lexer.expect('l')?;

        Some(JSONValue::Null)
    } else if lexer.current_is_decimal_digit()
        || lexer.current_equals('-')
        || lexer.current_equals('+')
    {
        // JSON numbers do not allow leading zeros
        if lexer.current_equals('0') && lexer.peek_ascii_char() == Some('0') {
            return None;
        }

        let number = parse_signed_decimal_literal(lexer)?;
        Some(JSONValue::Number(number))
    } else {
        None
    }
}

fn skip_json_whitespace(lexer: &mut StringLexer) {
    while lexer.current_is_json_whitespace() {
        lexer.advance();
    }
}

fn parse_json_object(lexer: &mut StringLexer) -> Option<JSONValue> {
    let mut properties = vec![];

    // Consume leading whitespace before the next character (key or closing brace)
    skip_json_whitespace(lexer);

    while !lexer.current_equals('}') && !lexer.is_end() {
        // Parse the string key
        lexer.expect('"')?;
        let key_string = parse_json_string(lexer)?;

        // Key separated by value by colon
        skip_json_whitespace(lexer);
        lexer.expect(':')?;

        // Parse the value
        skip_json_whitespace(lexer);
        let value = parse_json_value(lexer)?;

        // Add the key value pair to the object
        properties.push((key_string, value));

        // There may be whitespace after the property and before the comma/closing brace
        skip_json_whitespace(lexer);

        if !lexer.eat(',') {
            break;
        }

        // There may be whitespace after the comma and before the next property
        skip_json_whitespace(lexer);

        // Trailing comma is not allowed
        if lexer.current_equals('}') {
            return None;
        }
    }

    lexer.expect('}')?;

    Some(JSONValue::Object(properties))
}

fn parse_json_array(lexer: &mut StringLexer) -> Option<JSONValue> {
    let mut elements = vec![];

    // Consume leading whitespace before the next character (value or closing bracket)
    skip_json_whitespace(lexer);

    while !lexer.current_equals(']') && !lexer.is_end() {
        // Parse the array element
        let element = parse_json_value(lexer)?;
        elements.push(element);

        // There may be whitespace after a value and before the comma/closing bracket
        skip_json_whitespace(lexer);

        if !lexer.eat(',') {
            break;
        }

        // There may be whitespace after the comma and before the next value
        skip_json_whitespace(lexer);

        // Trailing comma is not allowed
        if lexer.current_equals(']') {
            return None;
        }
    }

    lexer.expect(']')?;

    Some(JSONValue::Array(elements))
}

fn parse_json_string(lexer: &mut StringLexer) -> Option<Wtf8String> {
    let mut string = Wtf8String::new();

    while !lexer.current_equals('"') && !lexer.is_end() {
        // Escape sequence which must be decoded
        if lexer.eat('\\') {
            if lexer.current_equals('"') {
                string.push_char('"');
                lexer.advance();
            } else if lexer.current_equals('\\') {
                string.push_char('\\');
                lexer.advance();
            } else if lexer.current_equals('/') {
                string.push_char('/');
                lexer.advance();
            } else if lexer.current_equals('b') {
                string.push_char('\x08');
                lexer.advance();
            } else if lexer.current_equals('f') {
                string.push_char('\x0C');
                lexer.advance();
            } else if lexer.current_equals('n') {
                string.push_char('\n');
                lexer.advance();
            } else if lexer.current_equals('r') {
                string.push_char('\r');
                lexer.advance();
            } else if lexer.current_equals('t') {
                string.push_char('\t');
                lexer.advance();
            } else if lexer.eat('u') {
                let mut code_point = 0;

                for _ in 0..4 {
                    code_point <<= 4;
                    code_point += lexer.current_hex_value()?;

                    lexer.advance();
                }

                // Control characters are not allowed
                if code_point <= 0x1F {
                    return None;
                }

                string.push(code_point)
            } else {
                return None;
            }
        } else if let Some(code_unit) = lexer.current() {
            // Control characters are not allowed
            if code_unit <= 0x1F {
                return None;
            }

            // Otherwise is a regular code unit which can be directly added to string
            string.push(code_unit as u32);
            lexer.advance();
        } else {
            return None;
        }
    }

    lexer.expect('"')?;

    Some(string)
}

impl JSONValue {
    pub fn to_js_value(&self, mut cx: Context) -> AllocResult<Handle<Value>> {
        let value = match self {
            Self::Null => cx.null(),
            Self::Boolean(b) => cx.bool(*b),
            Self::Number(n) => Value::from(*n).to_handle(cx),
            Self::String(s) => cx.alloc_wtf8_string(s)?.into(),
            Self::Array(values) => {
                let array = must_a!(array_create(cx, 0, None));

                // Key is shared between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);

                for (i, value) in values.iter().enumerate() {
                    key.replace(PropertyKey::from_u64(cx, i as u64)?);
                    let desc = Property::data(value.to_js_value(cx)?, true, true, true);
                    array.as_object().set_property(cx, key, desc)?;
                }

                array.into()
            }
            Self::Object(properties) => {
                let object = ordinary_object_create(cx)?;

                // Key is shared between iterations
                let mut key_value: Handle<Value> = Handle::empty(cx);
                let mut key = PropertyKey::uninit().to_handle(cx);

                for (key_string, value) in properties {
                    key_value.replace(cx.alloc_wtf8_string_ptr(key_string)?.as_string().into());
                    key.replace(PropertyKey::string(cx, key_value.as_string())?);

                    let value = value.to_js_value(cx)?;

                    must_a!(create_data_property_or_throw(cx, object, key, value));
                }

                object.into()
            }
        };

        Ok(value)
    }
}

struct JSONSerializer {
    /// String that we are building
    builder: Wtf8String,

    stack: Vec<Handle<ObjectValue>>,
    replacer_function: Option<Handle<ObjectValue>>,
    property_list: Option<Vec<Handle<PropertyKey>>>,
    indent: u32,
    gap: Wtf8String,
}

impl JSONSerializer {
    fn new(
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

    fn build(self, mut cx: Context) -> AllocResult<Handle<StringValue>> {
        Ok(cx.alloc_wtf8_string(&self.builder)?.as_string())
    }

    /// SerializeJSONProperty (https://tc39.es/ecma262/#sec-serializejsonproperty)
    ///
    /// Returns whether the property was successfully serialized. False is equivalent to returning
    /// undefined in the spec.
    fn serialize_json_property(
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
            if value_object.is_number_object() {
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
            return type_error(cx, "BigInt value can't be serialized to JSON");
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
            type_error(cx, "Cyclic object can't be serialized to JSON")
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
