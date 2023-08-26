use crate::{
    js::{
        common::wtf_8::Wtf8String,
        runtime::{
            abstract_operations::{
                call_object, create_data_property, create_data_property_or_throw,
                enumerable_own_property_names, length_of_array_like, KeyOrValue,
            },
            array_object::array_create,
            error::syntax_error_,
            function::get_argument,
            get,
            object_value::ObjectValue,
            ordinary_object::ordinary_object_create,
            property::Property,
            string_parsing::{parse_signed_decimal_literal, StringLexer},
            to_string,
            type_utilities::{is_array, is_callable},
            Context, EvalResult, Handle, PropertyKey, Realm, Value,
        },
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

// 25.5 The JSON Object
pub struct JSONObject;

impl JSONObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        object.intrinsic_func(cx, cx.names.parse(), Self::parse, 2, realm);

        // 25.5.3 JSON [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let math_name_value = cx.names.json().as_string().into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(math_name_value, false, false, true),
        );

        object
    }

    // 25.5.1 JSON.parse
    fn parse(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let text_arg = get_argument(cx, arguments, 0);
        let text_string = maybe!(to_string(cx, text_arg));

        let mut lexer = StringLexer::new(text_string);

        // First parse the entire JSON, does not allocate
        let json_value = if let Some(json_value) = parse_json(&mut lexer) {
            json_value
        } else {
            return syntax_error_(cx, "JSON.parse: Invalid JSON");
        };

        // Then convert to a JS value, which may allocate
        let value = json_value.to_js_value(cx);

        let reviver_arg = get_argument(cx, arguments, 1);
        if is_callable(reviver_arg) {
            let reviver = reviver_arg.as_object();

            let root = ordinary_object_create(cx);
            let root_name = cx.names.empty_string();
            must!(create_data_property_or_throw(cx, root, root_name, value));

            return Self::internalize_json_property(cx, root, root_name, reviver);
        }

        value.into()
    }

    // 25.5.1.1 InternalizeJSONProperty
    fn internalize_json_property(
        cx: Context,
        holder: Handle<ObjectValue>,
        holder_key: Handle<PropertyKey>,
        reviver: Handle<ObjectValue>,
    ) -> EvalResult<Handle<Value>> {
        let value = maybe!(get(cx, holder, holder_key));

        if value.is_object() {
            let mut value = value.as_object();

            // Key is shared between iterations
            let mut key = PropertyKey::uninit().to_handle(cx);

            if maybe!(is_array(cx, value.into())) {
                let length = maybe!(length_of_array_like(cx, value));

                for i in 0..length {
                    key.replace(PropertyKey::from_u64(cx, i));

                    let new_element =
                        maybe!(Self::internalize_json_property(cx, value, key, reviver));

                    if new_element.is_undefined() {
                        maybe!(value.delete(cx, key));
                    } else {
                        maybe!(create_data_property(cx, value, key, new_element));
                    }
                }
            } else {
                let key_values = maybe!(enumerable_own_property_names(cx, value, KeyOrValue::Key));
                for key_value in key_values {
                    key.replace(maybe!(PropertyKey::from_value(cx, key_value)));

                    let new_element =
                        maybe!(Self::internalize_json_property(cx, value, key, reviver));

                    if new_element.is_undefined() {
                        maybe!(value.delete(cx, key));
                    } else {
                        maybe!(create_data_property(cx, value, key, new_element));
                    }
                }
            }
        }

        let holder_key_value = holder_key.to_value(cx);

        call_object(cx, reviver, holder.into(), &[holder_key_value, value])
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
    pub fn to_js_value(&self, mut cx: Context) -> Handle<Value> {
        match self {
            Self::Null => cx.null(),
            Self::Boolean(b) => cx.bool(*b),
            Self::Number(n) => Value::from(*n).to_handle(cx),
            Self::String(s) => cx.alloc_wtf8_string_ptr(s).as_string().to_handle().into(),
            Self::Array(values) => {
                let array = must!(array_create(cx, 0, None));

                // Key is shared between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);

                for (i, value) in values.iter().enumerate() {
                    key.replace(PropertyKey::from_u64(cx, i as u64));
                    let desc = Property::data(value.to_js_value(cx), true, true, true);
                    array.object().set_property(cx, key, desc);
                }

                array.into()
            }
            Self::Object(properties) => {
                let object = ordinary_object_create(cx);

                // Key is shared between iterations
                let mut key_value: Handle<Value> = Handle::empty(cx);
                let mut key = PropertyKey::uninit().to_handle(cx);

                for (key_string, value) in properties {
                    key_value.replace(cx.alloc_wtf8_string_ptr(key_string).as_string().into());
                    key.replace(PropertyKey::string(cx, key_value.as_string()));

                    let value = value.to_js_value(cx);

                    must!(create_data_property_or_throw(cx, object, key, value));
                }

                object.into()
            }
        }
    }
}
