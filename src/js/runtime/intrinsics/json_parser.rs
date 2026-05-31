use crate::{
    common::wtf_8::Wtf8String,
    must_a,
    runtime::{
        abstract_operations::create_data_property_or_throw,
        array_object::array_create,
        ordinary_object::ordinary_object_create,
        property::Property,
        string_parsing::{parse_signed_decimal_literal, StringLexer},
        Context, EvalResult, Handle, PropertyKey, Value,
    },
};

pub enum JSONValue {
    Object(Vec<(Wtf8String, JSONValue)>),
    Array(Vec<JSONValue>),
    String(Wtf8String),
    Number(f64),
    Boolean(bool),
    Null,
}

pub fn parse_json(lexer: &mut StringLexer) -> Option<JSONValue> {
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
    pub fn to_js_value(&self, mut cx: Context) -> EvalResult<Handle<Value>> {
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
