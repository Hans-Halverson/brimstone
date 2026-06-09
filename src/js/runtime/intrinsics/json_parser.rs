use crate::{
    common::wtf_8::Wtf8String,
    must_a,
    parser::loc::{Loc, Pos},
    runtime::{
        Context, EvalResult, Handle, PropertyKey, Value,
        abstract_operations::create_data_property_or_throw,
        array_object::array_create,
        ordinary_object::ordinary_object_create,
        property::Property,
        string_parsing::{StringLexer, parse_between_ptrs_to_f64, skip_decimal_digits},
        string_value::StringValue,
    },
};

/// Simple AST for parsed JSON values.
pub enum JSONValue {
    Object { properties: Vec<(Wtf8String, JSONValue)>, loc: Loc },
    Array { elements: Vec<JSONValue>, loc: Loc },
    String { value: Wtf8String, loc: Loc },
    Number { value: f64, loc: Loc },
    Boolean { value: bool, loc: Loc },
    Null { loc: Loc },
}

/// The generated values from parsing JSON but in their original tree structure and with location
/// information. This is needed for source text access in JSON.parse's reviver function.
///
/// The original tree structure is needed since the structure of the parsed JSON value may be
/// changed during parsing when the reviver function is called.
pub struct JSONParseRecord {
    pub value: Handle<Value>,
    pub loc: Loc,
    pub children: Option<JSONParseRecordChildren>,
}

pub enum JSONParseRecordChildren {
    Elements(Vec<JSONParseRecord>),
    Properties(Vec<(Handle<PropertyKey>, JSONParseRecord)>),
}

pub fn parse_json(source_text: Handle<StringValue>) -> EvalResult<Option<JSONValue>> {
    let mut lexer = StringLexer::new(source_text)?;

    // Could have leading whitespace
    skip_json_whitespace(&mut lexer);

    let Some(value) = parse_json_value(&mut lexer) else {
        return Ok(None);
    };

    // Could have trailing whitespace
    skip_json_whitespace(&mut lexer);

    // Must be end of input otherwise is invalid JSON
    if !lexer.is_end() {
        return Ok(None);
    }

    Ok(Some(value))
}

fn parse_json_value(lexer: &mut StringLexer) -> Option<JSONValue> {
    let start_pos = lexer.pos();
    if lexer.eat('{') {
        parse_json_object(lexer, start_pos)
    } else if lexer.eat('[') {
        parse_json_array(lexer, start_pos)
    } else if lexer.eat('"') {
        let string_value = parse_json_string(lexer)?;
        let loc = lexer.mark_loc(start_pos);
        Some(JSONValue::String { value: string_value, loc })
    } else if lexer.eat('t') {
        lexer.expect('r')?;
        lexer.expect('u')?;
        lexer.expect('e')?;

        let loc = lexer.mark_loc(start_pos);

        Some(JSONValue::Boolean { value: true, loc })
    } else if lexer.eat('f') {
        lexer.expect('a')?;
        lexer.expect('l')?;
        lexer.expect('s')?;
        lexer.expect('e')?;

        let loc = lexer.mark_loc(start_pos);

        Some(JSONValue::Boolean { value: false, loc })
    } else if lexer.eat('n') {
        lexer.expect('u')?;
        lexer.expect('l')?;
        lexer.expect('l')?;

        let loc = lexer.mark_loc(start_pos);

        Some(JSONValue::Null { loc })
    } else if lexer.current_is_decimal_digit() || lexer.current_equals('-') {
        parse_json_number(lexer, start_pos)
    } else {
        None
    }
}

fn skip_json_whitespace(lexer: &mut StringLexer) {
    while lexer.current_is_json_whitespace() {
        lexer.advance();
    }
}

fn parse_json_object(lexer: &mut StringLexer, start_pos: Pos) -> Option<JSONValue> {
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

    let loc = lexer.mark_loc(start_pos);

    Some(JSONValue::Object { properties, loc })
}

fn parse_json_array(lexer: &mut StringLexer, start_pos: Pos) -> Option<JSONValue> {
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

    let loc = lexer.mark_loc(start_pos);

    Some(JSONValue::Array { elements, loc })
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

/// Parse a JSON number following the syntax specified in ECMA-404
fn parse_json_number(lexer: &mut StringLexer, start_pos: Pos) -> Option<JSONValue> {
    let start_ptr = lexer.current_ptr();

    // ECMA-404 allows a single leading minus but no leading plus
    let has_sign = lexer.eat('-');

    // Sign must be followed by a digit
    if has_sign && !lexer.current_is_decimal_digit() {
        return None;
    }

    // ECMA-404 disallows leading zeros
    if lexer.current_equals('0') && matches!(lexer.peek_ascii_char(), Some('0'..='9')) {
        return None;
    }

    // Parse digits around optional dot
    let has_digits_before_dot = skip_decimal_digits(lexer);
    let has_dot = lexer.eat('.');
    let has_digits_after_dot = skip_decimal_digits(lexer);

    // ECMA-404 requires that there be a digit on both sides of the dot
    if has_dot && !(has_digits_before_dot && has_digits_after_dot) {
        return None;
    }

    // Parse optional exponent if the exponent starts with a valid exponent character.
    if (lexer.current_equals('e') || lexer.current_equals('E'))
        && matches!(lexer.peek_ascii_char(), Some('+' | '-' | '0'..='9'))
    {
        lexer.advance();

        // Optional sign in exponent
        if lexer.current_equals('-') || lexer.current_equals('+') {
            lexer.advance();
        }

        // Exponent must have some digits
        let has_digits_in_exponent = skip_decimal_digits(lexer);
        if !has_digits_in_exponent {
            return None;
        }
    }

    let end_ptr = lexer.current_ptr();
    let number = parse_between_ptrs_to_f64(lexer, start_ptr, end_ptr);

    let loc = lexer.mark_loc(start_pos);

    Some(JSONValue::Number { value: number, loc })
}

impl JSONValue {
    pub fn to_js_value(&self, mut cx: Context) -> EvalResult<Handle<Value>> {
        let value = match self {
            Self::Null { .. } => cx.null(),
            Self::Boolean { value, .. } => cx.bool(*value),
            Self::Number { value, .. } => Value::from(*value).to_handle(cx),
            Self::String { value, .. } => cx.alloc_wtf8_string(value)?.into(),
            Self::Array { elements, .. } => {
                let array = must_a!(array_create(cx, 0, None));

                // Key is shared between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);

                for (i, value) in elements.iter().enumerate() {
                    key.replace(PropertyKey::from_u64(cx, i as u64)?);
                    let desc = Property::data(value.to_js_value(cx)?, true, true, true);
                    array.as_object().set_property(cx, key, desc)?;
                }

                array.into()
            }
            Self::Object { properties, .. } => {
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

    pub fn to_json_parse_record(&self, mut cx: Context) -> EvalResult<JSONParseRecord> {
        match self {
            Self::Null { loc } => {
                let null_value = cx.null();
                Ok(JSONParseRecord::new(null_value, *loc))
            }
            Self::Boolean { value, loc } => {
                let bool_value = cx.bool(*value);
                Ok(JSONParseRecord::new(bool_value, *loc))
            }
            Self::Number { value, loc } => {
                let number_value = Value::from(*value).to_handle(cx);
                Ok(JSONParseRecord::new(number_value, *loc))
            }
            Self::String { value, loc } => {
                let string_value = cx.alloc_wtf8_string(value)?.as_value();
                Ok(JSONParseRecord::new(string_value, *loc))
            }
            Self::Array { elements, loc } => {
                let array = must_a!(array_create(cx, 0, None));

                // Key is shared between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);
                let mut record_elements = vec![];

                for (i, json_value) in elements.iter().enumerate() {
                    key.replace(PropertyKey::from_u64(cx, i as u64)?);

                    let parse_record = json_value.to_json_parse_record(cx)?;
                    let value = parse_record.value;
                    record_elements.push(parse_record);

                    let desc = Property::data(value, true, true, true);
                    array.as_object().set_property(cx, key, desc)?;
                }

                Ok(JSONParseRecord::new_array(array.as_value(), *loc, record_elements))
            }
            Self::Object { properties, loc } => {
                let object = ordinary_object_create(cx)?;

                let mut record_properties = vec![];

                for (key_string, json_value) in properties {
                    let key_value = cx
                        .alloc_wtf8_string_ptr(key_string)?
                        .as_string()
                        .to_handle();
                    let key = PropertyKey::string(cx, key_value)?.to_handle(cx);

                    let value_record = json_value.to_json_parse_record(cx)?;
                    let value = value_record.value;
                    record_properties.push((key, value_record));

                    must_a!(create_data_property_or_throw(cx, object, key, value));
                }

                Ok(JSONParseRecord::new_object(object.as_value(), *loc, record_properties))
            }
        }
    }
}

impl JSONParseRecord {
    pub fn new(value: Handle<Value>, loc: Loc) -> Self {
        Self { value, loc, children: None }
    }

    pub fn new_object(
        value: Handle<Value>,
        loc: Loc,
        properties: Vec<(Handle<PropertyKey>, JSONParseRecord)>,
    ) -> Self {
        Self {
            value,
            loc,
            children: Some(JSONParseRecordChildren::Properties(properties)),
        }
    }

    pub fn new_array(value: Handle<Value>, loc: Loc, elements: Vec<JSONParseRecord>) -> Self {
        Self {
            value,
            loc,
            children: Some(JSONParseRecordChildren::Elements(elements)),
        }
    }
}
