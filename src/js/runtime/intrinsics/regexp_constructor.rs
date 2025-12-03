use std::mem::size_of;

use brimstone_macros::match_u32;
use bumpalo::Bump;

use crate::{
    common::{
        string::StringWidth,
        unicode::{
            is_ascii_alphabetic, is_decimal_digit, is_latin1, is_newline, is_surrogate_code_point,
            is_whitespace,
        },
        wtf_8::Wtf8String,
    },
    extend_object, must,
    parser::{
        ast::AstAlloc,
        lexer_stream::{
            HeapOneByteLexerStream, HeapTwoByteCodePointLexerStream,
            HeapTwoByteCodeUnitLexerStream, LexerStream,
        },
        regexp::{RegExp, RegExpFlags},
        regexp_parser::RegExpParser,
    },
    runtime::{
        abstract_operations::{define_property_or_throw, set},
        builtin_function::BuiltinFunction,
        error::{syntax_parse_error, type_error},
        eval_result::EvalResult,
        function::get_argument,
        gc::{Handle, HeapItem, HeapVisitor},
        get,
        heap_item_descriptor::HeapItemKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        realm::Realm,
        regexp::{compiled_regexp::CompiledRegExpObject, compiler::compile_regexp},
        string_value::StringValue,
        to_string,
        type_utilities::{is_regexp, same_value},
        Context, HeapPtr, PropertyDescriptor, Value,
    },
    set_uninit,
};

use super::{intrinsics::Intrinsic, rust_runtime::return_this};

// RegExp (Regular Expression) Objects (https://tc39.es/ecma262/#sec-regexp-regular-expression-objects)
extend_object! {
    pub struct RegExpObject {
        compiled_regexp: HeapPtr<CompiledRegExpObject>,
    }
}

impl RegExpObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<RegExpObject>> {
        let mut object = object_create_from_constructor::<RegExpObject>(
            cx,
            constructor,
            HeapItemKind::RegExpObject,
            Intrinsic::RegExpPrototype,
        )?;

        // Initialize with default values as allocation may occur before real values are set, so
        // we must ensure the RegExpObject is in a valid state.
        set_uninit!(object.compiled_regexp, HeapPtr::uninit());

        let object = object.to_handle();

        Self::define_last_index_property(cx, object);

        Ok(object)
    }

    pub fn new_from_compiled_regexp(
        cx: Context,
        compiled_regexp: Handle<CompiledRegExpObject>,
    ) -> Handle<RegExpObject> {
        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let mut object = must!(object_create_from_constructor::<RegExpObject>(
            cx,
            regexp_constructor,
            HeapItemKind::RegExpObject,
            Intrinsic::RegExpPrototype
        ));

        set_uninit!(object.compiled_regexp, *compiled_regexp);

        let object = object.to_handle();

        Self::define_last_index_property(cx, object);

        // Initialize last index property
        let zero_value = cx.zero();
        must!(set(cx, object.into(), cx.names.last_index(), zero_value, true));

        object
    }

    fn define_last_index_property(cx: Context, regexp_object: Handle<RegExpObject>) {
        let last_index_desc = PropertyDescriptor::data(cx.undefined(), true, false, false);
        must!(define_property_or_throw(
            cx,
            regexp_object.into(),
            cx.names.last_index(),
            last_index_desc
        ));
    }

    #[inline]
    pub fn compiled_regexp(&self) -> Handle<CompiledRegExpObject> {
        self.compiled_regexp.to_handle()
    }

    #[inline]
    pub fn flags(&self) -> RegExpFlags {
        self.compiled_regexp.flags
    }

    #[inline]
    pub fn escaped_pattern_source(&self) -> Handle<StringValue> {
        self.compiled_regexp.escaped_pattern_source()
    }
}

pub struct RegExpConstructor;

impl RegExpConstructor {
    /// Properties of the RegExp Constructor (https://tc39.es/ecma262/#sec-properties-of-the-regexp-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            2,
            cx.names.regexp(),
            realm,
            Intrinsic::FunctionPrototype,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::RegExpPrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.escape(), Self::escape, 1, realm);

        // get RegExp [ @@species ] (https://tc39.es/ecma262/#sec-get-regexp-%symbol.species%)
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, return_this, realm);

        func
    }

    /// RegExp (https://tc39.es/ecma262/#sec-regexp-pattern-flags)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let pattern_arg = get_argument(cx, arguments, 0);
        let flags_arg = get_argument(cx, arguments, 1);

        let pattern_is_regexp = is_regexp(cx, pattern_arg)?;

        let new_target = match cx.current_new_target() {
            None => {
                let new_target = cx.current_function();

                if pattern_is_regexp && flags_arg.is_undefined() {
                    let pattern_constructor =
                        get(cx, pattern_arg.as_object(), cx.names.constructor())?;

                    if same_value(new_target.into(), pattern_constructor) {
                        return Ok(pattern_arg);
                    }
                }

                new_target
            }
            Some(new_target) => new_target,
        };

        let regexp_source = if let Some(pattern_regexp_object) = as_regexp_object(pattern_arg) {
            // Construction from a regexp object
            if flags_arg.is_undefined() {
                RegExpSource::RegExpObject(pattern_regexp_object)
            } else {
                // If flags are provided we must reparse pattern instead of using compiled regexp
                // directly, since different flags may result in a different regexp.
                let pattern_value = pattern_regexp_object.escaped_pattern_source().into();
                RegExpSource::PatternAndFlags(pattern_value, FlagsSource::Value(flags_arg))
            }
        } else if pattern_is_regexp {
            // Construction from a pattern object that has a [Symbol.match] property
            let pattern = get(cx, pattern_arg.as_object(), cx.names.source())?;

            // Use flags argument if one is provided, otherwise default to pattern's flags property
            let flags_value = if flags_arg.is_undefined() {
                get(cx, pattern_arg.as_object(), cx.names.flags())?
            } else {
                flags_arg
            };

            RegExpSource::PatternAndFlags(pattern, FlagsSource::Value(flags_value))
        } else {
            // Construction from string pattern and flags arguments
            RegExpSource::PatternAndFlags(pattern_arg, FlagsSource::Value(flags_arg))
        };

        regexp_create(cx, regexp_source, new_target)
    }

    /// RegExp.escape (https://tc39.es/ecma262/#sec-regexp.escape)
    pub fn escape(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let string_arg = get_argument(cx, arguments, 0);
        if !string_arg.is_string() {
            return type_error(cx, "RegExp.escape called with non-string argument");
        }
        let string = string_arg.as_string();

        let mut escaped = Wtf8String::new();

        for code_point in string.iter_code_points() {
            // NOTE: Escaping a leading digit ensures that output corresponds with pattern text
            // which may be used after a \0 character escape or a DecimalEscape such as \1 and still
            // match S rather than be interpreted as an extension of the preceding escape sequence.
            // Escaping a leading ASCII letter does the same for the context after \c.
            if escaped.is_empty()
                && (is_ascii_alphabetic(code_point) || is_decimal_digit(code_point))
            {
                escaped.push_str(&format!("\\x{code_point:x}"));
                continue;
            }

            match_u32!(match code_point {
                // Syntax characters are directly escaped
                '^' | '$' | '\\' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}'
                | '|' | '/' => {
                    escaped.push_char('\\');
                    escaped.push(code_point);
                }
                // Control escapes
                '\t' => escaped.push_str("\\t"),
                '\n' => escaped.push_str("\\n"),
                '\u{000B}' => escaped.push_str("\\v"),
                '\u{000C}' => escaped.push_str("\\f"),
                '\r' => escaped.push_str("\\r"),
                // Other punctuators are `\x` escaped
                ',' | '-' | '=' | '<' | '>' | '#' | '&' | '!' | '%' | ':' | ';' | '@' | '~'
                | '\'' | '`' | '\"' => {
                    escaped.push_str(&format!("\\x{code_point:2x}"));
                }
                // Newlines, whitespace, and surrogate code points are either `\x` or `\u` escaped
                // as appropriate.
                _ if is_newline(code_point)
                    || is_whitespace(code_point)
                    || is_surrogate_code_point(code_point) =>
                {
                    if is_latin1(code_point) {
                        escaped.push_str(&format!("\\x{code_point:2x}"));
                    } else {
                        escaped.push_str(&format!("\\u{code_point:4x}"));
                    }
                }
                // Otherwise code point does not need to be escaped
                _ => escaped.push(code_point),
            })
        }

        Ok(cx.alloc_wtf8_string(&escaped).as_value())
    }
}

#[inline]
pub fn as_regexp_object(value: Handle<Value>) -> Option<Handle<RegExpObject>> {
    if value.is_object() {
        value.as_object().as_regexp_object()
    } else {
        None
    }
}

// Source used to construct a RegExp
pub enum RegExpSource {
    // Construct from a pre-existing RegExpObject
    RegExpObject(Handle<RegExpObject>),
    // Construct from a pair of pattern value and flags
    PatternAndFlags(Handle<Value>, FlagsSource),
}

pub enum FlagsSource {
    // Construct from pre-existing RegExpFloags
    RegExpFlags(RegExpFlags),
    // Construct from flags value
    Value(Handle<Value>),
}

/// RegExpCreate (https://tc39.es/ecma262/#sec-regexpcreate)
pub fn regexp_create(
    cx: Context,
    regexp_source: RegExpSource,
    constructor: Handle<ObjectValue>,
) -> EvalResult<Handle<Value>> {
    let mut regexp_object = RegExpObject::new_from_constructor(cx, constructor)?;

    match regexp_source {
        RegExpSource::RegExpObject(old_regexp_object) => {
            regexp_object.compiled_regexp = old_regexp_object.compiled_regexp;
        }
        RegExpSource::PatternAndFlags(pattern_value, flags_source) => {
            // Make sure to call ToString on pattern before flags, following order in spec
            let pattern = value_or_empty_string(cx, pattern_value);
            let pattern_string = to_string(cx, pattern)?;

            let flags = match flags_source {
                FlagsSource::RegExpFlags(flags) => flags,
                FlagsSource::Value(flags_value) => {
                    let flags_value = value_or_empty_string(cx, flags_value);
                    let flags_string = to_string(cx, flags_value)?;

                    parse_flags(cx, flags_string)?
                }
            };

            let alloc = Bump::new();
            let regexp = parse_pattern(cx, pattern_string, flags, &alloc)?;
            let source = escape_pattern_string(cx, pattern_string);

            let compiled_regexp = compile_regexp(cx, &regexp, source);

            regexp_object.compiled_regexp = *compiled_regexp;
        }
    }

    // Initialize last index property
    let zero_value = cx.zero();
    set(cx, regexp_object.into(), cx.names.last_index(), zero_value, true)?;

    Ok(regexp_object.as_value())
}

fn value_or_empty_string(cx: Context, value: Handle<Value>) -> Handle<Value> {
    if value.is_undefined() {
        cx.names.empty_string().as_string().into()
    } else {
        value
    }
}

fn parse_flags(cx: Context, flags_string: Handle<StringValue>) -> EvalResult<RegExpFlags> {
    fn parse_lexer_stream(cx: Context, lexer_stream: impl LexerStream) -> EvalResult<RegExpFlags> {
        match RegExpParser::parse_flags(lexer_stream) {
            Ok(flags) => Ok(flags),
            Err(error) => syntax_parse_error(cx, &error),
        }
    }

    let flat_string = flags_string.flatten();
    match flat_string.width() {
        StringWidth::OneByte => {
            let lexer_stream = HeapOneByteLexerStream::new(flat_string.as_one_byte_slice());
            parse_lexer_stream(cx, lexer_stream)
        }
        // Non-ASCII code points are not allowed in flags, so always safe to use unicode-unware
        // code unit lexer.
        StringWidth::TwoByte => {
            let lexer_stream =
                HeapTwoByteCodeUnitLexerStream::new(flat_string.as_two_byte_slice(), None);
            parse_lexer_stream(cx, lexer_stream)
        }
    }
}

fn parse_pattern(
    cx: Context,
    pattern_string: Handle<StringValue>,
    flags: RegExpFlags,
    alloc: AstAlloc,
) -> EvalResult<RegExp> {
    fn parse_lexer_stream<'a, T: LexerStream>(
        cx: Context,
        create_lexer_stream: &dyn Fn() -> T,
        flags: RegExpFlags,
        alloc: AstAlloc<'a>,
    ) -> EvalResult<RegExp<'a>> {
        match RegExpParser::parse_regexp(create_lexer_stream, flags, cx.options.as_ref(), alloc) {
            Ok(regexp) => Ok(regexp),
            Err(error) => syntax_parse_error(cx, &error),
        }
    }

    let flat_string = pattern_string.flatten();
    match flat_string.width() {
        StringWidth::OneByte => {
            let create_lexer_stream =
                || HeapOneByteLexerStream::new(flat_string.as_one_byte_slice());
            parse_lexer_stream(cx, &create_lexer_stream, flags, alloc)
        }
        StringWidth::TwoByte => {
            if flags.has_any_unicode_flag() {
                let create_lexer_stream =
                    || HeapTwoByteCodePointLexerStream::new(flat_string.as_two_byte_slice());
                parse_lexer_stream(cx, &create_lexer_stream, flags, alloc)
            } else {
                let create_lexer_stream =
                    || HeapTwoByteCodeUnitLexerStream::new(flat_string.as_two_byte_slice(), None);
                parse_lexer_stream(cx, &create_lexer_stream, flags, alloc)
            }
        }
    }
}

fn escape_pattern_string(
    mut cx: Context,
    pattern_string: Handle<StringValue>,
) -> Handle<StringValue> {
    // Special case the empty pattern string - equivalent to an empty non-capturing group
    if pattern_string.is_empty() {
        return cx.alloc_string("(?:)").as_string();
    }

    // Only need to escape line terminators and forward slash
    let neeeds_escape = pattern_string
        .iter_code_units()
        .any(|code_unit| code_unit == '/' as u16 || is_newline(code_unit as u32));
    if !neeeds_escape {
        return pattern_string;
    }

    let mut escaped_string = Wtf8String::new();

    for code_unit in pattern_string.iter_code_units() {
        if code_unit == '/' as u16 {
            escaped_string.push_str("\\/");
        } else if code_unit == '\n' as u16 {
            escaped_string.push_str("\\n");
        } else if code_unit == '\r' as u16 {
            escaped_string.push_str("\\r");
        } else if code_unit == '\u{2028}' as u16 {
            escaped_string.push_str("\\u2028");
        } else if code_unit == '\u{2029}' as u16 {
            escaped_string.push_str("\\u2029");
        } else {
            escaped_string.push(code_unit as u32);
        }
    }

    cx.alloc_wtf8_string(&escaped_string).as_string()
}

impl HeapItem for HeapPtr<RegExpObject> {
    fn byte_size(&self) -> usize {
        size_of::<RegExpObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.compiled_regexp);
    }
}
