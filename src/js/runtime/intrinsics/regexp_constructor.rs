use std::mem::size_of;

use crate::{
    extend_object,
    js::{
        parser::{
            ast,
            lexer_stream::{
                HeapOneByteLexerStream, HeapTwoByteCodePointLexerStream,
                HeapTwoByteCodeUnitLexerStream, LexerStream,
            },
            regexp::RegExpFlags,
            regexp_parser::RegExpParser,
        },
        runtime::{
            abstract_operations::{define_property_or_throw, set},
            builtin_function::BuiltinFunction,
            completion::EvalResult,
            error::syntax_error_,
            function::get_argument,
            gc::{Handle, HeapObject, HeapVisitor},
            get,
            interned_strings::InternedStrings,
            object_descriptor::ObjectKind,
            object_value::ObjectValue,
            ordinary_object::object_create_from_constructor,
            property::Property,
            realm::Realm,
            string_value::{StringValue, StringWidth},
            to_string,
            type_utilities::{is_regexp, same_value},
            Context, HeapPtr, PropertyDescriptor, Value,
        },
    },
    maybe, must, set_uninit,
};

use super::intrinsics::Intrinsic;

// 22.2 RegExp (Regular Expression) Objects
extend_object! {
    pub struct RegExpObject {
        // Flags of the regexp object
        flags: RegExpFlags,
        // The pattern component of the original regexp as a string. Escaped so that it can be
        // parsed into exactly the same pattern again.
        escaped_pattern_source: HeapPtr<StringValue>,
        // Lazily generated flags string
        flags_string: Option<HeapPtr<StringValue>>,
    }
}

impl RegExpObject {
    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<RegExpObject>> {
        let mut object = maybe!(object_create_from_constructor::<RegExpObject>(
            cx,
            constructor,
            ObjectKind::RegExpObject,
            Intrinsic::RegExpPrototype
        ));

        set_uninit!(object.flags_string, None);

        let object = object.to_handle();

        let last_index_desc = PropertyDescriptor::data(cx.undefined(), true, false, false);
        must!(define_property_or_throw(
            cx,
            object.into(),
            cx.names.last_index(),
            last_index_desc
        ));

        object.into()
    }

    pub fn new_from_literal(cx: &mut Context, lit: &ast::RegExpLiteral) -> Handle<RegExpObject> {
        // Can use source directly as "escaped" pattern source since
        let source = InternedStrings::get_str(cx, &lit.raw);

        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let mut regexp_object = must!(RegExpObject::new_from_constructor(cx, regexp_constructor));

        set_uninit!(regexp_object.flags, lit.regexp.flags);
        set_uninit!(regexp_object.escaped_pattern_source, source.get_());
        set_uninit!(regexp_object.flags_string, None);

        regexp_object.into()
    }

    pub fn flags(&self) -> RegExpFlags {
        self.flags
    }

    pub fn flags_string(&self) -> Option<Handle<StringValue>> {
        self.flags_string.map(|f| f.to_handle())
    }

    pub fn set_flags_string(&mut self, flags_string: Handle<StringValue>) {
        self.flags_string = Some(flags_string.get_());
    }

    pub fn escaped_pattern_source(&self) -> Handle<StringValue> {
        self.escaped_pattern_source.to_handle()
    }
}

pub struct RegExpConstructor;

impl RegExpConstructor {
    // 22.2.5 Properties of the RegExp Constructor
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            2,
            cx.names.regexp(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::RegExpPrototype).into(),
                false,
                false,
                false,
            ),
        );

        // 22.2.5.2 get RegExp [ @@species ]
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 22.2.4.1 RegExp
    fn construct(
        cx: &mut Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let pattern_arg = get_argument(cx, arguments, 0);
        let flags_arg = get_argument(cx, arguments, 1);

        let pattern_is_regexp = maybe!(is_regexp(cx, pattern_arg));

        let new_target = match new_target {
            None => {
                let new_target = cx.current_execution_context().function();

                if pattern_is_regexp && flags_arg.is_undefined() {
                    let pattern_constructor =
                        maybe!(get(cx, pattern_arg.as_object(), cx.names.constructor()));

                    if same_value(new_target.into(), pattern_constructor) {
                        return pattern_arg.into();
                    }
                }

                new_target
            }
            Some(new_target) => new_target,
        };

        let pattern_source;
        let flags_source;

        if pattern_arg.is_object() && pattern_arg.as_object().is_regexp_object() {
            // Construction from a regexp object
            let pattern_regexp_object = pattern_arg.as_object().cast::<RegExpObject>();

            pattern_source =
                PatternSourceValue::Known(pattern_regexp_object.escaped_pattern_source());

            if flags_arg.is_undefined() {
                flags_source = FlagsSource::Known(pattern_regexp_object.flags());
            } else {
                flags_source = FlagsSource::Value(flags_arg);
            }
        } else if pattern_is_regexp {
            // Construction from a pattern object that has a [Symbol.match] property
            let pattern = maybe!(get(cx, pattern_arg.as_object(), cx.names.source()));
            pattern_source = PatternSourceValue::Value(pattern);

            // Use flags argument if one is provided, otherwise default to pattern's flags property
            let flags_value = if flags_arg.is_undefined() {
                maybe!(get(cx, pattern_arg.as_object(), cx.names.flags()))
            } else {
                flags_arg
            };
            flags_source = FlagsSource::Value(flags_value);
        } else {
            // Construction from string pattern and flags arguments
            pattern_source = PatternSourceValue::Value(pattern_arg);
            flags_source = FlagsSource::Value(flags_arg);
        }

        regexp_create(cx, pattern_source, flags_source, new_target)
    }

    // 22.2.5.2 get RegExp [ @@species ]
    fn get_species(
        _: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}

// Internal enum used in RegExp constructor
pub enum PatternSourceValue {
    // Old escaped pattern source
    Known(Handle<StringValue>),
    Value(Handle<Value>),
}

// Internal enum used in RegExp constructor
enum PatternSourceString {
    // Old escaped pattern source
    Known(Handle<StringValue>),
    String(Handle<StringValue>),
}

// Internal enum used in RegExp constructor
pub enum FlagsSource {
    Known(RegExpFlags),
    Value(Handle<Value>),
}

// 22.2.3.1 RegExpCreate
pub fn regexp_create(
    cx: &mut Context,
    pattern_source: PatternSourceValue,
    flags_source: FlagsSource,
    constructor: Handle<ObjectValue>,
) -> EvalResult<Handle<Value>> {
    let mut regexp_object = maybe!(RegExpObject::new_from_constructor(cx, constructor));

    // Make sure to call ToString on pattern before flags, following order in spec
    let pattern_source = match pattern_source {
        PatternSourceValue::Known(old_pattern_source) => {
            PatternSourceString::Known(old_pattern_source)
        }
        PatternSourceValue::Value(pattern_value) => {
            let pattern = value_or_empty_string(cx, pattern_value);
            let pattern_string = maybe!(to_string(cx, pattern));

            PatternSourceString::String(pattern_string)
        }
    };

    // Parse flags if they need to be parsed, and initialize on regexp object
    let flags = match flags_source {
        FlagsSource::Known(flags) => flags,
        FlagsSource::Value(flags_value) => {
            let flags = value_or_empty_string(cx, flags_value);
            let flags_string = maybe!(to_string(cx, flags));

            maybe!(parse_flags(cx, flags_string))
        }
    };

    set_uninit!(regexp_object.flags, flags);

    // Parse pattern if it needs to be parsed, and initialize on regexp object
    let escaped_pattern_source = match pattern_source {
        PatternSourceString::Known(old_pattern_source) => old_pattern_source,
        PatternSourceString::String(pattern_string) => {
            maybe!(parse_pattern(cx, pattern_string, flags));

            // TODO: Escape pattern source
            pattern_string
        }
    };

    set_uninit!(regexp_object.escaped_pattern_source, escaped_pattern_source.get_());

    // Initialize last index property
    let zero_value = Value::from(0).to_handle(cx);
    maybe!(set(cx, regexp_object.into(), cx.names.last_index(), zero_value, true));

    regexp_object.into()
}

fn value_or_empty_string(cx: &mut Context, value: Handle<Value>) -> Handle<Value> {
    if value.is_undefined() {
        cx.names.empty_string().as_string().into()
    } else {
        value
    }
}

fn parse_flags(cx: &mut Context, flags_string: Handle<StringValue>) -> EvalResult<RegExpFlags> {
    fn parse_lexer_stream(
        cx: &mut Context,
        lexer_stream: impl LexerStream,
    ) -> EvalResult<RegExpFlags> {
        match RegExpParser::parse_flags(lexer_stream) {
            Ok(flags) => flags.into(),
            Err(error) => syntax_error_(cx, &error.to_string()),
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
            let lexer_stream = HeapTwoByteCodeUnitLexerStream::new(flat_string.as_two_byte_slice());
            parse_lexer_stream(cx, lexer_stream)
        }
    }
}

fn parse_pattern(
    cx: &mut Context,
    pattern_string: Handle<StringValue>,
    flags: RegExpFlags,
) -> EvalResult<()> {
    fn parse_lexer_stream(
        cx: &mut Context,
        lexer_stream: impl LexerStream,
        flags: RegExpFlags,
    ) -> EvalResult<()> {
        match RegExpParser::parse_regexp(lexer_stream, flags) {
            Ok(_) => ().into(),
            Err(error) => syntax_error_(cx, &error.to_string()),
        }
    }

    let flat_string = pattern_string.flatten();
    match flat_string.width() {
        StringWidth::OneByte => {
            let lexer_stream = HeapOneByteLexerStream::new(flat_string.as_one_byte_slice());
            parse_lexer_stream(cx, lexer_stream, flags)
        }
        StringWidth::TwoByte => {
            if flags.contains(RegExpFlags::UNICODE_AWARE) {
                let lexer_stream =
                    HeapTwoByteCodePointLexerStream::new(flat_string.as_two_byte_slice());
                parse_lexer_stream(cx, lexer_stream, flags)
            } else {
                let lexer_stream =
                    HeapTwoByteCodeUnitLexerStream::new(flat_string.as_two_byte_slice());
                parse_lexer_stream(cx, lexer_stream, flags)
            }
        }
    }
}

impl HeapObject for HeapPtr<RegExpObject> {
    fn byte_size(&self) -> usize {
        size_of::<RegExpObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}
