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
            regexp::{RegExp, RegExpFlags},
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
            regexp::{compiled_regexp::CompiledRegExpObject, compiler::compile_regexp},
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
        compiled_regexp: HeapPtr<CompiledRegExpObject>,
        // The pattern component of the original regexp as a string. Escaped so that it can be
        // parsed into exactly the same pattern again.
        escaped_pattern_source: HeapPtr<StringValue>,
        // Lazily generated flags string
        flags_string: Option<HeapPtr<StringValue>>,
    }
}

impl RegExpObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<RegExpObject>> {
        let mut object = maybe!(object_create_from_constructor::<RegExpObject>(
            cx,
            constructor,
            ObjectKind::RegExpObject,
            Intrinsic::RegExpPrototype
        ));

        // Initialize with default values as allocation may occur before real values are set, so
        // we must ensure the RegExpObject is in a valid state.
        set_uninit!(object.compiled_regexp, HeapPtr::uninit());
        set_uninit!(object.escaped_pattern_source, HeapPtr::uninit());
        set_uninit!(object.flags_string, None);

        let object = object.to_handle();

        Self::define_last_index_property(cx, object);

        object.into()
    }

    pub fn new_from_literal(
        cx: Context,
        lit: &ast::RegExpLiteral,
    ) -> EvalResult<Handle<RegExpObject>> {
        // Can use source directly as "escaped" pattern source since
        let source = InternedStrings::get_wtf8_str(cx, &lit.pattern);
        let compiled_regexp = compile_regexp(cx, &lit.regexp);

        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let mut object = must!(object_create_from_constructor::<RegExpObject>(
            cx,
            regexp_constructor,
            ObjectKind::RegExpObject,
            Intrinsic::RegExpPrototype
        ));

        set_uninit!(object.compiled_regexp, compiled_regexp.get_());
        set_uninit!(object.escaped_pattern_source, source.get_());
        set_uninit!(object.flags_string, None);

        let object = object.to_handle();

        Self::define_last_index_property(cx, object);

        // Initialize last index property
        let zero_value = Value::from(0).to_handle(cx);
        maybe!(set(cx, object.into(), cx.names.last_index(), zero_value, true));

        object.into()
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
    pub fn flags_string(&self) -> Option<Handle<StringValue>> {
        self.flags_string.map(|f| f.to_handle())
    }

    #[inline]
    pub fn set_flags_string(&mut self, flags_string: Handle<StringValue>) {
        self.flags_string = Some(flags_string.get_());
    }

    #[inline]
    pub fn escaped_pattern_source(&self) -> Handle<StringValue> {
        self.escaped_pattern_source.to_handle()
    }
}

pub struct RegExpConstructor;

impl RegExpConstructor {
    // 22.2.5 Properties of the RegExp Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
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
        cx: Context,
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

        let regexp_source = if pattern_arg.is_object() && pattern_arg.as_object().is_regexp_object()
        {
            // Construction from a regexp object
            let pattern_regexp_object = pattern_arg.as_object().cast::<RegExpObject>();

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
            let pattern = maybe!(get(cx, pattern_arg.as_object(), cx.names.source()));

            // Use flags argument if one is provided, otherwise default to pattern's flags property
            let flags_value = if flags_arg.is_undefined() {
                maybe!(get(cx, pattern_arg.as_object(), cx.names.flags()))
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

    // 22.2.5.2 get RegExp [ @@species ]
    fn get_species(
        _: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
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

// 22.2.3.1 RegExpCreate
pub fn regexp_create(
    cx: Context,
    regexp_source: RegExpSource,
    constructor: Handle<ObjectValue>,
) -> EvalResult<Handle<Value>> {
    let mut regexp_object = maybe!(RegExpObject::new_from_constructor(cx, constructor));

    match regexp_source {
        RegExpSource::RegExpObject(old_regexp_object) => {
            regexp_object.compiled_regexp = old_regexp_object.compiled_regexp;
            regexp_object.escaped_pattern_source = old_regexp_object.escaped_pattern_source;
            regexp_object.flags_string = old_regexp_object.flags_string;
        }
        RegExpSource::PatternAndFlags(pattern_value, flags_source) => {
            // Make sure to call ToString on pattern before flags, following order in spec
            let pattern = value_or_empty_string(cx, pattern_value);
            let pattern_string = maybe!(to_string(cx, pattern));

            let flags = match flags_source {
                FlagsSource::RegExpFlags(flags) => flags,
                FlagsSource::Value(flags_value) => {
                    let flags_value = value_or_empty_string(cx, flags_value);
                    let flags_string = maybe!(to_string(cx, flags_value));

                    maybe!(parse_flags(cx, flags_string))
                }
            };

            let regexp = maybe!(parse_pattern(cx, pattern_string, flags));

            let compiled_regexp = compile_regexp(cx, &regexp);

            // TODO: Escape pattern source
            regexp_object.compiled_regexp = compiled_regexp.get_();
            regexp_object.escaped_pattern_source = pattern_string.get_();
        }
    }

    // Initialize last index property
    let zero_value = Value::from(0).to_handle(cx);
    maybe!(set(cx, regexp_object.into(), cx.names.last_index(), zero_value, true));

    regexp_object.into()
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
    cx: Context,
    pattern_string: Handle<StringValue>,
    flags: RegExpFlags,
) -> EvalResult<RegExp> {
    fn parse_lexer_stream(
        cx: Context,
        lexer_stream: impl LexerStream,
        flags: RegExpFlags,
    ) -> EvalResult<RegExp> {
        match RegExpParser::parse_regexp(lexer_stream, flags) {
            Ok(regexp) => regexp.into(),
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
        visitor.visit_pointer(&mut self.compiled_regexp);
        visitor.visit_pointer(&mut self.escaped_pattern_source);
        visitor.visit_pointer_opt(&mut self.flags_string);
    }
}
