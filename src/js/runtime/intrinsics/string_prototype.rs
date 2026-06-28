use crate::{
    common::{
        icu::ICU,
        numeric::Numeric,
        string::StringWidth,
        unicode::{
            CodePoint, is_decimal_digit, is_high_surrogate_code_unit, is_low_surrogate_code_unit,
        },
        wtf_8::Wtf8String,
    },
    intrinsic_methods, must, must_a,
    parser::regexp::RegExpFlags,
    runtime::{
        Context, Handle, HeapPtr, PropertyKey,
        abstract_operations::{call_object, get_method, invoke},
        alloc_error::AllocResult,
        array_object::{array_create, create_array_from_list},
        error::{range_error, type_error},
        eval_result::EvalResult,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            regexp_constructor::{FlagsSource, RegExpSource, regexp_create},
            regexp_prototype::flags_string_contains,
            rust_runtime::RuntimeFunction,
            string_iterator::StringIterator,
        },
        object_value::ObjectValue,
        realm::Realm,
        string_object::StringObject,
        string_value::{
            FlatString, StringValue, UnsafeCodePointIterator, string_exceeds_max_length_error,
        },
        to_string,
        type_utilities::{
            is_callable, is_regexp, require_object_coercible, resolve_relative_index_argument,
            to_integer_or_infinity, to_length, to_number, to_uint32,
        },
        value::Value,
    },
    runtime_fn,
};

pub struct StringPrototype;

impl StringPrototype {
    /// Properties of the String Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-string-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let empty_string = cx.names.empty_string().as_string();
        let object = StringObject::new_with_proto(cx, object_proto, empty_string)?.as_object();
        let mut builder = IntrinsicBuilder::new(cx, realm, object);

        // Constructor property is added once StringConstructor has been created
        intrinsic_methods!(cx, builder, {
            at                   StringPrototype_at              (1),
            char_at              StringPrototype_char_at         (1),
            char_code_at         StringPrototype_char_code_at    (1),
            code_point_at        StringPrototype_code_point_at   (1),
            concat               StringPrototype_concat          (1),
            ends_with            StringPrototype_ends_with       (1),
            includes             StringPrototype_includes        (1),
            index_of             StringPrototype_index_of        (1),
            is_well_formed       StringPrototype_is_well_formed  (0),
            last_index_of        StringPrototype_last_index_of   (1),
            locale_compare       StringPrototype_locale_compare  (1),
            match_               StringPrototype_match_          (1),
            match_all            StringPrototype_match_all       (1),
            normalize            StringPrototype_normalize       (0),
            pad_end              StringPrototype_pad_end         (1),
            pad_start            StringPrototype_pad_start       (1),
            repeat               StringPrototype_repeat          (1),
            replace              StringPrototype_replace         (2),
            replace_all          StringPrototype_replace_all     (2),
            search               StringPrototype_search          (1),
            slice                StringPrototype_slice           (2),
            split                StringPrototype_split           (2),
            starts_with          StringPrototype_starts_with     (1),
            substring            StringPrototype_substring       (2),
            to_string            StringPrototype_to_string       (0),
            to_locale_lower_case StringPrototype_to_lower_case   (0),
            to_locale_upper_case StringPrototype_to_upper_case   (0),
            to_lower_case        StringPrototype_to_lower_case   (0),
            to_upper_case        StringPrototype_to_upper_case   (0),
            to_well_formed       StringPrototype_to_well_formed  (0),
            trim                 StringPrototype_trim            (0),
            trim_end             StringPrototype_trim_end        (0),
            trim_start           StringPrototype_trim_start      (0),
            value_of             StringPrototype_to_string       (0),
        });

        // String.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-string.prototype-%symbol.iterator%)
        builder.method(cx.symbols.iterator(), RuntimeFunction::StringPrototype_iterator, 0)?;

        builder.build()
    }

    /// Additional Properties of the String.prototype Object (https://tc39.es/ecma262/#sec-additional-properties-of-the-string.prototype-object)
    pub fn init_annex_b_methods(
        string_prototype: Handle<ObjectValue>,
        mut cx: Context,
        realm: Handle<Realm>,
    ) -> AllocResult<()> {
        let mut builder = IntrinsicBuilder::new(cx, realm, string_prototype);

        let substr_name = cx.alloc_static_string("substr")?;
        let substr = PropertyKey::string_not_array_index_handle(cx, substr_name)?;
        builder.method(substr, RuntimeFunction::StringPrototype_substr, 2)?;

        // String.prototype.trimLeft and String.prototype.trimRight are direct aliases for
        // String.prototype.trimStart and String.prototype.trimEnd respectively.
        let trim_start = must_a!(get(cx, string_prototype, cx.names.trim_start()));
        let trim_end = must_a!(get(cx, string_prototype, cx.names.trim_end()));

        let trim_left_name = cx.alloc_static_string("trimLeft")?;
        let trim_left = PropertyKey::string_not_array_index_handle(cx, trim_left_name)?;

        let trim_right_name = cx.alloc_static_string("trimRight")?;
        let trim_right = PropertyKey::string_not_array_index_handle(cx, trim_right_name)?;

        builder.data(trim_left, trim_start)?;
        builder.data(trim_right, trim_end)?;

        macro_rules! html_methods {
            ($($name:expr, $method:path, $length:expr),*) => {
                $(
                    let name = cx.alloc_static_string($name)?;
                    let key = PropertyKey::string_not_array_index_handle(cx, name)?;
                    builder.method(key, $method, $length)?;
                )*
            }
        }

        html_methods! {
            "anchor", RuntimeFunction::StringPrototype_anchor, 1,
            "big", RuntimeFunction::StringPrototype_big, 0,
            "blink", RuntimeFunction::StringPrototype_blink, 0,
            "bold", RuntimeFunction::StringPrototype_bold, 0,
            "fixed", RuntimeFunction::StringPrototype_fixed, 0,
            "fontcolor", RuntimeFunction::StringPrototype_font_color, 1,
            "fontsize", RuntimeFunction::StringPrototype_font_size, 1,
            "italics", RuntimeFunction::StringPrototype_italics, 0,
            "link", RuntimeFunction::StringPrototype_link, 1,
            "small", RuntimeFunction::StringPrototype_small, 0,
            "strike", RuntimeFunction::StringPrototype_strike, 0,
            "sub", RuntimeFunction::StringPrototype_sub, 0,
            "sup", RuntimeFunction::StringPrototype_sup, 0
        }

        builder.build()?;

        Ok(())
    }

    runtime_fn! {
    /// String.prototype.at (https://tc39.es/ecma262/#sec-string.prototype.at)
    fn at(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "at")?;
        let string = to_string(cx, object)?;

        let length = string.len() as i64;

        let index_arg = arguments.get(cx, 0);
        let relative_index = to_integer_or_infinity(cx, index_arg)?;
        if relative_index == f64::INFINITY {
            return Ok(cx.undefined());
        }

        let index = if relative_index >= 0.0 {
            relative_index as i64
        } else {
            length + (relative_index as i64)
        };

        if index < 0 || index >= length {
            return Ok(cx.undefined());
        }

        let code_unit_string = FlatString::from_code_unit(cx, string.code_unit_at(index as u32)?)?;

        Ok(code_unit_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.charAt (https://tc39.es/ecma262/#sec-string.prototype.charat)
    fn char_at(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "charAt")?;
        let string = to_string(cx, object)?;

        let position_arg = arguments.get(cx, 0);
        let position = to_integer_or_infinity(cx, position_arg)?;

        if position < 0.0 || position >= string.len() as f64 {
            return Ok(cx.names.empty_string().as_string().as_value());
        }

        let char_string = FlatString::from_code_unit(cx, string.code_unit_at(position as u32)?)?;

        Ok(char_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.charCodeAt (https://tc39.es/ecma262/#sec-string.prototype.charcodeat)
    fn char_code_at(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "charCodeAt")?;
        let string = to_string(cx, object)?;

        let position_arg = arguments.get(cx, 0);
        let position = to_integer_or_infinity(cx, position_arg)?;

        if position < 0.0 || position >= string.len() as f64 {
            return Ok(cx.nan());
        }

        let code_unit = string.code_unit_at(position as u32)?;
        Ok(cx.smi(code_unit))
    }}

    runtime_fn! {
    /// String.prototype.codePointAt (https://tc39.es/ecma262/#sec-string.prototype.codepointat)
    fn code_point_at(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "codePointAt")?;
        let string = to_string(cx, object)?;

        let position_arg = arguments.get(cx, 0);
        let position = to_integer_or_infinity(cx, position_arg)?;

        if position < 0.0 || position >= string.len() as f64 {
            return Ok(cx.undefined());
        }

        let code_point = string.code_point_at(position as u32)?;
        Ok(cx.smi(code_point as i32))
    }}

    runtime_fn! {
    /// String.prototype.concat (https://tc39.es/ecma262/#sec-string.prototype.concat)
    fn concat(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "concat")?;
        let mut concat_string = to_string(cx, object)?;

        for argument in arguments.iter() {
            let string = to_string(cx, *argument)?;
            concat_string = StringValue::concat(cx, concat_string, string)?;
        }

        Ok(concat_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.endsWith (https://tc39.es/ecma262/#sec-string.prototype.endswith)
    fn ends_with(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "endsWith")?;
        let string = to_string(cx, object)?;
        let length = string.len();

        let search_value = arguments.get(cx, 0);
        if is_regexp(cx, search_value)? {
            return type_error(cx, "String.prototype.endsWith first argument cannot be a RegExp");
        }

        let search_string = to_string(cx, search_value)?;

        let end_index_argument = arguments.get(cx, 1);
        let end_index = if end_index_argument.is_undefined() {
            length
        } else {
            let end_index = to_integer_or_infinity(cx, end_index_argument)?;
            end_index.clamp(0.0, length as f64) as u32
        };

        let search_length = search_string.len();
        if search_length == 0 {
            return Ok(cx.bool(true));
        }

        let start_index = match end_index.checked_sub(search_length) {
            Some(start_index) => start_index,
            None => return Ok(cx.bool(false)),
        };

        let ends_with_string = string.substring_equals(search_string, start_index)?;

        Ok(cx.bool(ends_with_string))
    }}

    runtime_fn! {
    /// String.prototype.includes (https://tc39.es/ecma262/#sec-string.prototype.includes)
    fn includes(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "includes")?;
        let string = to_string(cx, object)?;

        let search_string = arguments.get(cx, 0);
        if is_regexp(cx, search_string)? {
            return type_error(cx, "String.prototype.includes cannot take a RegExp");
        }

        let search_string = to_string(cx, search_string)?;

        let pos_arg = arguments.get(cx, 1);
        let pos = to_integer_or_infinity(cx, pos_arg)?;
        let pos = pos.clamp(0.0, string.len() as f64) as u32;

        if pos > string.len() {
            return Ok(cx.bool(false));
        }

        let found_search_string = string.find(search_string, pos)?.is_some();
        Ok(cx.bool(found_search_string))
    }}

    runtime_fn! {
    /// String.prototype.indexOf (https://tc39.es/ecma262/#sec-string.prototype.indexof)
    fn index_of(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "indexOf")?;
        let string = to_string(cx, object)?;

        let search_arg = arguments.get(cx, 0);
        let search_string = to_string(cx, search_arg)?;

        let pos_arg = arguments.get(cx, 1);
        let pos = to_integer_or_infinity(cx, pos_arg)?;
        let pos = pos.clamp(0.0, string.len() as f64) as u32;

        if pos > string.len() {
            return Ok(cx.negative_one());
        }

        match string.find(search_string, pos)? {
            None => Ok(cx.negative_one()),
            Some(index) => Ok(cx.number(index)),
        }
    }}

    runtime_fn! {
    /// String.prototype.isWellFormed (https://tc39.es/ecma262/#sec-string.prototype.iswellformed)
    fn is_well_formed(cx, this_value, _) {
        let object = this_object_coercible_value(cx, this_value, "isWellFormed")?;
        let string = to_string(cx, object)?;

        Ok(cx.bool(string.is_well_formed()?))
    }}

    runtime_fn! {
    /// String.prototype.lastIndexOf (https://tc39.es/ecma262/#sec-string.prototype.lastindexof)
    fn last_index_of(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "lastIndexOf")?;
        let string = to_string(cx, object)?;

        let search_arg = arguments.get(cx, 0);
        let search_string = to_string(cx, search_arg)?;

        let pos_arg = arguments.get(cx, 1);
        let num_pos = to_number(cx, pos_arg)?;

        let pos = if num_pos.is_nan() {
            f64::INFINITY
        } else {
            to_integer_or_infinity(cx, num_pos)?
        };

        let string_end = pos.clamp(0.0, string.len() as f64) as u32;

        match string.rfind(search_string, string_end)? {
            None => Ok(cx.negative_one()),
            Some(index) => Ok(cx.number(index)),
        }
    }}

    runtime_fn! {
    /// String.prototype.localeCompare (https://tc39.es/ecma262/#sec-string.prototype.localecompare)
    fn locale_compare(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "localeCompare")?;
        let string = to_string(cx, object)?;

        let other_arg = arguments.get(cx, 0);
        let other_string = to_string(cx, other_arg)?;

        let wtf8_string = string.to_wtf8_string()?;
        let wtf8_other_string = other_string.to_wtf8_string()?;
        let comparison = ICU
            .collator
            .as_borrowed()
            .compare_utf8(wtf8_string.as_bytes(), wtf8_other_string.as_bytes());

        Ok(cx.smi(comparison as i8))
    }}

    runtime_fn! {
    /// String.prototype.match (https://tc39.es/ecma262/#sec-string.prototype.match)
    fn match_(cx, this_value, arguments) {
        let this_object = this_object_coercible_value(cx, this_value, "match")?;

        let regexp_arg = arguments.get(cx, 0);
        if regexp_arg.is_object() {
            let matcher = get_method(cx, regexp_arg, cx.symbols.match_())?;
            if let Some(matcher) = matcher {
                return call_object(cx, matcher, regexp_arg, &[this_object]);
            }
        }

        let this_string = to_string(cx, this_object)?;

        let regexp_source = RegExpSource::PatternAndFlags(
            regexp_arg,
            FlagsSource::RegExpFlags(RegExpFlags::empty()),
        );
        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let regexp_object = regexp_create(cx, regexp_source, regexp_constructor)?;

        invoke(cx, regexp_object, cx.symbols.match_(), &[this_string.into()])
    }}

    runtime_fn! {
    /// String.prototype.matchAll (https://tc39.es/ecma262/#sec-string.prototype.matchall)
    fn match_all(cx, this_value, arguments) {
        let this_object = this_object_coercible_value(cx, this_value, "matchAll")?;

        let regexp_arg = arguments.get(cx, 0);
        if regexp_arg.is_object() {
            if is_regexp(cx, regexp_arg)? {
                let regexp_object = regexp_arg.as_object();

                let flags_string = get(cx, regexp_object, cx.names.flags())?;
                require_object_coercible(cx, flags_string)?;

                let has_global_flag = if let Some(regexp_object) = regexp_object.as_regexp_object()
                {
                    regexp_object.flags().is_global()
                } else {
                    let flags_string = to_string(cx, flags_string)?;

                    flags_string_contains(flags_string, 'g' as u32)?
                };

                if !has_global_flag {
                    return type_error(
                        cx,
                        "String.prototype.matchAll expects RegExp with global flag",
                    );
                }
            }

            let matcher = get_method(cx, regexp_arg, cx.symbols.match_all())?;
            if let Some(matcher) = matcher {
                return call_object(cx, matcher, regexp_arg, &[this_object]);
            }
        }

        let this_string = to_string(cx, this_object)?;

        let regexp_source = RegExpSource::PatternAndFlags(
            regexp_arg,
            FlagsSource::RegExpFlags(RegExpFlags::GLOBAL),
        );
        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let regexp_object = regexp_create(cx, regexp_source, regexp_constructor)?;

        invoke(cx, regexp_object, cx.symbols.match_all(), &[this_string.into()])
    }}

    runtime_fn! {
    /// String.prototype.normalize (https://tc39.es/ecma262/#sec-string.prototype.normalize)
    fn normalize(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "normalize")?;
        let string = to_string(cx, object)?;

        let form_arg = arguments.get(cx, 0);
        let form = if form_arg.is_undefined() {
            NormalizationForm::NFC
        } else {
            let form_string = *to_string(cx, form_arg)?.flatten()?;
            if form_string == cx.names.nfc.as_string().as_flat() {
                NormalizationForm::NFC
            } else if form_string == cx.names.nfd.as_string().as_flat() {
                NormalizationForm::NFD
            } else if form_string == cx.names.nfkc.as_string().as_flat() {
                NormalizationForm::NFKC
            } else if form_string == cx.names.nfkd.as_string().as_flat() {
                NormalizationForm::NFKD
            } else {
                return range_error(
                    cx,
                    "String.prototype.normalize normalization form must be 'NFC', 'NFD', 'NFKC', or 'NFKD'",
                );
            }
        };

        let normalized_string = match form {
            NormalizationForm::NFC => {
                normalize_string(cx, string, |iter| ICU.normalizers.nfc.normalize_iter(iter))?
            }
            NormalizationForm::NFD => {
                normalize_string(cx, string, |iter| ICU.normalizers.nfd.normalize_iter(iter))?
            }
            NormalizationForm::NFKC => {
                normalize_string(cx, string, |iter| ICU.normalizers.nfkc.normalize_iter(iter))?
            }
            NormalizationForm::NFKD => {
                normalize_string(cx, string, |iter| ICU.normalizers.nfkd.normalize_iter(iter))?
            }
        };

        Ok(normalized_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.padEnd (https://tc39.es/ecma262/#sec-string.prototype.padend)
    fn pad_end(cx, this_value, arguments) {
        let max_length_arg = arguments.get(cx, 0);
        let fill_string_arg = arguments.get(cx, 1);

        Self::pad_string(cx, this_value, max_length_arg, fill_string_arg, false, "padEnd")
    }}

    runtime_fn! {
    /// String.prototype.padStart (https://tc39.es/ecma262/#sec-string.prototype.padstart)
    fn pad_start(cx, this_value, arguments) {
        let max_length_arg = arguments.get(cx, 0);
        let fill_string_arg = arguments.get(cx, 1);

        Self::pad_string(cx, this_value, max_length_arg, fill_string_arg, true, "padStart")
    }}

    pub fn pad_string(
        cx: Context,
        this_value: Handle<Value>,
        max_length_arg: Handle<Value>,
        fill_string_arg: Handle<Value>,
        is_start: bool,
        method_name: &str,
    ) -> EvalResult<Handle<Value>> {
        let object = this_object_coercible_value(cx, this_value, method_name)?;
        let string = to_string(cx, object)?;

        let int_max_length = to_length(cx, max_length_arg)?;
        let string_length = string.len();

        // No need to pad as string already has max length
        if int_max_length <= string_length as u64 {
            return Ok(string.as_value());
        }

        let fill_string = if fill_string_arg.is_undefined() {
            cx.names.space().as_string()
        } else {
            to_string(cx, fill_string_arg)?
        };

        // Check for an empty padding string which would have no effect
        let fill_string_length = fill_string.len();
        if fill_string_length == 0 {
            return Ok(string.as_value());
        }

        let Ok(int_max_length) = u32::try_from(int_max_length) else {
            return range_error(
                cx,
                &format!(
                    "String.prototype.{} target string length exceeds maximum string size",
                    method_name
                ),
            );
        };
        let fill_length = int_max_length - string_length;

        // Find the number of whole pad strings we can fit into the padding length
        let num_whole_repetitions = fill_length / fill_string_length;
        let mut pad_string = fill_string.repeat(cx, num_whole_repetitions)?.as_string();

        // Add a partial pad string if the pad strings did not evenly divide the padding length
        let partial_length = fill_length - (num_whole_repetitions * fill_string_length);
        if partial_length != 0 {
            let partial_string = fill_string.substring(cx, 0, partial_length)?.as_string();
            pad_string = StringValue::concat(cx, pad_string, partial_string)?;
        }

        if is_start {
            Ok(StringValue::concat(cx, pad_string, string)?.as_value())
        } else {
            Ok(StringValue::concat(cx, string, pad_string)?.as_value())
        }
    }

    runtime_fn! {
    /// String.prototype.repeat (https://tc39.es/ecma262/#sec-string.prototype.repeat)
    fn repeat(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "repeat")?;
        let string = to_string(cx, object)?;

        let n_arg = arguments.get(cx, 0);
        let n = to_integer_or_infinity(cx, n_arg)?;
        if n.is_sign_negative() || n.is_infinite() {
            return range_error(
                cx,
                "String.prototype.repeat count must be a finite, non-negative number",
            );
        } else if n > u32::MAX_AS_F64 {
            return string_exceeds_max_length_error(cx);
        } else if n == 0.0 {
            return Ok(cx.names.empty_string().as_string().as_value());
        }

        Ok(string.repeat(cx, n as u32)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.replace (https://tc39.es/ecma262/#sec-string.prototype.replace)
    fn replace(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "replace")?;

        let search_arg = arguments.get(cx, 0);
        let replace_arg = arguments.get(cx, 1);

        // Use the @@replace method of the argument if one exists
        if search_arg.is_object() {
            let replacer = get_method(cx, search_arg, cx.symbols.replace())?;
            if let Some(replacer) = replacer {
                return call_object(cx, replacer, search_arg, &[object, replace_arg]);
            }
        }

        let target_string = to_string(cx, object)?;
        let search_string = to_string(cx, search_arg)?;

        let replace_value = if is_callable(replace_arg) {
            ReplaceValue::Function(replace_arg.as_object())
        } else {
            ReplaceValue::String(to_string(cx, replace_arg)?)
        };

        // Find the first match of the search string in the target string
        let matched_position = target_string.find(search_string, 0)?;
        let matched_position = if let Some(matched_position) = matched_position {
            matched_position
        } else {
            return Ok(target_string.as_value());
        };

        let replacement_string = match replace_value {
            // If replace argument is a function, replacement is the result of calling that function
            ReplaceValue::Function(replace_function) => {
                let matched_position_value = cx.number(matched_position);
                let replacement = call_object(
                    cx,
                    replace_function,
                    cx.undefined(),
                    &[
                        search_string.into(),
                        matched_position_value,
                        target_string.into(),
                    ],
                )?;

                to_string(cx, replacement)?
            }
            // Otherwise replacement is the result of calling GetSubstitution
            ReplaceValue::String(replace_string) => {
                let substitution_template =
                    SubstitutionTemplateParser::new(false).parse(cx, replace_string)?;
                must!(substitution_template.get_substitution(
                    cx,
                    target_string,
                    search_string,
                    matched_position,
                    &[],
                    None
                ))
            }
        };

        // Replace the matched substring with the replacement string
        let preceding_string = target_string
            .substring(cx, 0, matched_position)?
            .as_string();
        let following_string = target_string
            .substring(cx, matched_position + search_string.len(), target_string.len())?
            .as_string();

        let result_string =
            StringValue::concat_all(cx, &[preceding_string, replacement_string, following_string])?;

        Ok(result_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.replaceAll (https://tc39.es/ecma262/#sec-string.prototype.replaceall)
    fn replace_all(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "replaceAll")?;

        let search_arg = arguments.get(cx, 0);
        let replace_arg = arguments.get(cx, 1);

        // Use the @@replace method of the argument if one exists
        if search_arg.is_object() {
            // If search argument is a RegExp, check that it has the global flag
            if is_regexp(cx, search_arg)? {
                let search_regexp = search_arg.as_object();
                let flags_value = get(cx, search_regexp, cx.names.flags())?;

                require_object_coercible(cx, flags_value)?;

                let flags_string = to_string(cx, flags_value)?;

                if !flags_string_contains(flags_string, 'g' as u32)? {
                    return type_error(
                        cx,
                        "String.prototype.replaceAll expects RegExp with global flag",
                    );
                }
            }

            let replacer = get_method(cx, search_arg, cx.symbols.replace())?;
            if let Some(replacer) = replacer {
                return call_object(cx, replacer, search_arg, &[object, replace_arg]);
            }
        }

        let target_string = to_string(cx, object)?;
        let search_string = to_string(cx, search_arg)?;

        let replace_value = if is_callable(replace_arg) {
            ReplaceValue::Function(replace_arg.as_object())
        } else {
            ReplaceValue::String(to_string(cx, replace_arg)?)
        };

        // Collect positions of all matches of the search string in the target string
        let mut matched_positions = vec![];
        let advance_by = u32::max(1, search_string.len());

        let mut matched_position = target_string.find(search_string, 0)?;
        while let Some(position) = matched_position {
            matched_positions.push(position);
            matched_position = target_string.find(search_string, position + advance_by)?;
        }

        let mut string_parts = vec![];
        let mut end_of_last_match = 0;

        for matched_position in matched_positions {
            // Add the unchanged substring between the last match and this match
            let preserved_substring = target_string
                .substring(cx, end_of_last_match, matched_position)?
                .as_string();
            string_parts.push(preserved_substring);

            // Calculate the replacement string and add it
            let replacement_string = match replace_value {
                // If replace argument is a function, replacement is the result of calling that function
                ReplaceValue::Function(replace_function) => {
                    let matched_position_value = cx.number(matched_position);
                    let replacement = call_object(
                        cx,
                        replace_function,
                        cx.undefined(),
                        &[
                            search_string.into(),
                            matched_position_value,
                            target_string.into(),
                        ],
                    )?;

                    to_string(cx, replacement)?
                }
                // Otherwise replacement is the result of calling GetSubstitution
                ReplaceValue::String(replace_string) => {
                    let substitution_template =
                        SubstitutionTemplateParser::new(false).parse(cx, replace_string)?;
                    must!(substitution_template.get_substitution(
                        cx,
                        target_string,
                        search_string,
                        matched_position,
                        &[],
                        None
                    ))
                }
            };

            string_parts.push(replacement_string);

            end_of_last_match = matched_position + search_string.len();
        }

        // Add the unchanged substring after the last match
        if end_of_last_match < target_string.len() {
            let preserved_substring = target_string
                .substring(cx, end_of_last_match, target_string.len())?
                .as_string();
            string_parts.push(preserved_substring);
        }

        Ok(StringValue::concat_all(cx, &string_parts)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.search (https://tc39.es/ecma262/#sec-string.prototype.search)
    fn search(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "search")?;

        // Use the @@search method of the argument if one exists
        let regexp_arg = arguments.get(cx, 0);
        if regexp_arg.is_object() {
            let searcher = get_method(cx, regexp_arg, cx.symbols.search())?;
            if let Some(searcher) = searcher {
                return call_object(cx, searcher, regexp_arg, &[object]);
            }
        }

        let string = to_string(cx, object)?;

        // Otherwise create a RegExp from input string and use its @@search method
        let regexp_source = RegExpSource::PatternAndFlags(
            regexp_arg,
            FlagsSource::RegExpFlags(RegExpFlags::empty()),
        );
        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let regexp_object = regexp_create(cx, regexp_source, regexp_constructor)?;

        invoke(cx, regexp_object, cx.symbols.search(), &[string.into()])
    }}

    runtime_fn! {
    /// String.prototype.slice (https://tc39.es/ecma262/#sec-string.prototype.slice)
    fn slice(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "slice")?;
        let string = to_string(cx, object)?;
        let length = string.len();

        let start_arg = arguments.get(cx, 0);
        let start_index = resolve_relative_index_argument(cx, start_arg, length as u64)? as u32;

        let end_argument = arguments.get(cx, 1);
        let end_index = if !end_argument.is_undefined() {
            resolve_relative_index_argument(cx, end_argument, length as u64)? as u32
        } else {
            length
        };

        if start_index >= end_index {
            return Ok(cx.names.empty_string().as_string().as_value());
        }

        Ok(string.substring(cx, start_index, end_index)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.split (https://tc39.es/ecma262/#sec-string.prototype.split)
    fn split(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "split")?;

        let separator_argument = arguments.get(cx, 0);
        let limit_argument = arguments.get(cx, 1);

        // Use the @@split method of the separator if one exists
        if separator_argument.is_object() {
            let split_key = cx.symbols.split();
            let splitter = get_method(cx, separator_argument, split_key)?;

            if let Some(splitter) = splitter {
                return call_object(cx, splitter, separator_argument, &[object, limit_argument]);
            }
        }

        let string = to_string(cx, object)?;

        // Limit defaults to 2^32 - 1
        let limit = if limit_argument.is_undefined() {
            u32::MAX
        } else {
            to_uint32(cx, limit_argument)?
        };

        let separator = to_string(cx, separator_argument)?;

        if limit == 0 {
            let array_object = array_create(cx, 0, None)?;
            return Ok(array_object.as_value());
        } else if separator_argument.is_undefined() {
            return Ok(create_array_from_list(cx, &[string.into()])?.as_value());
        }

        // If separator is empty then return each code unit individually, up to the given limit
        let separator_length = separator.len();
        if separator_length == 0 {
            let mut code_unit_strings = vec![];
            let limit = u32::min(limit, string.len());

            for i in 0..limit {
                let substring = string.substring(cx, i, i + 1)?;
                code_unit_strings.push(substring.into());
            }

            return Ok(create_array_from_list(cx, &code_unit_strings)?.as_value());
        }

        // If the string is empty then it is the only substring
        let string_length = string.len();
        if string_length == 0 {
            return Ok(create_array_from_list(cx, &[string.into()])?.as_value());
        }

        let mut substrings = vec![];

        // Find the index of the first separator
        let mut i = 0;
        let mut next_separator_index_opt = string.find(separator, i)?;

        while let Some(next_separator_index) = next_separator_index_opt {
            // Add the substring up until the next separator
            let substring = string.substring(cx, i, next_separator_index)?.as_string();
            substrings.push(substring.into());

            // If we have reached the limit of substrings then return them
            if substrings.len() == limit as usize {
                return Ok(create_array_from_list(cx, &substrings)?.as_value());
            }

            // Find and skip the next separator
            i = next_separator_index + separator_length;

            // No next index if out of bounds. Make sure to clamp index to string length.
            if i >= string_length {
                i = string_length;
                break;
            }

            next_separator_index_opt = string.find(separator, i)?;
        }

        // Now that the last separator has the rest of the string is the last substring
        let last_substring = string.substring(cx, i, string.len())?.as_string();
        substrings.push(last_substring.into());

        Ok(create_array_from_list(cx, &substrings)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.startsWith (https://tc39.es/ecma262/#sec-string.prototype.startswith)
    fn starts_with(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "startsWith")?;
        let string = to_string(cx, object)?;
        let length = string.len();

        let search_value = arguments.get(cx, 0);
        if is_regexp(cx, search_value)? {
            return type_error(cx, "String.prototype.startsWith first argument cannot be a RegExp");
        }

        let search_string = to_string(cx, search_value)?;

        let start_index_argument = arguments.get(cx, 1);
        let start_index = if start_index_argument.is_undefined() {
            0
        } else {
            let start_index = to_integer_or_infinity(cx, start_index_argument)?;

            if start_index < 0.0 {
                0
            } else if start_index > (length as f64) {
                length
            } else {
                start_index as u32
            }
        };

        let search_length = search_string.len();
        if search_length == 0 {
            return Ok(cx.bool(true));
        }

        let end_index = start_index + search_length;
        if end_index > length {
            return Ok(cx.bool(false));
        }

        let starts_with_string = string.substring_equals(search_string, start_index)?;

        Ok(cx.bool(starts_with_string))
    }}

    runtime_fn! {
    /// String.prototype.substring (https://tc39.es/ecma262/#sec-string.prototype.substring)
    fn substring(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "substring")?;
        let string = to_string(cx, object)?;
        let length = string.len();

        let start_arg = arguments.get(cx, 0);
        let start = to_integer_or_infinity(cx, start_arg)?;
        let mut int_start = f64::max(0.0, f64::min(start, length as f64)) as u32;

        let end_argument = arguments.get(cx, 1);
        let mut int_end = if end_argument.is_undefined() {
            length
        } else {
            let end = to_integer_or_infinity(cx, end_argument)?;
            f64::max(0.0, f64::min(end, length as f64)) as u32
        };

        if int_end < int_start {
            std::mem::swap(&mut int_start, &mut int_end);
        }

        Ok(string.substring(cx, int_start, int_end)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.toLowerCase (https://tc39.es/ecma262/#sec-string.prototype.tolowercase)
    fn to_lower_case(cx, this_value, _) {
        let object = this_object_coercible_value(cx, this_value, "toLowerCase")?;
        let string = to_string(cx, object)?;

        Ok(string.to_lower_case(cx)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.toString (https://tc39.es/ecma262/#sec-string.prototype.tostring)
    fn to_string(cx, this_value, _) {
        this_string_value(cx, this_value, "toString")
    }}

    runtime_fn! {
    /// String.prototype.toUpperCase (https://tc39.es/ecma262/#sec-string.prototype.touppercase)
    fn to_upper_case(cx, this_value, _) {
        let object = this_object_coercible_value(cx, this_value, "toUpperCase")?;
        let string = to_string(cx, object)?;

        Ok(string.to_upper_case(cx)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.toWellFormed (https://tc39.es/ecma262/#sec-string.prototype.towellformed)
    fn to_well_formed(cx, this_value, _) {
        let object = this_object_coercible_value(cx, this_value, "toWellFormed")?;
        let string = to_string(cx, object)?;

        Ok(string.to_well_formed(cx)?.to_handle().as_value())
    }}

    runtime_fn! {
    /// String.prototype.trim (https://tc39.es/ecma262/#sec-string.prototype.trim)
    fn trim(cx, this_value, _) {
        let object = this_object_coercible_value(cx, this_value, "trim")?;
        let string = to_string(cx, object)?;

        Ok(string.trim(cx, true, true)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.trimEnd (https://tc39.es/ecma262/#sec-string.prototype.trimend)
    fn trim_end(cx, this_value, _) {
        let object = this_object_coercible_value(cx, this_value, "trimEnd")?;
        let string = to_string(cx, object)?;

        Ok(string.trim(cx, false, true)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.trimStart (https://tc39.es/ecma262/#sec-string.prototype.trimstart)
    fn trim_start(cx, this_value, _) {
        let object = this_object_coercible_value(cx, this_value, "trimStart")?;
        let string = to_string(cx, object)?;

        Ok(string.trim(cx, true, false)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-string.prototype-%symbol.iterator%)
    fn iterator(cx, this_value, _) {
        let object = this_object_coercible_value(cx, this_value, "iterator")?;
        let string = to_string(cx, object)?;

        let flat_string = string.flatten()?;

        Ok(StringIterator::new(cx, flat_string)?.as_value())
    }}

    runtime_fn! {
    /// String.prototype.substr (https://tc39.es/ecma262/#sec-string.prototype.substr)
    fn substr(cx, this_value, arguments) {
        let object = this_object_coercible_value(cx, this_value, "substr")?;
        let string = to_string(cx, object)?;

        let string_length = string.len() as u32;

        // Convert the start argument to an integer
        let start_arg = arguments.get(cx, 0);
        let start_index =
            resolve_relative_index_argument(cx, start_arg, string_length as u64)? as u32;

        // Second argument is the length
        let length_arg = arguments.get(cx, 1);
        let length = if length_arg.is_undefined() {
            string_length
        } else {
            let length = to_integer_or_infinity(cx, length_arg)?;
            length.clamp(0.0, string_length as f64) as u32
        };

        // Finally find the end index, given the start and length arguments
        let end_index = u32::min(start_index + length, string_length);

        Ok(string.substring(cx, start_index, end_index)?.as_value())
    }}

    /// CreateHTML (https://tc39.es/ecma262/#sec-createhtml)
    fn create_html(
        mut cx: Context,
        value: Handle<Value>,
        tag: &str,
        method_name: &str,
        attribute_and_value: Option<(&str, Handle<Value>)>,
    ) -> EvalResult<Handle<StringValue>> {
        let object = this_object_coercible_value(cx, value, method_name)?;
        let string = to_string(cx, object)?.to_wtf8_string()?;

        let mut html = Wtf8String::new();

        html.push_char('<');
        html.push_str(tag);

        if let Some((attribute, value)) = attribute_and_value {
            let value_string = to_string(cx, value)?.to_wtf8_string()?;

            html.push_char(' ');
            html.push_str(attribute);
            html.push_str("=\"");

            // Escape quotes in the value string
            for code_point in value_string.iter_code_points() {
                if code_point == '\"' as u32 {
                    html.push_str("&quot;");
                } else {
                    html.push(code_point);
                }
            }

            html.push_char('"');
        }

        html.push_char('>');
        html.push_wtf8_str(&string);
        html.push_str("</");
        html.push_str(tag);
        html.push_char('>');

        Ok(cx.alloc_wtf8_string(&html)?.as_string())
    }

    runtime_fn! {
    /// String.prototype.anchor (https://tc39.es/ecma262/#sec-string.prototype.anchor)
    fn anchor(cx, this_value, arguments) {
        let name_arg = arguments.get(cx, 0);
        let html_string =
            Self::create_html(cx, this_value, "a", "anchor", Some(("name", name_arg)))?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.big (https://tc39.es/ecma262/#sec-string.prototype.big)
    fn big(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "big", "big", None)?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.blink (https://tc39.es/ecma262/#sec-string.prototype.blink)
    fn blink(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "blink", "blink", None)?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.bold (https://tc39.es/ecma262/#sec-string.prototype.bold)
    fn bold(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "b", "bold", None)?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.fixed (https://tc39.es/ecma262/#sec-string.prototype.fixed)
    fn fixed(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "tt", "fixed", None)?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.fontcolor (https://tc39.es/ecma262/#sec-string.prototype.fontcolor)
    fn font_color(cx, this_value, arguments) {
        let color_arg = arguments.get(cx, 0);
        let html_string =
            Self::create_html(cx, this_value, "font", "fontcolor", Some(("color", color_arg)))?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.fontsize (https://tc39.es/ecma262/#sec-string.prototype.fontsize)
    fn font_size(cx, this_value, arguments) {
        let size_arg = arguments.get(cx, 0);
        let html_string =
            Self::create_html(cx, this_value, "font", "fontsize", Some(("size", size_arg)))?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.italics (https://tc39.es/ecma262/#sec-string.prototype.italics)
    fn italics(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "i", "italics", None)?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.link (https://tc39.es/ecma262/#sec-string.prototype.link)
    fn link(cx, this_value, arguments) {
        let url_arg = arguments.get(cx, 0);
        let html_string = Self::create_html(cx, this_value, "a", "link", Some(("href", url_arg)))?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.small (https://tc39.es/ecma262/#sec-string.prototype.small)
    fn small(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "small", "small", None)?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.strike (https://tc39.es/ecma262/#sec-string.prototype.strike)
    fn strike(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "strike", "strike", None)?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.sub (https://tc39.es/ecma262/#sec-string.prototype.sub)
    fn sub(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "sub", "sub", None)?;
        Ok(html_string.as_value())
    }}

    runtime_fn! {
    /// String.prototype.sup (https://tc39.es/ecma262/#sec-string.prototype.sup)
    fn sup(cx, this_value, _) {
        let html_string = Self::create_html(cx, this_value, "sup", "sup", None)?;
        Ok(html_string.as_value())
    }}
}

fn this_object_coercible_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<Value>> {
    if value.is_nullish() {
        return type_error(
            cx,
            &format!("String.prototype.{} called on null or undefined", method_name),
        );
    }

    Ok(value)
}

fn this_string_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<Value>> {
    if value.is_string() {
        return Ok(value);
    }

    if value.is_object() {
        let object_value = value.as_object();
        if let Some(string_object) = object_value.as_string_object() {
            return Ok(string_object.string_data().as_value());
        }
    }

    type_error(cx, &format!("String.prototype.{} must be called on a string", method_name))
}

#[allow(clippy::upper_case_acronyms)]
enum NormalizationForm {
    NFC,
    NFD,
    NFKC,
    NFKD,
}

enum StringPart {
    // A valid range of code points between the two indices [start, end)
    ValidRange(u32, u32),
    UnpairedSurrogate(u16),
}

/// Break up string into sequences of valid code points and individual unpaired surrogates
fn to_valid_string_parts(string: HeapPtr<FlatString>) -> Vec<StringPart> {
    match string.width() {
        StringWidth::OneByte => vec![StringPart::ValidRange(0, string.len())],
        StringWidth::TwoByte => {
            let mut parts = vec![];
            let mut start = 0;

            let code_units = string.as_two_byte_slice();
            let mut i = 0;

            while i < code_units.len() {
                let code_unit = code_units[i];
                if is_high_surrogate_code_unit(code_unit) {
                    // Valid surrogate pair
                    if i + 1 < code_units.len() && is_low_surrogate_code_unit(code_units[i + 1]) {
                        i += 2;
                        continue;
                    }

                    // Otherwise an unpaired high surrogate falls through
                } else if !is_low_surrogate_code_unit(code_unit) {
                    i += 1;
                    continue;
                }

                // Handle an unpaired high or low surrogate

                // First flush the valid range up until this unpaired surrogate, if there was such
                // a range.
                if i != start {
                    parts.push(StringPart::ValidRange(start as u32, i as u32));
                }

                parts.push(StringPart::UnpairedSurrogate(code_unit));

                i += 1;
                start = i;
            }

            // Flush the final valid range if one exists
            if i != start {
                parts.push(StringPart::ValidRange(start as u32, code_units.len() as u32));
            }

            parts
        }
    }
}

/// Wrapper around UnsafeCodePointIterator that returns chars. Must only be created for an
/// UnsafeCodePointIterator over valid unicode code points.
struct CharIterator {
    iter: UnsafeCodePointIterator,
}

impl CharIterator {
    pub fn new(iter: UnsafeCodePointIterator) -> Self {
        Self { iter }
    }
}

impl Iterator for CharIterator {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|code_point| unsafe { char::from_u32_unchecked(code_point) })
    }
}

/// Normalize the given string using a function that returns the ICU4X normalization iterator for
/// an iterator over valid code points.
fn normalize_string<I: Iterator<Item = char>>(
    cx: Context,
    string: Handle<StringValue>,
    f: impl Fn(CharIterator) -> I,
) -> EvalResult<Handle<StringValue>> {
    let parts = to_valid_string_parts(*string.flatten()?);

    let mut normalized_string = Wtf8String::new();

    for part in parts {
        match part {
            StringPart::ValidRange(start, end) => {
                let iter = CharIterator::new(string.iter_slice_code_points(start, end)?);
                for code_point in f(iter) {
                    normalized_string.push(code_point as CodePoint);
                }
            }
            StringPart::UnpairedSurrogate(code_unit) => {
                normalized_string.push(code_unit as CodePoint);
            }
        }
    }

    Ok(FlatString::from_wtf8(cx, normalized_string.as_bytes())?
        .as_string()
        .to_handle())
}

/// Replace argument may be either a function or string
pub enum ReplaceValue {
    Function(Handle<ObjectValue>),
    String(Handle<StringValue>),
}

pub struct SubstitutionTemplate {
    template: Handle<FlatString>,
    parts: Vec<SubstitutionPart>,
}

enum SubstitutionPart {
    /// A range of characters from the original string, [start, end)
    Range(u32, u32),
    /// The matched substring
    Match,
    /// The portion of the string before the match
    BeforeMatch,
    /// The portion of the string after the match
    AfterMatch,
    /// The indexed capture group with the given index. Also includes the [start, end) range of the
    /// full $NN portion of the original string.
    IndexedCapture(u8, u32, u32),
    /// The named capture group with the given name
    NamedCapture(Handle<StringValue>),
}

pub struct SubstitutionTemplateParser {
    parts: Vec<SubstitutionPart>,
    /// Current index in the string
    pos: u32,
    /// Start of the current range of literal characters
    current_range_start: u32,
    /// Whether to allow named captures
    allow_named_captures: bool,
}

impl SubstitutionTemplateParser {
    pub fn new(allow_named_captures: bool) -> Self {
        Self {
            parts: vec![],
            pos: 0,
            current_range_start: 0,
            allow_named_captures,
        }
    }

    fn flush_range_and_skip(&mut self, skip_length: u32) {
        if self.pos > self.current_range_start {
            self.parts
                .push(SubstitutionPart::Range(self.current_range_start, self.pos));
        }

        self.pos += skip_length;
        self.current_range_start = self.pos;
    }

    /// GetSubstitution (https://tc39.es/ecma262/#sec-getsubstitution) - precomputed portions
    pub fn parse(
        mut self,
        cx: Context,
        template: Handle<StringValue>,
    ) -> AllocResult<SubstitutionTemplate> {
        let template = template.flatten()?;
        let template_length = template.len();

        while self.pos < template_length {
            // The start of a substitution
            if template.code_unit_at(self.pos) == b'$' as u16 {
                // Check if we are already at the end of the template, in which case `$` is a literal
                if self.pos + 1 == template_length {
                    self.pos += 1;
                    continue;
                }

                let next_code_unit = template.code_unit_at(self.pos + 1);
                if next_code_unit == '$' as u16 {
                    // Escaped `$` is represented as a range of length 1 pointing to the `$`
                    let dollar_sign_pos = self.pos + 1;
                    self.flush_range_and_skip(2);
                    self.parts
                        .push(SubstitutionPart::Range(dollar_sign_pos, dollar_sign_pos + 1));
                } else if next_code_unit == '`' as u16 {
                    self.flush_range_and_skip(2);
                    self.parts.push(SubstitutionPart::BeforeMatch);
                } else if next_code_unit == '&' as u16 {
                    self.flush_range_and_skip(2);
                    self.parts.push(SubstitutionPart::Match);
                } else if next_code_unit == '\'' as u16 {
                    self.flush_range_and_skip(2);
                    self.parts.push(SubstitutionPart::AfterMatch);
                } else if is_decimal_digit(next_code_unit as u32) {
                    // Must be an indexed capture group
                    let start = self.pos;

                    let mut capture_index = next_code_unit as u8 - b'0';
                    let mut skip_length = 2;

                    // Check if the capture index is two digits
                    if self.pos + 2 < template_length {
                        let next_code_unit = template.code_unit_at(self.pos + 2);
                        if is_decimal_digit(next_code_unit as u32) {
                            capture_index = capture_index * 10 + (next_code_unit as u8 - b'0');
                            skip_length = 3;
                        }
                    }

                    // `$0` or `$00` are interpreted as literals
                    if capture_index == 0 {
                        self.pos += 1;
                        continue;
                    }

                    self.flush_range_and_skip(skip_length);
                    self.parts.push(SubstitutionPart::IndexedCapture(
                        capture_index,
                        start,
                        self.pos,
                    ));
                } else if next_code_unit == '<' as u16 && self.allow_named_captures {
                    // Find the closing `>` for the capture name
                    let mut name_end_pos = self.pos + 2;
                    while name_end_pos < template_length
                        && template.code_unit_at(name_end_pos) != '>' as u16
                    {
                        name_end_pos += 1;
                    }

                    // Did not find closing `>` so `$<` is a literal
                    if name_end_pos == template_length {
                        self.pos += 2;
                        continue;
                    }

                    // Must be a named capture group, so extract name
                    let name = template
                        .substring(cx, self.pos + 2, name_end_pos)?
                        .as_string();

                    self.flush_range_and_skip(name_end_pos - self.pos + 1);
                    self.parts.push(SubstitutionPart::NamedCapture(name));
                } else {
                    // Otherwise was a `$` literal
                    self.pos += 1;
                }
            } else {
                self.pos += 1;
            }
        }

        self.flush_range_and_skip(0);

        Ok(SubstitutionTemplate { parts: self.parts, template })
    }
}

impl SubstitutionTemplate {
    /// GetSubstitution (https://tc39.es/ecma262/#sec-getsubstitution)
    pub fn get_substitution(
        &self,
        cx: Context,
        target_string: Handle<StringValue>,
        matched_string: Handle<StringValue>,
        matched_position: u32,
        indexed_captures: &[Option<Handle<StringValue>>],
        named_captures: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<StringValue>> {
        let mut string_parts = vec![];

        for template_part in &self.parts {
            match template_part {
                SubstitutionPart::Range(start, end) => {
                    let substring = self.template.substring(cx, *start, *end)?.as_string();
                    string_parts.push(substring);
                }
                SubstitutionPart::Match => {
                    string_parts.push(matched_string);
                }
                SubstitutionPart::BeforeMatch => {
                    let substring = target_string
                        .substring(cx, 0, matched_position)?
                        .as_string();
                    string_parts.push(substring);
                }
                SubstitutionPart::AfterMatch => {
                    let target_string_length = target_string.len();
                    let after_match_pos =
                        u32::min(matched_position + matched_string.len(), target_string_length);

                    let substring = target_string
                        .substring(cx, after_match_pos, target_string_length)?
                        .as_string();
                    string_parts.push(substring);
                }
                SubstitutionPart::IndexedCapture(index, start, end) => {
                    let index = *index as usize;

                    // Only use capture group if index is in range
                    if index <= indexed_captures.len() {
                        if let Some(captured_string) = indexed_captures.get(index - 1).unwrap() {
                            string_parts.push(*captured_string);
                        }

                        continue;
                    }

                    // If index is multiple digits but out of range, instead treat as a single digit
                    // followed by a literal.
                    if index >= 10 {
                        // Check if the first digit is in range
                        let first_digit_index = index / 10;
                        if first_digit_index <= indexed_captures.len() {
                            if let Some(captured_string) =
                                indexed_captures.get(first_digit_index - 1).unwrap()
                            {
                                string_parts.push(*captured_string);
                            }

                            // Append second digit as a literal
                            let second_digit_char = (index % 10) as u32 + '0' as u32;
                            let second_digit_string =
                                FlatString::from_code_point(cx, second_digit_char)?;
                            string_parts.push(second_digit_string.as_string().to_handle());

                            continue;
                        }
                    }

                    // Otherwise treat as a literal
                    let substring = self.template.substring(cx, *start, *end)?.as_string();
                    string_parts.push(substring);
                }
                SubstitutionPart::NamedCapture(name) => {
                    if let Some(named_captures) = named_captures {
                        let key = PropertyKey::string_handle(cx, *name)?;
                        let captured_string = get(cx, named_captures, key)?;

                        if !captured_string.is_undefined() {
                            let captured_string = to_string(cx, captured_string)?;
                            string_parts.push(captured_string);
                        }
                    }
                }
            }
        }

        StringValue::concat_all(cx, &string_parts)
    }
}
