use std::cmp::Ordering;

use crate::{
    js::{
        common::{
            icu::ICU,
            unicode::{
                is_decimal_digit, is_high_surrogate_code_unit, is_low_surrogate_code_unit,
                CodePoint,
            },
            wtf_8::Wtf8String,
        },
        parser::regexp::RegExpFlags,
        runtime::{
            abstract_operations::{call_object, get_method, invoke},
            array_object::{array_create, create_array_from_list},
            error::{range_error, type_error},
            eval_result::EvalResult,
            function::get_argument,
            get,
            interned_strings::InternedStrings,
            intrinsics::{
                intrinsics::Intrinsic,
                regexp_constructor::{regexp_create, RegExpSource},
                regexp_prototype::flags_string_contains,
                string_iterator::StringIterator,
            },
            numeric_constants::MAX_U32_AS_F64,
            object_value::ObjectValue,
            realm::Realm,
            string_object::StringObject,
            string_value::{CodePointIterator, FlatString, StringValue, StringWidth},
            to_string,
            type_utilities::{
                is_callable, is_regexp, require_object_coercible, to_integer_or_infinity,
                to_length, to_number, to_uint32,
            },
            value::Value,
            Context, Handle, HeapPtr, PropertyKey,
        },
    },
    must,
};

use super::regexp_constructor::FlagsSource;

pub struct StringPrototype;

impl StringPrototype {
    /// Properties of the String Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-string-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let empty_string = cx.names.empty_string().as_string();
        let mut object = StringObject::new_with_proto(cx, object_proto, empty_string).as_object();

        // Constructor property is added once StringConstructor has been created
        object.intrinsic_func(cx, cx.names.at(), Self::at, 1, realm);
        object.intrinsic_func(cx, cx.names.char_at(), Self::char_at, 1, realm);
        object.intrinsic_func(cx, cx.names.char_code_at(), Self::char_code_at, 1, realm);
        object.intrinsic_func(cx, cx.names.code_point_at(), Self::code_point_at, 1, realm);
        object.intrinsic_func(cx, cx.names.concat(), Self::concat, 1, realm);
        object.intrinsic_func(cx, cx.names.ends_with(), Self::ends_with, 1, realm);
        object.intrinsic_func(cx, cx.names.includes(), Self::includes, 1, realm);
        object.intrinsic_func(cx, cx.names.index_of(), Self::index_of, 1, realm);
        object.intrinsic_func(cx, cx.names.is_well_formed(), Self::is_well_formed, 0, realm);
        object.intrinsic_func(cx, cx.names.last_index_of(), Self::last_index_of, 1, realm);
        object.intrinsic_func(cx, cx.names.locale_compare(), Self::locale_compare, 1, realm);
        object.intrinsic_func(cx, cx.names.match_(), Self::match_, 1, realm);
        object.intrinsic_func(cx, cx.names.match_all(), Self::match_all, 1, realm);
        object.intrinsic_func(cx, cx.names.normalize(), Self::normalize, 0, realm);
        object.intrinsic_func(cx, cx.names.pad_end(), Self::pad_end, 1, realm);
        object.intrinsic_func(cx, cx.names.pad_start(), Self::pad_start, 1, realm);
        object.intrinsic_func(cx, cx.names.repeat(), Self::repeat, 1, realm);
        object.intrinsic_func(cx, cx.names.replace(), Self::replace, 2, realm);
        object.intrinsic_func(cx, cx.names.replace_all(), Self::replace_all, 2, realm);
        object.intrinsic_func(cx, cx.names.search(), Self::search, 1, realm);
        object.intrinsic_func(cx, cx.names.slice(), Self::slice, 2, realm);
        object.intrinsic_func(cx, cx.names.split(), Self::split, 2, realm);
        object.intrinsic_func(cx, cx.names.starts_with(), Self::starts_with, 1, realm);
        object.intrinsic_func(cx, cx.names.substring(), Self::substring, 2, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, cx.names.to_locale_lower_case(), Self::to_lower_case, 0, realm);
        object.intrinsic_func(cx, cx.names.to_locale_upper_case(), Self::to_upper_case, 0, realm);
        object.intrinsic_func(cx, cx.names.to_lower_case(), Self::to_lower_case, 0, realm);
        object.intrinsic_func(cx, cx.names.to_upper_case(), Self::to_upper_case, 0, realm);
        object.intrinsic_func(cx, cx.names.to_well_formed(), Self::to_well_formed, 0, realm);
        object.intrinsic_func(cx, cx.names.trim(), Self::trim, 0, realm);
        object.intrinsic_func(cx, cx.names.trim_end(), Self::trim_end, 0, realm);
        object.intrinsic_func(cx, cx.names.trim_start(), Self::trim_start, 0, realm);
        object.intrinsic_func(cx, cx.names.value_of(), Self::to_string, 0, realm);

        // String.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-string.prototype-%symbol.iterator%)
        let iterator_key = cx.well_known_symbols.iterator();
        object.intrinsic_func(cx, iterator_key, Self::iterator, 0, realm);

        object
    }

    /// String.prototype.at (https://tc39.es/ecma262/#sec-string.prototype.at)
    pub fn at(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let length = string.len() as i64;

        let index_arg = get_argument(cx, arguments, 0);
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

        let code_unit_string = FlatString::from_code_unit(cx, string.code_unit_at(index as u32));

        Ok(code_unit_string.as_value())
    }

    /// String.prototype.charAt (https://tc39.es/ecma262/#sec-string.prototype.charat)
    pub fn char_at(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let position_arg = get_argument(cx, arguments, 0);
        let position = to_integer_or_infinity(cx, position_arg)?;

        if position < 0.0 || position >= string.len() as f64 {
            return Ok(cx.names.empty_string().as_string().as_value());
        }

        let char_string = FlatString::from_code_unit(cx, string.code_unit_at(position as u32));

        Ok(char_string.as_value())
    }

    /// String.prototype.charCodeAt (https://tc39.es/ecma262/#sec-string.prototype.charcodeat)
    pub fn char_code_at(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let position_arg = get_argument(cx, arguments, 0);
        let position = to_integer_or_infinity(cx, position_arg)?;

        if position < 0.0 || position >= string.len() as f64 {
            return Ok(cx.nan());
        }

        let code_unit = string.code_unit_at(position as u32);
        Ok(cx.smi(code_unit as i32))
    }

    /// String.prototype.codePointAt (https://tc39.es/ecma262/#sec-string.prototype.codepointat)
    pub fn code_point_at(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let position_arg = get_argument(cx, arguments, 0);
        let position = to_integer_or_infinity(cx, position_arg)?;

        if position < 0.0 || position >= string.len() as f64 {
            return Ok(cx.undefined());
        }

        let code_point = string.code_point_at(position as u32);
        Ok(cx.smi(code_point as i32))
    }

    /// String.prototype.concat (https://tc39.es/ecma262/#sec-string.prototype.concat)
    pub fn concat(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let mut concat_string = to_string(cx, object)?;

        for argument in arguments {
            let string = to_string(cx, *argument)?;
            concat_string = StringValue::concat(cx, concat_string, string);
        }

        Ok(concat_string.as_value())
    }

    /// String.prototype.endsWith (https://tc39.es/ecma262/#sec-string.prototype.endswith)
    pub fn ends_with(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;
        let length = string.len();

        let search_value = get_argument(cx, arguments, 0);
        if is_regexp(cx, search_value)? {
            return type_error(cx, "first argument to endsWith cannot be a RegExp");
        }

        let search_string = to_string(cx, search_value)?;

        let end_index_argument = get_argument(cx, arguments, 1);
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

        let ends_with_string = string.substring_equals(search_string, start_index);

        Ok(cx.bool(ends_with_string))
    }

    /// String.prototype.includes (https://tc39.es/ecma262/#sec-string.prototype.includes)
    pub fn includes(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let search_string = get_argument(cx, arguments, 0);
        if is_regexp(cx, search_string)? {
            return type_error(cx, "String.prototype.includes cannot take a regular expression");
        }

        let search_string = to_string(cx, search_string)?;

        let pos_arg = get_argument(cx, arguments, 1);
        let pos = to_integer_or_infinity(cx, pos_arg)?;
        let pos = pos.clamp(0.0, string.len() as f64) as u32;

        if pos > string.len() {
            return Ok(cx.bool(false));
        }

        let found_search_string = string.find(search_string, pos).is_some();
        Ok(cx.bool(found_search_string))
    }

    /// String.prototype.indexOf (https://tc39.es/ecma262/#sec-string.prototype.indexof)
    pub fn index_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let search_arg = get_argument(cx, arguments, 0);
        let search_string = to_string(cx, search_arg)?;

        let pos_arg = get_argument(cx, arguments, 1);
        let pos = to_integer_or_infinity(cx, pos_arg)?;
        let pos = pos.clamp(0.0, string.len() as f64) as u32;

        if pos > string.len() {
            return Ok(cx.negative_one());
        }

        match string.find(search_string, pos) {
            None => Ok(cx.negative_one()),
            Some(index) => Ok(Value::from(index).to_handle(cx)),
        }
    }

    /// String.prototype.isWellFormed (https://tc39.es/ecma262/#sec-string.prototype.iswellformed)
    pub fn is_well_formed(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        Ok(cx.bool(string.is_well_formed()))
    }

    /// String.prototype.lastIndexOf (https://tc39.es/ecma262/#sec-string.prototype.lastindexof)
    pub fn last_index_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let search_arg = get_argument(cx, arguments, 0);
        let search_string = to_string(cx, search_arg)?;

        let pos_arg = get_argument(cx, arguments, 1);
        let num_pos = to_number(cx, pos_arg)?;

        let pos = if num_pos.is_nan() {
            f64::INFINITY
        } else {
            to_integer_or_infinity(cx, num_pos)?
        };

        let string_end = pos.clamp(0.0, string.len() as f64) as u32;

        match string.rfind(search_string, string_end) {
            None => Ok(cx.negative_one()),
            Some(index) => Ok(Value::from(index).to_handle(cx)),
        }
    }

    /// String.prototype.localeCompare (https://tc39.es/ecma262/#sec-string.prototype.localecompare)
    pub fn locale_compare(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other_string = to_string(cx, other_arg)?;

        let wtf8_string = string.to_wtf8_string();
        let wtf8_other_string = other_string.to_wtf8_string();
        let comparison = ICU
            .collator
            .as_borrowed()
            .compare_utf8(wtf8_string.as_bytes(), wtf8_other_string.as_bytes());

        let comparison_number = match comparison {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        };

        Ok(cx.smi(comparison_number))
    }

    /// String.prototype.match (https://tc39.es/ecma262/#sec-string.prototype.match)
    pub fn match_(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_object = require_object_coercible(cx, this_value)?;

        let regexp_arg = get_argument(cx, arguments, 0);
        if !regexp_arg.is_nullish() {
            let matcher = get_method(cx, regexp_arg, cx.well_known_symbols.match_())?;
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

        invoke(cx, regexp_object, cx.well_known_symbols.match_(), &[this_string.into()])
    }

    /// String.prototype.matchAll (https://tc39.es/ecma262/#sec-string.prototype.matchall)
    pub fn match_all(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_object = require_object_coercible(cx, this_value)?;

        let regexp_arg = get_argument(cx, arguments, 0);
        if !regexp_arg.is_nullish() {
            if is_regexp(cx, regexp_arg)? {
                let regexp_object = regexp_arg.as_object();

                let flags_string = get(cx, regexp_object, cx.names.flags())?;
                require_object_coercible(cx, flags_string)?;

                let has_global_flag = if let Some(regexp_object) = regexp_object.as_regexp_object()
                {
                    regexp_object.flags().is_global()
                } else {
                    let flags_string = to_string(cx, flags_string)?;

                    flags_string_contains(flags_string, 'g' as u32)
                };

                if !has_global_flag {
                    return type_error(
                        cx,
                        "String.prototype.matchAll expects RegExp with global flag",
                    );
                }
            }

            let matcher = get_method(cx, regexp_arg, cx.well_known_symbols.match_all())?;
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

        invoke(cx, regexp_object, cx.well_known_symbols.match_all(), &[this_string.into()])
    }

    /// String.prototype.normalize (https://tc39.es/ecma262/#sec-string.prototype.normalize)
    pub fn normalize(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let form_arg = get_argument(cx, arguments, 0);
        let form = if form_arg.is_undefined() {
            NormalizationForm::NFC
        } else {
            let form_string = *to_string(cx, form_arg)?.flatten();
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
                normalize_string(cx, string, |iter| ICU.normalizers.nfc.normalize_iter(iter))
            }
            NormalizationForm::NFD => {
                normalize_string(cx, string, |iter| ICU.normalizers.nfd.normalize_iter(iter))
            }
            NormalizationForm::NFKC => {
                normalize_string(cx, string, |iter| ICU.normalizers.nfkc.normalize_iter(iter))
            }
            NormalizationForm::NFKD => {
                normalize_string(cx, string, |iter| ICU.normalizers.nfkd.normalize_iter(iter))
            }
        };

        Ok(normalized_string.as_value())
    }

    /// String.prototype.padEnd (https://tc39.es/ecma262/#sec-string.prototype.padend)
    pub fn pad_end(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let max_length_arg = get_argument(cx, arguments, 0);
        let fill_string_arg = get_argument(cx, arguments, 1);

        Self::pad_string(cx, this_value, max_length_arg, fill_string_arg, false)
    }

    /// String.prototype.padStart (https://tc39.es/ecma262/#sec-string.prototype.padstart)
    pub fn pad_start(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let max_length_arg = get_argument(cx, arguments, 0);
        let fill_string_arg = get_argument(cx, arguments, 1);

        Self::pad_string(cx, this_value, max_length_arg, fill_string_arg, true)
    }

    pub fn pad_string(
        cx: Context,
        this_value: Handle<Value>,
        max_length_arg: Handle<Value>,
        fill_string_arg: Handle<Value>,
        is_start: bool,
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let int_max_length = to_length(cx, max_length_arg)?;
        let string_length = string.len();

        // No need to pad as string already has max length
        if int_max_length <= string_length as u64 {
            return Ok(string.as_value());
        } else if int_max_length > u32::MAX as u64 {
            return range_error(cx, "target string length exceeds maximum string size");
        }

        let int_max_length = int_max_length as u32;

        let fill_string = if fill_string_arg.is_undefined() {
            InternedStrings::get_str(cx, " ")
        } else {
            to_string(cx, fill_string_arg)?
        };

        let fill_length = int_max_length - string_length;
        let fill_string_length = fill_string.len();

        // Check for an empty padding string which would have no effect
        if fill_string_length == 0 {
            return Ok(string.as_value());
        }

        // Find the number of whole pad strings we can fit into the padding length
        let num_whole_repitions = fill_length / fill_string_length;
        let mut pad_string = fill_string.repeat(cx, num_whole_repitions).as_string();

        // Add a partial pad string if the pad strings did not evenly divide the padding length
        let partial_length = fill_length - (num_whole_repitions * fill_string_length);
        if partial_length != 0 {
            let partial_string = fill_string.substring(cx, 0, partial_length).as_string();
            pad_string = StringValue::concat(cx, pad_string, partial_string);
        }

        if is_start {
            Ok(StringValue::concat(cx, pad_string, string).as_value())
        } else {
            Ok(StringValue::concat(cx, string, pad_string).as_value())
        }
    }

    /// String.prototype.repeat (https://tc39.es/ecma262/#sec-string.prototype.repeat)
    pub fn repeat(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let n_arg = get_argument(cx, arguments, 0);
        let n = to_integer_or_infinity(cx, n_arg)?;
        if !(0.0..=MAX_U32_AS_F64).contains(&n) {
            return range_error(cx, "count must be a finite, positive number that does not exceed the maximum string size");
        } else if n == 0.0 {
            return Ok(cx.names.empty_string().as_string().as_value());
        }

        Ok(string.repeat(cx, n as u32).as_value())
    }

    /// String.prototype.replace (https://tc39.es/ecma262/#sec-string.prototype.replace)
    pub fn replace(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;

        let search_arg = get_argument(cx, arguments, 0);
        let replace_arg = get_argument(cx, arguments, 1);

        // Use the @@replace method of the argument if one exists
        if !search_arg.is_nullish() {
            let replacer = get_method(cx, search_arg, cx.well_known_symbols.replace())?;
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
        let matched_position = target_string.find(search_string, 0);
        let matched_position = if let Some(matched_position) = matched_position {
            matched_position
        } else {
            return Ok(target_string.as_value());
        };

        let replacement_string = match replace_value {
            // If replace argument is a function, replacement is the result of calling that function
            ReplaceValue::Function(replace_function) => {
                let matched_position_value = Value::from(matched_position).to_handle(cx);
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
                    SubstitutionTemplateParser::new(false).parse(cx, replace_string);
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
        let preceding_string = target_string.substring(cx, 0, matched_position).as_string();
        let following_string = target_string
            .substring(cx, matched_position + search_string.len(), target_string.len())
            .as_string();

        let result_string =
            StringValue::concat_all(cx, &[preceding_string, replacement_string, following_string]);

        Ok(result_string.as_value())
    }

    /// String.prototype.replaceAll (https://tc39.es/ecma262/#sec-string.prototype.replaceall)
    pub fn replace_all(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;

        let search_arg = get_argument(cx, arguments, 0);
        let replace_arg = get_argument(cx, arguments, 1);

        // Use the @@replace method of the argument if one exists
        if !search_arg.is_nullish() {
            // If search argument is a RegExp, check that it has the global flag
            if is_regexp(cx, search_arg)? {
                let search_regexp = search_arg.as_object();
                let flags_value = get(cx, search_regexp, cx.names.flags())?;

                require_object_coercible(cx, flags_value)?;

                let flags_string = to_string(cx, flags_value)?;

                if !flags_string_contains(flags_string, 'g' as u32) {
                    return type_error(
                        cx,
                        "String.prototype.replaceAll expects RegExp with global flag",
                    );
                }
            }

            let replacer = get_method(cx, search_arg, cx.well_known_symbols.replace())?;
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

        let mut matched_position = target_string.find(search_string, 0);
        while let Some(position) = matched_position {
            matched_positions.push(position);
            matched_position = target_string.find(search_string, position + advance_by);
        }

        let mut string_parts = vec![];
        let mut end_of_last_match = 0;

        for matched_position in matched_positions {
            // Add the unchanged substring between the last match and this match
            let preserved_substring = target_string
                .substring(cx, end_of_last_match, matched_position)
                .as_string();
            string_parts.push(preserved_substring);

            // Calculate the replacement string and add it
            let replacement_string = match replace_value {
                // If replace argument is a function, replacement is the result of calling that function
                ReplaceValue::Function(replace_function) => {
                    let matched_position_value = Value::from(matched_position).to_handle(cx);
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
                        SubstitutionTemplateParser::new(false).parse(cx, replace_string);
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
                .substring(cx, end_of_last_match, target_string.len())
                .as_string();
            string_parts.push(preserved_substring);
        }

        Ok(StringValue::concat_all(cx, &string_parts).as_value())
    }

    /// String.prototype.search (https://tc39.es/ecma262/#sec-string.prototype.search)
    pub fn search(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;

        // Use the @@search method of the argument if one exists
        let regexp_arg = get_argument(cx, arguments, 0);
        if !regexp_arg.is_nullish() {
            let searcher = get_method(cx, regexp_arg, cx.well_known_symbols.search())?;
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

        invoke(cx, regexp_object, cx.well_known_symbols.search(), &[string.into()])
    }

    /// String.prototype.slice (https://tc39.es/ecma262/#sec-string.prototype.slice)
    pub fn slice(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;
        let length = string.len();

        let start_arg = get_argument(cx, arguments, 0);
        let relative_start = to_integer_or_infinity(cx, start_arg)?;
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u32
            }
        } else {
            u32::min(relative_start as u32, length)
        };

        let end_argument = get_argument(cx, arguments, 1);
        let end_index = if !end_argument.is_undefined() {
            let relative_end = to_integer_or_infinity(cx, end_argument)?;

            if relative_end < 0.0 {
                if relative_end == f64::NEG_INFINITY {
                    0
                } else {
                    i64::max(length as i64 + relative_end as i64, 0) as u32
                }
            } else {
                u32::min(relative_end as u32, length)
            }
        } else {
            length
        };

        if start_index >= end_index {
            return Ok(cx.names.empty_string().as_string().as_value());
        }

        Ok(string.substring(cx, start_index, end_index).as_value())
    }

    /// String.prototype.split (https://tc39.es/ecma262/#sec-string.prototype.split)
    pub fn split(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;

        let separator_argument = get_argument(cx, arguments, 0);
        let limit_argument = get_argument(cx, arguments, 1);

        // Use the @@split method of the separator if one exists
        if !separator_argument.is_nullish() {
            let split_key = cx.well_known_symbols.split();
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
            return Ok(create_array_from_list(cx, &[string.into()]).as_value());
        }

        // If separator is empty then return each code unit individually, up to the given limit
        let separator_length = separator.len();
        if separator_length == 0 {
            let mut code_unit_strings = vec![];
            let limit = u32::min(limit, string.len());

            for i in 0..limit {
                let substring = string.substring(cx, i, i + 1);
                code_unit_strings.push(substring.into());
            }

            return Ok(create_array_from_list(cx, &code_unit_strings).as_value());
        }

        // If the string is empty then it is the only substring
        let string_length = string.len();
        if string_length == 0 {
            return Ok(create_array_from_list(cx, &[string.into()]).as_value());
        }

        let mut substrings = vec![];

        // Find the index of the first separator
        let mut i = 0;
        let mut next_separator_index_opt = string.find(separator, i);

        while let Some(next_separator_index) = next_separator_index_opt {
            // Add the substring up until the next separator
            let substring = string.substring(cx, i, next_separator_index).as_string();
            substrings.push(substring.into());

            // If we have reached the limit of substrings then return them
            if substrings.len() == limit as usize {
                return Ok(create_array_from_list(cx, &substrings).as_value());
            }

            // Find and skip the next separator
            i = next_separator_index + separator_length;

            // No next index if out of bounds. Make sure to clamp index to string length.
            if i >= string_length {
                i = string_length;
                break;
            }

            next_separator_index_opt = string.find(separator, i);
        }

        // Now that the last separator has the rest of the string is the last substring
        let last_substring = string.substring(cx, i, string.len()).as_string();
        substrings.push(last_substring.into());

        Ok(create_array_from_list(cx, &substrings).as_value())
    }

    /// String.prototype.startsWith (https://tc39.es/ecma262/#sec-string.prototype.startswith)
    pub fn starts_with(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;
        let length = string.len();

        let search_value = get_argument(cx, arguments, 0);
        if is_regexp(cx, search_value)? {
            return type_error(cx, "first argument to startsWith cannot be a RegExp");
        }

        let search_string = to_string(cx, search_value)?;

        let start_index_argument = get_argument(cx, arguments, 1);
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

        let starts_with_string = string.substring_equals(search_string, start_index);

        Ok(cx.bool(starts_with_string))
    }

    /// String.prototype.substring (https://tc39.es/ecma262/#sec-string.prototype.substring)
    pub fn substring(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;
        let length = string.len();

        let start_arg = get_argument(cx, arguments, 0);
        let start = to_integer_or_infinity(cx, start_arg)?;
        let mut int_start = f64::max(0.0, f64::min(start, length as f64)) as u32;

        let end_argument = get_argument(cx, arguments, 1);
        let mut int_end = if end_argument.is_undefined() {
            length
        } else {
            let end = to_integer_or_infinity(cx, end_argument)?;
            f64::max(0.0, f64::min(end, length as f64)) as u32
        };

        if int_end < int_start {
            std::mem::swap(&mut int_start, &mut int_end);
        }

        Ok(string.substring(cx, int_start, int_end).as_value())
    }

    /// String.prototype.toLowerCase (https://tc39.es/ecma262/#sec-string.prototype.tolowercase)
    pub fn to_lower_case(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        Ok(string.to_lower_case(cx).as_value())
    }

    /// String.prototype.toString (https://tc39.es/ecma262/#sec-string.prototype.tostring)
    pub fn to_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        this_string_value(cx, this_value)
    }

    /// String.prototype.toUpperCase (https://tc39.es/ecma262/#sec-string.prototype.touppercase)
    pub fn to_upper_case(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        Ok(string.to_upper_case(cx).as_value())
    }

    /// String.prototype.toWellFormed (https://tc39.es/ecma262/#sec-string.prototype.towellformed)
    pub fn to_well_formed(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        Ok(string.to_well_formed(cx).to_handle().as_value())
    }

    /// String.prototype.trim (https://tc39.es/ecma262/#sec-string.prototype.trim)
    pub fn trim(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        Ok(string.trim(cx, true, true).as_value())
    }

    /// String.prototype.trimEnd (https://tc39.es/ecma262/#sec-string.prototype.trimend)
    pub fn trim_end(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        Ok(string.trim(cx, false, true).as_value())
    }

    /// String.prototype.trimStart (https://tc39.es/ecma262/#sec-string.prototype.trimstart)
    pub fn trim_start(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        Ok(string.trim(cx, true, false).as_value())
    }

    /// String.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-string.prototype-%symbol.iterator%)
    pub fn iterator(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;
        let string = to_string(cx, object)?;

        let flat_string = string.flatten();

        Ok(StringIterator::new(cx, flat_string).as_value())
    }
}

fn this_string_value(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    if value.is_string() {
        return Ok(value);
    }

    if value.is_object() {
        let object_value = value.as_object();
        if let Some(string_object) = object_value.as_string_object() {
            return Ok(string_object.string_data().as_value());
        }
    }

    type_error(cx, "value cannot be converted to string")
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

/// Wrapper around CodePointIterator that returns chars. Must only be created for CodePointIterators
/// over valid unicode code points.
struct CharIterator {
    iter: CodePointIterator,
}

impl CharIterator {
    pub fn new(iter: CodePointIterator) -> Self {
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
) -> Handle<StringValue> {
    let parts = to_valid_string_parts(*string.flatten());

    let mut normalized_string = Wtf8String::new();

    for part in parts {
        match part {
            StringPart::ValidRange(start, end) => {
                let iter = CharIterator::new(string.iter_slice_code_points(start, end));
                for code_point in f(iter) {
                    normalized_string.push(code_point as CodePoint);
                }
            }
            StringPart::UnpairedSurrogate(code_unit) => {
                normalized_string.push(code_unit as CodePoint);
            }
        }
    }

    FlatString::from_wtf8(cx, normalized_string.as_bytes())
        .as_string()
        .to_handle()
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
    pub fn parse(mut self, cx: Context, template: Handle<StringValue>) -> SubstitutionTemplate {
        let template = template.flatten();
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
                        .substring(cx, self.pos + 2, name_end_pos)
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

        SubstitutionTemplate { parts: self.parts, template }
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
                    let substring = self.template.substring(cx, *start, *end).as_string();
                    string_parts.push(substring);
                }
                SubstitutionPart::Match => {
                    string_parts.push(matched_string);
                }
                SubstitutionPart::BeforeMatch => {
                    let substring = target_string.substring(cx, 0, matched_position).as_string();
                    string_parts.push(substring);
                }
                SubstitutionPart::AfterMatch => {
                    let target_string_length = target_string.len();
                    let after_match_pos =
                        u32::min(matched_position + matched_string.len(), target_string_length);

                    let substring = target_string
                        .substring(cx, after_match_pos, target_string_length)
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
                                FlatString::from_code_point(cx, second_digit_char);
                            string_parts.push(second_digit_string.as_string().to_handle());

                            continue;
                        }
                    }

                    // Otherwise treat as a literal
                    let substring = self.template.substring(cx, *start, *end).as_string();
                    string_parts.push(substring);
                }
                SubstitutionPart::NamedCapture(name) => {
                    if let Some(named_captures) = named_captures {
                        let key = PropertyKey::string(cx, *name).to_handle(cx);
                        let captured_string = get(cx, named_captures, key)?;

                        if !captured_string.is_undefined() {
                            let captured_string = to_string(cx, captured_string)?;
                            string_parts.push(captured_string);
                        }
                    }
                }
            }
        }

        Ok(StringValue::concat_all(cx, &string_parts))
    }
}
