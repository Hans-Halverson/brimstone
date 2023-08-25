use crate::{
    js::{
        common::{
            icu::ICU,
            unicode::{is_high_surrogate_code_unit, is_low_surrogate_code_unit, CodePoint},
            wtf_8::Wtf8String,
        },
        parser::regexp::RegExpFlags,
        runtime::{
            abstract_operations::{call_object, get_method, invoke},
            array_object::{array_create, create_array_from_list},
            completion::EvalResult,
            error::{range_error_, type_error_},
            function::get_argument,
            get,
            interned_strings::InternedStrings,
            intrinsics::{
                intrinsics::Intrinsic,
                regexp_constructor::{regexp_create, RegExpSource},
                regexp_prototype::flags_string_contains,
                string_iterator::StringIterator,
            },
            object_value::ObjectValue,
            realm::Realm,
            string_object::StringObject,
            string_value::{CodePointIterator, FlatString, StringValue, StringWidth},
            to_string,
            type_utilities::{
                is_regexp, require_object_coercible, to_integer_or_infinity, to_length, to_number,
                to_uint32,
            },
            value::Value,
            Context, Handle, HeapPtr,
        },
    },
    maybe,
};

use super::regexp_constructor::{FlagsSource, RegExpObject};

pub struct StringPrototype;

impl StringPrototype {
    // 22.1.3 Properties of the String Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let empty_string = cx.names.empty_string().as_string();
        let mut object: Handle<ObjectValue> =
            StringObject::new_with_proto(cx, object_proto, empty_string).into();

        // Constructor property is added once StringConstructor has been created
        object.intrinsic_func(cx, cx.names.at(), Self::at, 1, realm);
        object.intrinsic_func(cx, cx.names.char_at(), Self::char_at, 1, realm);
        object.intrinsic_func(cx, cx.names.char_code_at(), Self::char_code_at, 1, realm);
        object.intrinsic_func(cx, cx.names.code_point_at(), Self::code_point_at, 1, realm);
        object.intrinsic_func(cx, cx.names.concat(), Self::concat, 1, realm);
        object.intrinsic_func(cx, cx.names.ends_with(), Self::ends_with, 1, realm);
        object.intrinsic_func(cx, cx.names.includes(), Self::includes, 1, realm);
        object.intrinsic_func(cx, cx.names.index_of(), Self::index_of, 1, realm);
        object.intrinsic_func(cx, cx.names.last_index_of(), Self::last_index_of, 1, realm);
        object.intrinsic_func(cx, cx.names.match_(), Self::match_, 1, realm);
        object.intrinsic_func(cx, cx.names.match_all(), Self::match_all, 1, realm);
        object.intrinsic_func(cx, cx.names.normalize(), Self::normalize, 0, realm);
        object.intrinsic_func(cx, cx.names.pad_end(), Self::pad_end, 1, realm);
        object.intrinsic_func(cx, cx.names.pad_start(), Self::pad_start, 1, realm);
        object.intrinsic_func(cx, cx.names.repeat(), Self::repeat, 1, realm);
        object.intrinsic_func(cx, cx.names.slice(), Self::slice, 2, realm);
        object.intrinsic_func(cx, cx.names.split(), Self::split, 2, realm);
        object.intrinsic_func(cx, cx.names.starts_with(), Self::starts_with, 1, realm);
        object.intrinsic_func(cx, cx.names.substring(), Self::substring, 2, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(
            cx,
            cx.names.to_locale_lower_case(),
            Self::to_locale_lower_case,
            0,
            realm,
        );
        object.intrinsic_func(
            cx,
            cx.names.to_locale_upper_case(),
            Self::to_locale_upper_case,
            0,
            realm,
        );
        object.intrinsic_func(cx, cx.names.to_lower_case(), Self::to_lower_case, 0, realm);
        object.intrinsic_func(cx, cx.names.to_upper_case(), Self::to_upper_case, 0, realm);
        object.intrinsic_func(cx, cx.names.trim(), Self::trim, 0, realm);
        object.intrinsic_func(cx, cx.names.trim_end(), Self::trim_end, 0, realm);
        object.intrinsic_func(cx, cx.names.trim_start(), Self::trim_start, 0, realm);
        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        // 22.1.3.34 String.prototype [ @@iterator ]
        let iterator_key = cx.well_known_symbols.iterator();
        object.intrinsic_func(cx, iterator_key, Self::iterator, 0, realm);

        object.into()
    }

    // 22.1.3.1 String.prototype.at
    fn at(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let length = string.len() as i64;

        let index_arg = get_argument(cx, arguments, 0);
        let relative_index = maybe!(to_integer_or_infinity(cx, index_arg));
        if relative_index == f64::INFINITY {
            return cx.undefined().into();
        }

        let index = if relative_index >= 0.0 {
            relative_index as i64
        } else {
            length + (relative_index as i64)
        };

        if index < 0 || index >= length {
            return cx.undefined().into();
        }

        FlatString::from_code_unit(cx, string.code_unit_at(index as usize))
            .as_string()
            .into()
    }

    // 22.1.3.2 String.prototype.charAt
    fn char_at(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let position_arg = get_argument(cx, arguments, 0);
        let position = maybe!(to_integer_or_infinity(cx, position_arg));

        if position < 0.0 || position >= string.len() as f64 {
            return cx.names.empty_string().as_string().into();
        }

        FlatString::from_code_unit(cx, string.code_unit_at(position as usize))
            .as_string()
            .into()
    }

    // 22.1.3.3 String.prototype.charCodeAt
    fn char_code_at(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let position_arg = get_argument(cx, arguments, 0);
        let position = maybe!(to_integer_or_infinity(cx, position_arg));

        if position < 0.0 || position >= string.len() as f64 {
            return cx.names.empty_string().as_string().into();
        }

        let code_unit = string.code_unit_at(position as usize);
        Value::smi(code_unit as i32).to_handle(cx).into()
    }

    // 22.1.3.4 String.prototype.codePointAt
    fn code_point_at(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let position_arg = get_argument(cx, arguments, 0);
        let position = maybe!(to_integer_or_infinity(cx, position_arg));

        if position < 0.0 || position >= string.len() as f64 {
            return cx.names.empty_string().as_string().into();
        }

        let code_point = string.code_point_at(position as usize);
        Value::smi(code_point as i32).to_handle(cx).into()
    }

    // 22.1.3.5 String.prototype.concat
    fn concat(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let mut concat_string = maybe!(to_string(cx, object));

        for argument in arguments {
            let string = maybe!(to_string(cx, *argument));
            concat_string = StringValue::concat(cx, concat_string, string);
        }

        concat_string.into()
    }

    // 22.1.3.7 String.prototype.endsWith
    fn ends_with(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));
        let length = string.len();

        let search_value = get_argument(cx, arguments, 0);
        if search_value.is_object() && search_value.as_object().is_regexp_object() {
            return type_error_(cx, "first argument to startsWith cannot be a RegExp");
        }

        let search_string = maybe!(to_string(cx, search_value));

        let end_index_argument = get_argument(cx, arguments, 1);
        let end_index = if end_index_argument.is_undefined() {
            length
        } else {
            let end_index = maybe!(to_integer_or_infinity(cx, end_index_argument));

            if end_index < 0.0 {
                0
            } else if end_index > (length as f64) {
                length
            } else {
                end_index as usize
            }
        };

        let search_length = search_string.len();
        if search_length == 0 {
            return cx.bool(true).into();
        }

        let start_index = match end_index.checked_sub(search_length) {
            Some(start_index) => start_index,
            None => return cx.bool(false).into(),
        };

        let ends_with_string = string.substring_equals(search_string, start_index);

        cx.bool(ends_with_string).into()
    }

    // 22.1.3.8 String.prototype.includes
    fn includes(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let search_string = get_argument(cx, arguments, 0);
        if maybe!(is_regexp(cx, search_string)) {
            return type_error_(cx, "String.prototype.includes cannot take a regular expression");
        }

        let search_string = maybe!(to_string(cx, search_string));

        let pos_arg = get_argument(cx, arguments, 1);
        let pos = maybe!(to_integer_or_infinity(cx, pos_arg));
        if pos == f64::INFINITY {
            return Value::smi(-1).to_handle(cx).into();
        }

        let pos = pos as usize;
        if pos >= string.len() {
            return Value::smi(-1).to_handle(cx).into();
        }

        let found_search_string = string.find(search_string, pos).is_some();
        cx.bool(found_search_string).into()
    }

    // 22.1.3.9 String.prototype.indexOf
    fn index_of(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let search_arg = get_argument(cx, arguments, 0);
        let search_string = maybe!(to_string(cx, search_arg));

        let pos_arg = get_argument(cx, arguments, 1);
        let pos = maybe!(to_integer_or_infinity(cx, pos_arg));
        if pos == f64::INFINITY {
            return Value::smi(-1).to_handle(cx).into();
        }

        let pos = pos as usize;
        if pos >= string.len() {
            return Value::smi(-1).to_handle(cx).into();
        }

        match string.find(search_string, pos) {
            None => Value::smi(-1).to_handle(cx).into(),
            Some(index) => Value::from(index).to_handle(cx).into(),
        }
    }

    // 22.1.3.10 String.prototype.lastIndexOf
    fn last_index_of(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let search_arg = get_argument(cx, arguments, 0);
        let search_string = maybe!(to_string(cx, search_arg));

        let mut string_end = search_string.len();

        let pos_arg = get_argument(cx, arguments, 1);
        let num_pos = maybe!(to_number(cx, pos_arg));

        if !num_pos.is_nan() {
            let pos = maybe!(to_integer_or_infinity(cx, num_pos));
            if pos != f64::INFINITY {
                string_end = usize::clamp(pos as usize, 0, string_end);
            }
        }

        match string.rfind(search_string, string_end) {
            None => Value::smi(-1).to_handle(cx).into(),
            Some(index) => Value::from(index).to_handle(cx).into(),
        }
    }

    // 22.1.3.12 String.prototype.match
    fn match_(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_object = maybe!(require_object_coercible(cx, this_value));

        let regexp_arg = get_argument(cx, arguments, 0);
        if !regexp_arg.is_nullish() {
            let matcher = maybe!(get_method(cx, regexp_arg, cx.well_known_symbols.match_()));
            if let Some(matcher) = matcher {
                return call_object(cx, matcher, regexp_arg, &[this_object.into()]);
            }
        }

        let this_string = maybe!(to_string(cx, this_object));

        let regexp_source = RegExpSource::PatternAndFlags(
            regexp_arg,
            FlagsSource::RegExpFlags(RegExpFlags::empty()),
        );
        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let regexp_object = maybe!(regexp_create(cx, regexp_source, regexp_constructor));

        invoke(cx, regexp_object, cx.well_known_symbols.match_(), &[this_string.into()])
    }

    // 22.1.3.13 String.prototype.matchAll
    fn match_all(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_object = maybe!(require_object_coercible(cx, this_value));

        let regexp_arg = get_argument(cx, arguments, 0);
        if !regexp_arg.is_nullish() {
            if maybe!(is_regexp(cx, regexp_arg)) {
                let regexp_object = regexp_arg.as_object();
                let has_global_flag = if regexp_object.is_regexp_object() {
                    regexp_object
                        .cast::<RegExpObject>()
                        .flags()
                        .contains(RegExpFlags::GLOBAL)
                } else {
                    let flags_string = maybe!(get(cx, regexp_object, cx.names.flags()));
                    maybe!(require_object_coercible(cx, flags_string));
                    let flags_string = maybe!(to_string(cx, flags_string));

                    flags_string_contains(flags_string, 'g' as u32)
                };

                if !has_global_flag {
                    return type_error_(
                        cx,
                        "String.prototype.matchAll expects RegExp with global flag",
                    );
                }
            }

            let matcher = maybe!(get_method(cx, regexp_arg, cx.well_known_symbols.match_all()));
            if let Some(matcher) = matcher {
                return call_object(cx, matcher, regexp_arg, &[this_object.into()]);
            }
        }

        let this_string = maybe!(to_string(cx, this_object));

        let regexp_source = RegExpSource::PatternAndFlags(
            regexp_arg,
            FlagsSource::RegExpFlags(RegExpFlags::GLOBAL),
        );
        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let regexp_object = maybe!(regexp_create(cx, regexp_source, regexp_constructor));

        invoke(cx, regexp_object, cx.well_known_symbols.match_all(), &[this_string.into()])
    }

    // 22.1.3.14 String.prototype.normalize
    fn normalize(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let form_arg = get_argument(cx, arguments, 0);
        let form = if form_arg.is_undefined() {
            NormalizationForm::NFC
        } else {
            let form_string = maybe!(to_string(cx, form_arg)).flatten().get_();
            if form_string == cx.names.nfc.as_string().as_flat() {
                NormalizationForm::NFC
            } else if form_string == cx.names.nfd.as_string().as_flat() {
                NormalizationForm::NFD
            } else if form_string == cx.names.nfkc.as_string().as_flat() {
                NormalizationForm::NFKC
            } else if form_string == cx.names.nfkd.as_string().as_flat() {
                NormalizationForm::NFKD
            } else {
                return range_error_(
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

        normalized_string.into()
    }

    // 22.1.3.15 String.prototype.padEnd
    fn pad_end(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let max_length_arg = get_argument(cx, arguments, 0);
        let fill_string_arg = get_argument(cx, arguments, 1);

        Self::pad_string(cx, this_value, max_length_arg, fill_string_arg, false)
    }

    // 22.1.3.16 String.prototype.padStart
    fn pad_start(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let max_length_arg = get_argument(cx, arguments, 0);
        let fill_string_arg = get_argument(cx, arguments, 1);

        Self::pad_string(cx, this_value, max_length_arg, fill_string_arg, true)
    }

    fn pad_string(
        cx: &mut Context,
        this_value: Handle<Value>,
        max_length_arg: Handle<Value>,
        fill_string_arg: Handle<Value>,
        is_start: bool,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let int_max_length = maybe!(to_length(cx, max_length_arg));
        let string_length = string.len() as u64;

        // No need to pad as string already has max length
        if int_max_length <= string_length {
            return string.into();
        }

        let fill_string = if fill_string_arg.is_undefined() {
            InternedStrings::get_str(cx, " ")
        } else {
            maybe!(to_string(cx, fill_string_arg))
        };

        let fill_length = int_max_length - string_length;
        let fill_string_length = fill_string.len() as u64;

        // Check for an empty padding string which would have no effect
        if fill_string_length == 0 {
            return string.into();
        }

        // Find the number of whole pad strings we can fit into the padding length
        let num_whole_repitions = fill_length / fill_string_length;
        let mut pad_string = fill_string.repeat(cx, num_whole_repitions).as_string();

        // Add a partial pad string if the pad strings did not evenly divide the padding length
        let partial_length = fill_length - (num_whole_repitions * fill_string_length);
        if partial_length != 0 {
            let partial_string = fill_string
                .substring(cx, 0, partial_length as usize)
                .as_string();
            pad_string = StringValue::concat(cx, pad_string, partial_string);
        }

        if is_start {
            StringValue::concat(cx, pad_string, string).into()
        } else {
            StringValue::concat(cx, string, pad_string).into()
        }
    }

    // 22.1.3.17 String.prototype.repeat
    fn repeat(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let n_arg = get_argument(cx, arguments, 0);
        let n = maybe!(to_integer_or_infinity(cx, n_arg));
        if n < 0.0 || n == f64::INFINITY {
            return range_error_(cx, "count must be a finite, positive number");
        } else if n == 0.0 {
            return cx.names.empty_string().as_string().into();
        }

        string.repeat(cx, n as u64).as_string().into()
    }

    // 22.1.3.21 String.prototype.slice
    fn slice(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));
        let length = string.len();

        let start_arg = get_argument(cx, arguments, 0);
        let relative_start = maybe!(to_integer_or_infinity(cx, start_arg));
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length as u64)
        };

        let end_argument = get_argument(cx, arguments, 1);
        let end_index = if !end_argument.is_undefined() {
            let relative_end = maybe!(to_integer_or_infinity(cx, end_argument));

            if relative_end < 0.0 {
                if relative_end == f64::NEG_INFINITY {
                    0
                } else {
                    i64::max(length as i64 + relative_end as i64, 0) as u64
                }
            } else {
                u64::min(relative_end as u64, length as u64)
            }
        } else {
            length as u64
        };

        if start_index >= end_index {
            return cx.names.empty_string().as_string().into();
        }

        let substring = string
            .substring(cx, start_index as usize, end_index as usize)
            .as_string();

        substring.into()
    }

    // 22.1.3.22 String.prototype.split
    fn split(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));

        let separator_argument = get_argument(cx, arguments, 0);
        let limit_argument = get_argument(cx, arguments, 1);

        // Use the @@split method of the separator if one exists
        if !separator_argument.is_nullish() {
            let split_key = cx.well_known_symbols.split();
            let splitter = maybe!(get_method(cx, separator_argument, split_key));

            if let Some(splitter) = splitter {
                return call_object(cx, splitter, separator_argument, &[object, limit_argument]);
            }
        }

        let string = maybe!(to_string(cx, object));

        // Limit defaults to 2^32 - 1
        let limit = if limit_argument.is_undefined() {
            u32::MAX
        } else {
            maybe!(to_uint32(cx, limit_argument))
        };

        let separator = maybe!(to_string(cx, separator_argument));

        if limit == 0 {
            let array_object = maybe!(array_create(cx, 0, None));
            return array_object.into();
        } else if separator_argument.is_undefined() {
            return create_array_from_list(cx, &[string.into()]).into();
        }

        // If separator is empty then return each code unit individually, up to the given limit
        let separator_length = separator.len();
        if separator_length == 0 {
            let mut code_unit_strings = vec![];
            let limit = usize::min(limit as usize, string.len());

            for i in 0..limit {
                let substring = string.substring(cx, i, i + 1);
                code_unit_strings.push(substring.into());
            }

            return create_array_from_list(cx, &code_unit_strings).into();
        }

        // If the string is empty then it is the only substring
        let string_length = string.len();
        if string_length == 0 {
            return create_array_from_list(cx, &[string.into()]).into();
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
                return create_array_from_list(cx, &substrings).into();
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

        create_array_from_list(cx, &substrings).into()
    }

    // 22.1.3.23 String.prototype.startsWith
    fn starts_with(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));
        let length = string.len();

        let search_value = get_argument(cx, arguments, 0);
        if search_value.is_object() && search_value.as_object().is_regexp_object() {
            return type_error_(cx, "first argument to startsWith cannot be a RegExp");
        }

        let search_string = maybe!(to_string(cx, search_value));

        let start_index_argument = get_argument(cx, arguments, 1);
        let start_index = if start_index_argument.is_undefined() {
            0
        } else {
            let start_index = maybe!(to_integer_or_infinity(cx, start_index_argument));

            if start_index < 0.0 {
                0
            } else if start_index > (length as f64) {
                length
            } else {
                start_index as usize
            }
        };

        let search_length = search_string.len();
        if search_length == 0 {
            return cx.bool(true).into();
        }

        let end_index = start_index + search_length;
        if end_index > length {
            return cx.bool(false).into();
        }

        let starts_with_string = string.substring_equals(search_string, start_index);

        cx.bool(starts_with_string).into()
    }

    // 22.1.3.24 String.prototype.substring
    fn substring(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));
        let length = string.len();

        let start_arg = get_argument(cx, arguments, 0);
        let start = maybe!(to_integer_or_infinity(cx, start_arg));
        let mut int_start = f64::max(0.0, f64::min(start, length as f64)) as usize;

        let end_argument = get_argument(cx, arguments, 1);
        let mut int_end = if end_argument.is_undefined() {
            length as usize
        } else {
            let end = maybe!(to_integer_or_infinity(cx, end_argument));
            f64::max(0.0, f64::min(end, length as f64)) as usize
        };

        if int_end < int_start {
            std::mem::swap(&mut int_start, &mut int_end);
        }

        let substring = string.substring(cx, int_start, int_end).as_string();

        substring.into()
    }

    // 22.1.3.25 String.prototype.toLocaleLowerCase
    fn to_locale_lower_case(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        string.to_lower_case(cx).as_string().into()
    }

    // 22.1.3.26 String.prototype.toLocaleUpperCase
    fn to_locale_upper_case(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        string.to_upper_case(cx).as_string().into()
    }

    // 22.1.3.27 String.prototype.toLowerCase
    fn to_lower_case(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        string.to_lower_case(cx).as_string().into()
    }

    // 22.1.3.28 String.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_string_value(cx, this_value)
    }

    // 22.1.3.29 String.prototype.toUpperCase
    fn to_upper_case(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        string.to_upper_case(cx).as_string().into()
    }

    // 22.1.3.30 String.prototype.trim
    fn trim(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        string.trim(cx, true, true).into()
    }

    // 22.1.3.31 String.prototype.trimEnd
    fn trim_end(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        string.trim(cx, false, true).into()
    }

    // 22.1.3.32 String.prototype.trimStart
    fn trim_start(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        string.trim(cx, true, false).into()
    }

    // 22.1.3.33 String.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_string_value(cx, this_value)
    }

    // 22.1.3.34 String.prototype [ @@iterator ]
    fn iterator(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let flat_string = string.flatten();

        StringIterator::new(cx, flat_string).into()
    }
}

fn this_string_value(cx: &mut Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    if value.is_string() {
        return value.into();
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_string_object() {
            return object_value.cast::<StringObject>().string_data().into();
        }
    }

    type_error_(cx, "value cannot be converted to string")
}

enum NormalizationForm {
    NFC,
    NFD,
    NFKC,
    NFKD,
}

enum StringPart {
    // A valid range of code points between the two indices [start, end)
    ValidRange(usize, usize),
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
                    parts.push(StringPart::ValidRange(start, i));
                }

                parts.push(StringPart::UnpairedSurrogate(code_unit));

                i += 1;
                start = i;
            }

            // Flush the final valid range if one exists
            if i != start {
                parts.push(StringPart::ValidRange(start, code_units.len()));
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
    cx: &mut Context,
    string: Handle<StringValue>,
    f: impl Fn(CharIterator) -> I,
) -> Handle<StringValue> {
    let parts = to_valid_string_parts(string.flatten().get_());

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
