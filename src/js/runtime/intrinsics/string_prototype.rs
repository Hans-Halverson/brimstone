use crate::{
    js::runtime::{
        abstract_operations::{call_object, get_method, invoke},
        array_object::{array_create, create_array_from_list},
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        intrinsics::{intrinsics::Intrinsic, string_iterator::StringIterator},
        object_value::ObjectValue,
        realm::Realm,
        string_object::StringObject,
        string_value::{FlatString, StringValue},
        to_string,
        type_utilities::{
            is_regexp, require_object_coercible, to_integer_or_infinity, to_number, to_uint32,
        },
        value::Value,
        Context, Handle,
    },
    maybe,
};

use super::regexp_constructor::{regexp_create, FlagsSource, PatternSourceValue};

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

        let pattern_source = PatternSourceValue::Value(regexp_arg);
        let flags_source = FlagsSource::Value(cx.undefined());

        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let regexp_object =
            maybe!(regexp_create(cx, pattern_source, flags_source, regexp_constructor));

        invoke(cx, regexp_object, cx.well_known_symbols.match_(), &[this_string.into()])
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

            for code_unit in string.iter_slice_code_units(0, limit) {
                let code_unit_string = FlatString::from_code_unit(cx, code_unit).as_string();
                code_unit_strings.push(code_unit_string.into());
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

        StringIterator::new(cx, string).into()
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
