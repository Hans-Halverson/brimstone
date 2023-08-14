use crate::{
    js::{
        common::unicode::{needs_surrogate_pair, CodePoint},
        parser::regexp::RegExpFlags,
        runtime::{
            abstract_operations::{
                call, construct, create_data_property_or_throw, set, species_constructor,
            },
            array_object::{array_create, create_array_from_list},
            completion::EvalResult,
            error::type_error_,
            function::get_argument,
            get,
            interned_strings::InternedStrings,
            intrinsics::regexp_string_iterator::RegExpStringIterator,
            object_descriptor::ObjectKind,
            object_value::ObjectValue,
            ordinary_object::object_create_with_optional_proto,
            realm::Realm,
            regexp::matcher::run_matcher,
            string_value::StringValue,
            to_string,
            type_utilities::{is_callable, same_object_value, to_boolean, to_length},
            Context, Handle, PropertyKey, Value,
        },
    },
    maybe, must,
};

use super::{intrinsics::Intrinsic, regexp_constructor::RegExpObject};

pub struct RegExpPrototype;

impl RegExpPrototype {
    // 22.2.6 Properties of the RegExp Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once RegExpConstructor has been created
        object.intrinsic_func(cx, cx.names.exec(), Self::exec, 1, realm);
        object.intrinsic_getter(cx, cx.names.dot_all(), Self::dot_all, realm);
        object.intrinsic_getter(cx, cx.names.flags(), Self::flags, realm);
        object.intrinsic_getter(cx, cx.names.global(), Self::global, realm);
        object.intrinsic_getter(cx, cx.names.has_indices(), Self::has_indices, realm);
        object.intrinsic_getter(cx, cx.names.ignore_case(), Self::ignore_case, realm);
        object.intrinsic_func(cx, cx.well_known_symbols.match_(), Self::match_, 1, realm);
        object.intrinsic_func(cx, cx.well_known_symbols.match_all(), Self::match_all, 1, realm);
        object.intrinsic_getter(cx, cx.names.multiline(), Self::multiline, realm);
        object.intrinsic_getter(cx, cx.names.source(), Self::source, realm);
        object.intrinsic_getter(cx, cx.names.sticky(), Self::sticky, realm);
        object.intrinsic_func(cx, cx.names.test(), Self::test, 1, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 1, realm);
        object.intrinsic_getter(cx, cx.names.unicode(), Self::unicode, realm);

        object
    }

    // 22.2.6.2 RegExp.prototype.exec
    fn exec(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let regexp_object = if this_value.is_object() && this_value.as_object().is_regexp_object() {
            this_value.as_object().cast::<RegExpObject>()
        } else {
            return type_error_(
                cx,
                "RegExpr.prototype.exec must be called on a regular expression",
            );
        };

        let string_arg = get_argument(cx, arguments, 0);
        let string_value = maybe!(to_string(cx, string_arg));

        regexp_builtin_exec(cx, regexp_object, string_value)
    }

    // 22.2.6.3 get RegExp.prototype.dotAll
    fn dot_all(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::DOT_ALL)
    }

    // 22.2.6.4 get RegExp.prototype.flags
    fn flags(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "Expected a regular expression");
        }

        let this_object = this_value.as_object();

        // When this value is a regexp object, use cached flags string
        let is_regexp_object = this_object.is_regexp_object();
        if is_regexp_object {
            let regexp_object = this_object.cast::<RegExpObject>();
            if let Some(flags_string) = regexp_object.flags_string() {
                return flags_string.into();
            }
        }

        let mut flags_string = String::new();

        let has_indices_value = maybe!(get(cx, this_object, cx.names.has_indices()));
        if to_boolean(has_indices_value.get()) {
            flags_string.push('d');
        }

        let global_value = maybe!(get(cx, this_object, cx.names.global()));
        if to_boolean(global_value.get()) {
            flags_string.push('g');
        }

        let ignore_case_value = maybe!(get(cx, this_object, cx.names.ignore_case()));
        if to_boolean(ignore_case_value.get()) {
            flags_string.push('i');
        }

        let multiline_value = maybe!(get(cx, this_object, cx.names.multiline()));
        if to_boolean(multiline_value.get()) {
            flags_string.push('m');
        }

        let dot_all_value = maybe!(get(cx, this_object, cx.names.dot_all()));
        if to_boolean(dot_all_value.get()) {
            flags_string.push('s');
        }

        let unicode_value = maybe!(get(cx, this_object, cx.names.unicode()));
        if to_boolean(unicode_value.get()) {
            flags_string.push('u');
        }

        let sticky_value = maybe!(get(cx, this_object, cx.names.sticky()));
        if to_boolean(sticky_value.get()) {
            flags_string.push('y');
        }

        let flags_string = if flags_string.is_empty() {
            cx.names.empty_string().as_string()
        } else {
            cx.alloc_string(&flags_string)
        };

        // Cache flags string for regexp objects
        if is_regexp_object {
            let mut regexp_object = this_object.cast::<RegExpObject>();
            regexp_object.set_flags_string(flags_string);
        }

        flags_string.into()
    }

    // 22.2.6.5 get RegExp.prototype.global
    fn global(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::GLOBAL)
    }

    // 22.2.6.6 get RegExp.prototype.hasIndices
    fn has_indices(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::HAS_INDICES)
    }

    // 22.2.6.7 get RegExp.prototype.ignoreCase
    fn ignore_case(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::IGNORE_CASE)
    }

    // 22.2.6.8 RegExp.prototype [ @@match ]
    fn match_(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "RegExpr.prototype[@@match] must be called on object");
        }

        let regexp_object = this_value.as_object();

        let string_arg = get_argument(cx, arguments, 0);
        let string_value = maybe!(to_string(cx, string_arg));

        let (is_global, is_unicode) = if regexp_object.is_regexp_object() {
            let flags = regexp_object.cast::<RegExpObject>().flags();
            (flags.contains(RegExpFlags::GLOBAL), flags.contains(RegExpFlags::UNICODE_AWARE))
        } else {
            let flags_string = maybe!(get(cx, regexp_object, cx.names.flags()));
            let flags_string = maybe!(to_string(cx, flags_string));

            let is_global = flags_string_contains(flags_string, 'g' as u32);
            let is_unicode = flags_string_contains(flags_string, 'u' as u32);

            (is_global, is_unicode)
        };

        if !is_global {
            return regexp_exec(cx, regexp_object, string_value);
        }

        let zero_value = Value::from(0).to_handle(cx);
        maybe!(set(cx, regexp_object, cx.names.last_index(), zero_value, true));

        let result_array = maybe!(array_create(cx, 0, None));
        let mut n = 0;

        loop {
            let result = maybe!(regexp_exec(cx, regexp_object, string_value));
            if result.is_null() {
                if n == 0 {
                    return cx.null().into();
                } else {
                    return result_array.into();
                }
            }

            // RegExpExec only returns an object or null
            debug_assert!(result.is_object());
            let result_object = result.as_object();

            let zero_key = PropertyKey::array_index(cx, 0).to_handle(cx);
            let match_string = maybe!(get(cx, result_object, zero_key));
            let match_string = maybe!(to_string(cx, match_string));

            let n_key = PropertyKey::array_index(cx, n).to_handle(cx);
            must!(create_data_property_or_throw(
                cx,
                result_array.into(),
                n_key,
                match_string.into()
            ));

            if match_string.len() == 0 {
                let last_index = maybe!(get(cx, regexp_object, cx.names.last_index()));
                let last_index = maybe!(to_length(cx, last_index));

                let next_index = advance_string_index(string_value, last_index, is_unicode);
                let next_index_value = Value::from(next_index).to_handle(cx);
                maybe!(set(cx, regexp_object, cx.names.last_index(), next_index_value, true));
            }

            n += 1;
        }
    }

    // 22.2.6.9 RegExp.prototype [ @@matchAll ]
    fn match_all(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "RegExpr.prototype[@@matchAll] must be called on object");
        }

        let regexp_object = this_value.as_object();

        let string_arg = get_argument(cx, arguments, 0);
        let string_value = maybe!(to_string(cx, string_arg));

        let constructor =
            maybe!(species_constructor(cx, regexp_object, Intrinsic::RegExpConstructor));

        let flags_string = maybe!(get(cx, regexp_object, cx.names.flags()));
        let flags_string = maybe!(to_string(cx, flags_string));

        let matcher =
            maybe!(construct(cx, constructor, &[regexp_object.into(), flags_string.into()], None));

        let last_index = maybe!(get(cx, regexp_object, cx.names.last_index()));
        let last_index = maybe!(to_length(cx, last_index));
        let last_index_value = Value::from(last_index).to_handle(cx);

        maybe!(set(cx, matcher, cx.names.last_index(), last_index_value, true));

        let is_global = flags_string_contains(flags_string, 'g' as u32);
        let is_unicode = flags_string_contains(flags_string, 'u' as u32);

        RegExpStringIterator::new(cx, regexp_object, string_value, is_global, is_unicode).into()
    }

    // 22.2.6.10 get RegExp.prototype.multiline
    fn multiline(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::MULTILINE)
    }

    // 22.2.6.13 get RegExp.prototype.source
    fn source(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if this_value.is_object() {
            let this_object = this_value.as_object();
            if this_object.is_regexp_object() {
                return this_object
                    .cast::<RegExpObject>()
                    .escaped_pattern_source()
                    .into();
            } else if same_object_value(
                this_object.get_(),
                cx.get_intrinsic_ptr(Intrinsic::RegExpPrototype),
            ) {
                return cx.alloc_string("(?:)").into();
            }
        }

        type_error_(cx, "Expected a regular expression")
    }

    // 22.2.6.15 get RegExp.prototype.sticky
    fn sticky(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::STICKY)
    }

    // 22.2.6.16 RegExp.prototype.test
    fn test(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let regexp_object = if this_value.is_object() {
            this_value.as_object()
        } else {
            return type_error_(cx, "RegExpr.prototype.test must be called on an object");
        };

        let string_arg = get_argument(cx, arguments, 0);
        let string_value = maybe!(to_string(cx, string_arg));

        let exec_result = maybe!(regexp_exec(cx, regexp_object, string_value));

        cx.bool(!exec_result.is_null()).into()
    }

    // 22.2.6.17 RegExp.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "Expected a regular expression");
        }

        let this_object = this_value.as_object();

        let pattern_value = maybe!(get(cx, this_object, cx.names.source()));
        let pattern_string = maybe!(to_string(cx, pattern_value));

        let flags_value = maybe!(get(cx, this_object, cx.names.flags()));
        let flags_string = maybe!(to_string(cx, flags_value));

        let slash_string = InternedStrings::get_str(cx, "/");

        StringValue::concat_all(cx, &[slash_string, pattern_string, slash_string, flags_string])
            .into()
    }

    // 22.2.6.18 get RegExp.prototype.unicode
    fn unicode(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::UNICODE_AWARE)
    }
}

// 22.2.6.4.1 RegExpHasFlag
fn regexp_has_flag(
    cx: &mut Context,
    this_value: Handle<Value>,
    flag: RegExpFlags,
) -> EvalResult<Handle<Value>> {
    if this_value.is_object() {
        let this_object = this_value.as_object();
        if this_object.is_regexp_object() {
            let has_flag = this_object.cast::<RegExpObject>().flags().contains(flag);
            return cx.bool(has_flag).into();
        } else if same_object_value(
            this_object.get_(),
            cx.get_intrinsic_ptr(Intrinsic::RegExpPrototype),
        ) {
            return cx.undefined().into();
        }
    }

    type_error_(cx, "Expected a regular expression")
}

pub fn flags_string_contains(flags_string: Handle<StringValue>, flag: CodePoint) -> bool {
    flags_string.iter_code_points().any(|c| c == flag)
}

// 22.2.7.1 RegExpExec
pub fn regexp_exec(
    cx: &mut Context,
    regexp_object: Handle<ObjectValue>,
    string_value: Handle<StringValue>,
) -> EvalResult<Handle<Value>> {
    let exec = maybe!(get(cx, regexp_object, cx.names.exec()));

    if is_callable(exec) {
        let exec_result = maybe!(call(cx, exec, regexp_object.into(), &[string_value.into()]));
        if !exec_result.is_null() && !exec_result.is_object() {
            return type_error_(cx, "Regular expression exec must return null or an object");
        }

        return exec_result.into();
    }

    if !regexp_object.is_regexp_object() {
        return type_error_(cx, "Expected a regular expression");
    }

    regexp_builtin_exec(cx, regexp_object.cast::<RegExpObject>(), string_value)
}

// 22.2.7.2 RegExpBuiltinExec
fn regexp_builtin_exec(
    cx: &mut Context,
    regexp_object: Handle<RegExpObject>,
    string_value: Handle<StringValue>,
) -> EvalResult<Handle<Value>> {
    let compiled_regexp = regexp_object.compiled_regexp();
    let string_length = string_value.len();

    let last_index = maybe!(get(cx, regexp_object.into(), cx.names.last_index()));
    let mut last_index = maybe!(to_length(cx, last_index)) as usize;

    let flags = regexp_object.flags();
    let is_global = flags.contains(RegExpFlags::GLOBAL);
    let is_sticky = flags.contains(RegExpFlags::STICKY);
    let has_indices = flags.contains(RegExpFlags::HAS_INDICES);

    if !is_global && !is_sticky {
        last_index = 0;
    }

    // Check if last index is already out of range meaning the match will always fail, resetting
    // last index under certain flags.
    if last_index > string_length {
        if is_global || is_sticky {
            let zero_value = Value::from(0).to_handle(cx);
            maybe!(set(cx, regexp_object.into(), cx.names.last_index(), zero_value, true));
        }

        return cx.null().into();
    }

    // Run the matching engine on the regexp and input string
    let match_ = run_matcher(compiled_regexp.get_(), string_value, last_index);

    // Handle match failure, resetting last index under sticy flag
    if match_.is_none() {
        if is_sticky {
            let zero_value = Value::from(0).to_handle(cx);
            maybe!(set(cx, regexp_object.into(), cx.names.last_index(), zero_value, true));
        }

        return cx.null().into();
    }

    let capture_groups = &match_.unwrap().capture_groups;

    // 0'th capture which matches entire pattern is guaranteed to exist
    let full_capture = capture_groups[0].as_ref().unwrap();

    // Update last index to point past end of capture
    if is_global || is_sticky {
        let last_index_value = Value::from(full_capture.end).to_handle(cx);
        maybe!(set(cx, regexp_object.into(), cx.names.last_index(), last_index_value, true));
    }

    // Build result array of matches
    let result_array: Handle<ObjectValue> =
        must!(array_create(cx, capture_groups.len() as u64, None)).into();

    // Mark the start of the full match
    let index_value = Value::from(full_capture.start).to_handle(cx);
    must!(create_data_property_or_throw(cx, result_array, cx.names.index(), index_value));

    // Include the input string in the result
    must!(create_data_property_or_throw(
        cx,
        result_array,
        cx.names.input(),
        string_value.into()
    ));

    // Add the groups object to the result, or undefined if there are no named capture groups
    let named_groups_object = if compiled_regexp.has_named_capture_groups {
        object_create_with_optional_proto::<ObjectValue>(cx, ObjectKind::OrdinaryObject, None)
            .to_handle()
            .into()
    } else {
        cx.undefined()
    };
    must!(create_data_property_or_throw(
        cx,
        result_array,
        cx.names.groups(),
        named_groups_object
    ));

    // Set up indices array to collect capture group indices, if flag is set
    let indices_result = if has_indices {
        let indices_array: Handle<ObjectValue> =
            must!(array_create(cx, capture_groups.len() as u64, None)).into();

        // Indices array contains named capture groups object if there are any named groups
        let named_groups_object = if compiled_regexp.has_named_capture_groups {
            object_create_with_optional_proto::<ObjectValue>(cx, ObjectKind::OrdinaryObject, None)
                .to_handle()
                .into()
        } else {
            cx.undefined()
        };
        must!(create_data_property_or_throw(
            cx,
            indices_array,
            cx.names.groups(),
            named_groups_object
        ));

        Some((indices_array, named_groups_object))
    } else {
        None
    };

    // Add all capture groups to the result, including implicit 0'th capture group
    for (i, capture) in capture_groups.iter().enumerate() {
        let captured_value = if let Some(capture) = capture {
            string_value
                .substring(cx, capture.start, capture.end)
                .as_string()
                .into()
        } else {
            cx.undefined()
        };

        let index_key = PropertyKey::from_u64(cx, i as u64).to_handle(cx);
        must!(create_data_property_or_throw(cx, result_array, index_key, captured_value));

        // Add capture indices to indices array if present
        let match_index_pair = if let Some((indices_array, indices_groups)) = indices_result {
            let match_index_pair = if let Some(capture) = capture {
                let start_index = Value::from(capture.start).to_handle(cx);
                let end_index = Value::from(capture.end).to_handle(cx);
                create_array_from_list(cx, &[start_index, end_index]).into()
            } else {
                cx.undefined()
            };

            must!(create_data_property_or_throw(cx, indices_array, index_key, match_index_pair));
            Some((match_index_pair, indices_groups))
        } else {
            None
        };

        // Add group name to groups object if group was named
        if i != 0 {
            if let Some(group_name) = compiled_regexp.capture_groups_as_slice()[i - 1] {
                let group_name = group_name.to_handle();
                let group_name_key = PropertyKey::string(cx, group_name).to_handle(cx);

                // Group names object is guaranteed to be an object value
                let groups = named_groups_object.as_object();

                must!(create_data_property_or_throw(cx, groups, group_name_key, captured_value));

                // Add capture indices to the group names object in the indices array if necessary
                if let Some((match_index_pair, indices_groups)) = match_index_pair {
                    // Group names object is guaranteed to be an object value
                    let groups = indices_groups.as_object();

                    must!(create_data_property_or_throw(
                        cx,
                        groups,
                        group_name_key,
                        match_index_pair
                    ));
                }
            }
        }
    }

    // Add indices to result if necessary
    if let Some((indices_array, _)) = indices_result {
        must!(create_data_property_or_throw(
            cx,
            result_array,
            cx.names.indices(),
            indices_array.into()
        ));
    }

    result_array.into()
}

// 22.2.7.3 AdvanceStringIndex
// Increments the index by one if not unicode-aware, and by the size of the current code point if unicode-aware.
pub fn advance_string_index(
    string_value: Handle<StringValue>,
    prev_index: u64,
    is_unicode: bool,
) -> u64 {
    if !is_unicode {
        return prev_index + 1;
    }

    let num_code_units = if needs_surrogate_pair(string_value.code_point_at(prev_index as usize)) {
        2
    } else {
        1
    };

    prev_index + num_code_units
}
