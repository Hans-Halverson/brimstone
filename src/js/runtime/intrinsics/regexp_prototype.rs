use crate::{
    js::{
        common::unicode::{needs_surrogate_pair, CodePoint},
        parser::regexp::RegExpFlags,
        runtime::{
            abstract_operations::{
                call, call_object, construct, create_data_property_or_throw, length_of_array_like,
                set, species_constructor,
            },
            array_object::{array_create, create_array_from_list},
            completion::EvalResult,
            error::type_error_,
            function::get_argument,
            get,
            interned_strings::InternedStrings,
            intrinsics::{
                regexp_string_iterator::RegExpStringIterator,
                string_prototype::SubstitutionTemplateParser,
            },
            object_descriptor::ObjectKind,
            object_value::ObjectValue,
            ordinary_object::object_create_with_optional_proto,
            realm::Realm,
            regexp::matcher::run_matcher,
            string_value::StringValue,
            to_string,
            type_utilities::{
                is_callable, same_object_value, same_value, to_boolean, to_integer_or_infinity,
                to_length, to_object, to_uint32,
            },
            Context, Handle, PropertyKey, Value,
        },
    },
    maybe, must,
};

use super::{
    intrinsics::Intrinsic, regexp_constructor::RegExpObject, string_prototype::ReplaceValue,
};

pub struct RegExpPrototype;

impl RegExpPrototype {
    // 22.2.6 Properties of the RegExp Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
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
        object.intrinsic_func(cx, cx.well_known_symbols.replace(), Self::replace, 2, realm);
        object.intrinsic_func(cx, cx.well_known_symbols.search(), Self::search, 1, realm);
        object.intrinsic_getter(cx, cx.names.source(), Self::source, realm);
        object.intrinsic_func(cx, cx.well_known_symbols.split(), Self::split, 2, realm);
        object.intrinsic_getter(cx, cx.names.sticky(), Self::sticky, realm);
        object.intrinsic_func(cx, cx.names.test(), Self::test, 1, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_getter(cx, cx.names.unicode(), Self::unicode, realm);
        object.intrinsic_getter(cx, cx.names.unicode_sets(), Self::unicode_sets, realm);

        object
    }

    // 22.2.6.2 RegExp.prototype.exec
    pub fn exec(
        cx: Context,
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
    pub fn dot_all(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::DOT_ALL)
    }

    // 22.2.6.4 get RegExp.prototype.flags
    pub fn flags(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "Expected a regular expression");
        }

        let this_object = this_value.as_object();

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

        let unicode_sets_value = maybe!(get(cx, this_object, cx.names.unicode_sets()));
        if to_boolean(unicode_sets_value.get()) {
            flags_string.push('v');
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

        flags_string.into()
    }

    // 22.2.6.5 get RegExp.prototype.global
    pub fn global(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::GLOBAL)
    }

    // 22.2.6.6 get RegExp.prototype.hasIndices
    pub fn has_indices(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::HAS_INDICES)
    }

    // 22.2.6.7 get RegExp.prototype.ignoreCase
    pub fn ignore_case(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::IGNORE_CASE)
    }

    // 22.2.6.8 RegExp.prototype [ @@match ]
    pub fn match_(
        cx: Context,
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

        let flags_string = maybe!(get(cx, regexp_object, cx.names.flags()));
        let flags_string = maybe!(to_string(cx, flags_string));

        let is_global = flags_string_contains(flags_string, 'g' as u32);
        let is_unicode = flags_string_contains(flags_string, 'u' as u32);

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

                let next_index = advance_u64_string_index(string_value, last_index, is_unicode);
                let next_index_value = Value::from(next_index).to_handle(cx);
                maybe!(set(cx, regexp_object, cx.names.last_index(), next_index_value, true));
            }

            n += 1;
        }
    }

    // 22.2.6.9 RegExp.prototype [ @@matchAll ]
    pub fn match_all(
        cx: Context,
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

        RegExpStringIterator::new(cx, matcher, string_value, is_global, is_unicode).into()
    }

    // 22.2.6.10 get RegExp.prototype.multiline
    pub fn multiline(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::MULTILINE)
    }

    // 22.2.6.11 RegExp.prototype [ @@replace ]
    pub fn replace(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "RegExpr.prototype[@@replace] must be called on object");
        }

        let regexp_object = this_value.as_object();

        let target_string_arg = get_argument(cx, arguments, 0);
        let target_string = maybe!(to_string(cx, target_string_arg));

        let replace_arg = get_argument(cx, arguments, 1);
        let replace_value = if is_callable(replace_arg) {
            ReplaceValue::Function(replace_arg.as_object())
        } else {
            ReplaceValue::String(maybe!(to_string(cx, replace_arg)))
        };

        // Get flags string, determining if RegExp is unicode or global
        let flags_value = maybe!(get(cx, regexp_object, cx.names.flags()));
        let flags_string = maybe!(to_string(cx, flags_value));

        let mut is_unicode = false;
        let mut is_global = false;
        for code_point in flags_string.iter_code_points() {
            if code_point == 'u' as u32 || code_point == 'v' as u32 {
                is_unicode = true;
            } else if code_point == 'g' as u32 {
                is_global = true;
            }
        }

        if is_global {
            let zero_value = Value::from(0).to_handle(cx);
            maybe!(set(cx, regexp_object, cx.names.last_index(), zero_value, true));
        }

        // Key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        let mut exec_results = vec![];

        loop {
            // Search target string, finding all matches if global
            let exec_result = maybe!(regexp_exec(cx, regexp_object, target_string.into()));
            if exec_result.is_null() {
                break;
            }

            let exec_result = exec_result.as_object();

            if !is_global {
                exec_results.push(exec_result);
                break;
            }

            // Extract matched string
            key.replace(PropertyKey::array_index(cx, 0));
            let matched_value = maybe!(get(cx, exec_result, key));
            let matched_string = maybe!(to_string(cx, matched_value));

            exec_results.push(exec_result);

            // If matched string is empty then increment last index
            if matched_string.is_empty() {
                let this_index = maybe!(get(cx, regexp_object, cx.names.last_index()));
                let this_index = maybe!(to_length(cx, this_index));

                let next_index = advance_u64_string_index(target_string, this_index, is_unicode);
                let next_index_value = Value::from(next_index).to_handle(cx);
                maybe!(set(cx, regexp_object, cx.names.last_index(), next_index_value, true));
            }
        }

        let mut string_parts = vec![];
        let mut next_source_position = 0;

        // Cached substitution template
        let mut substitution_template = None;

        for exec_result in exec_results {
            let result_length = maybe!(length_of_array_like(cx, exec_result));
            let num_captures = result_length.saturating_sub(1);

            // Extract the matched string
            key.replace(PropertyKey::array_index(cx, 0));
            let matched_value = maybe!(get(cx, exec_result, key));
            let matched_string = maybe!(to_string(cx, matched_value));

            // Extract the position of the matched string
            let matched_position = maybe!(get(cx, exec_result, cx.names.index()));
            let matched_position = maybe!(to_integer_or_infinity(cx, matched_position));
            let matched_position =
                f64::clamp(matched_position, 0.0, target_string.len() as f64) as u32;

            // Collect all captures by their capture index
            let mut indexed_captures = vec![];
            for i in 1..=num_captures {
                key.replace(PropertyKey::from_u64(cx, i));
                let capture_value = maybe!(get(cx, exec_result, key));
                if capture_value.is_undefined() {
                    indexed_captures.push(None);
                } else {
                    let capture_string = maybe!(to_string(cx, capture_value));
                    indexed_captures.push(Some(capture_string));
                }
            }

            let named_captures = maybe!(get(cx, exec_result, cx.names.groups()));

            let replacement_string = match replace_value {
                ReplaceValue::Function(replacer_function) => {
                    // Construct arguments for replacer function
                    let mut replacer_args = vec![];
                    replacer_args.push(matched_string.into());
                    replacer_args.extend(indexed_captures.into_iter().map(|capture| {
                        if let Some(capture) = capture {
                            capture.into()
                        } else {
                            cx.undefined()
                        }
                    }));
                    replacer_args.push(Value::from(matched_position).to_handle(cx));
                    replacer_args.push(target_string.into());

                    if !named_captures.is_undefined() {
                        replacer_args.push(named_captures);
                    }

                    // Call replacer function and return string
                    let replacement_value =
                        maybe!(call_object(cx, replacer_function, cx.undefined(), &replacer_args));

                    maybe!(to_string(cx, replacement_value))
                }
                ReplaceValue::String(replace_string) => {
                    let named_captures = if named_captures.is_undefined() {
                        None
                    } else {
                        Some(maybe!(to_object(cx, named_captures)))
                    };

                    // Cache substitution template since it does not change between matches
                    if substitution_template.is_none() {
                        let new_template =
                            SubstitutionTemplateParser::new(named_captures.is_some())
                                .parse(cx, replace_string);
                        substitution_template = Some(new_template);
                    }

                    // Apply substitution template
                    maybe!(substitution_template.as_ref().unwrap().get_substitution(
                        cx,
                        target_string,
                        matched_string,
                        matched_position,
                        &indexed_captures,
                        named_captures
                    ))
                }
            };

            // Add unchanged part between matches, then replacement for match
            if matched_position >= next_source_position {
                let unchanged_part = target_string
                    .substring(cx, next_source_position, matched_position)
                    .as_string();

                string_parts.push(unchanged_part);
                string_parts.push(replacement_string);

                next_source_position = matched_position + matched_string.len();
            }
        }

        // Add remaining portion of string
        if next_source_position < target_string.len() {
            let remaining_string = target_string
                .substring(cx, next_source_position, target_string.len())
                .as_string();
            string_parts.push(remaining_string);
        }

        StringValue::concat_all(cx, &string_parts).into()
    }

    // 22.2.6.12 RegExp.prototype [ @@search ]
    pub fn search(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "RegExpr.prototype[@@search] must be called on object");
        }

        let regexp_object = this_value.as_object();

        let string_arg = get_argument(cx, arguments, 0);
        let string_value = maybe!(to_string(cx, string_arg));

        // Save original last index, resetting to zero for search
        let previous_last_index = maybe!(get(cx, regexp_object, cx.names.last_index()));
        if !previous_last_index.is_positive_zero() {
            let zero_value = Value::from(0).to_handle(cx);
            maybe!(set(cx, regexp_object, cx.names.last_index(), zero_value, true));
        }

        // Perform RegExp search
        let result = maybe!(regexp_exec(cx, regexp_object, string_value));

        // Restore original last index
        let current_last_index = maybe!(get(cx, regexp_object, cx.names.last_index()));
        if !same_value(current_last_index, previous_last_index) {
            maybe!(set(cx, regexp_object, cx.names.last_index(), previous_last_index, true));
        }

        if result.is_null() {
            Value::from(-1).to_handle(cx).into()
        } else {
            get(cx, result.as_object(), cx.names.index())
        }
    }

    // 22.2.6.13 get RegExp.prototype.source
    pub fn source(
        mut cx: Context,
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

    // 22.2.6.14 RegExp.prototype [ @@split ]
    pub fn split(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let regexp_object = if this_value.is_object() {
            this_value.as_object()
        } else {
            return type_error_(cx, "RegExpr.prototype[@@split] must be called on an object");
        };

        let string_arg = get_argument(cx, arguments, 0);
        let string_value = maybe!(to_string(cx, string_arg));

        let constructor =
            maybe!(species_constructor(cx, regexp_object, Intrinsic::RegExpConstructor));

        // Get flags string, determining if a unicode or sticky flag is set
        let flags_string = maybe!(get(cx, regexp_object, cx.names.flags()));
        let mut flags_string = maybe!(to_string(cx, flags_string));

        let mut is_unicode = false;
        let mut is_sticky = false;
        for code_point in flags_string.iter_code_points() {
            if code_point == 'u' as u32 || code_point == 'v' as u32 {
                is_unicode = true;
            } else if code_point == 'y' as u32 {
                is_sticky = true;
            }
        }

        // Make sure the sticky flag is included in the flags string
        if !is_sticky {
            let y_string = InternedStrings::get_str(cx, "y");
            flags_string = StringValue::concat(cx, flags_string, y_string);
        }

        let splitter =
            maybe!(construct(cx, constructor, &[regexp_object.into(), flags_string.into()], None));

        let result_array: Handle<ObjectValue> = maybe!(array_create(cx, 0, None)).into();

        // Calculate optional limit argument
        let limit_arg = get_argument(cx, arguments, 1);
        let limit = if limit_arg.is_undefined() {
            u32::MAX
        } else {
            maybe!(to_uint32(cx, limit_arg))
        };

        if limit == 0 {
            return result_array.into();
        }

        // Handle the empty string case
        if string_value.is_empty() {
            let exec_result = maybe!(regexp_exec(cx, splitter, string_value));
            if !exec_result.is_null() {
                return result_array.into();
            }

            let zero_key = PropertyKey::from_u8(0).to_handle(cx);
            maybe!(create_data_property_or_throw(cx, result_array, zero_key, string_value.into()));
        }

        // Property keys are shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        let size = string_value.len();
        let mut array_length = 0;
        let mut p = 0;
        let mut q = 0;

        // Keep executing RegExp until there are no more matches or the entire string has been
        // searched.
        while q < size {
            let q_value = Value::from(q).to_handle(cx);
            maybe!(set(cx, splitter, cx.names.last_index(), q_value, true));

            // Execute RegExp at current index, advancing to next index if there is no match
            let exec_result = maybe!(regexp_exec(cx, splitter, string_value));

            if exec_result.is_null() {
                q = advance_string_index(string_value, q, is_unicode);
            } else {
                // Otherwise there was a match so determine end of match
                let exec_result = exec_result.as_object();

                let e = maybe!(get(cx, splitter, cx.names.last_index()));
                let e = maybe!(to_length(cx, e));
                let e = u64::min(e, size as u64) as u32;

                // If there was a match but it is empty then advance to next index
                if e == p {
                    q = advance_string_index(string_value, q, is_unicode);
                } else {
                    // Add portion of the string since the last match to the result array
                    let match_slice = string_value.substring(cx, p, q).as_string();

                    key.replace(PropertyKey::array_index(cx, array_length));
                    maybe!(create_data_property_or_throw(
                        cx,
                        result_array,
                        key,
                        match_slice.into()
                    ));

                    // Check if we have hit split limit
                    array_length += 1;
                    if array_length == limit {
                        return result_array.into();
                    }

                    p = e;

                    // Add capture groups to the result array
                    let number_of_captures = maybe!(length_of_array_like(cx, exec_result));
                    let number_of_captures = number_of_captures.saturating_sub(1);

                    for i in 1..=number_of_captures {
                        key.replace(PropertyKey::from_u64(cx, i));
                        let next_capture = maybe!(get(cx, exec_result, key));

                        key.replace(PropertyKey::array_index(cx, array_length));
                        maybe!(create_data_property_or_throw(cx, result_array, key, next_capture));

                        // Check if we have hit split limit
                        array_length += 1;
                        if array_length == limit {
                            return result_array.into();
                        }
                    }

                    q = p;
                }
            }
        }

        // Add remaining portion of the original string to the result array
        let remaining_string = string_value.substring(cx, p, size).as_string();
        key.replace(PropertyKey::array_index(cx, array_length));
        maybe!(create_data_property_or_throw(cx, result_array, key, remaining_string.into()));

        result_array.into()
    }

    // 22.2.6.15 get RegExp.prototype.sticky
    pub fn sticky(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::STICKY)
    }

    // 22.2.6.16 RegExp.prototype.test
    pub fn test(
        cx: Context,
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
    pub fn to_string(
        cx: Context,
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
    pub fn unicode(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::UNICODE_AWARE)
    }

    // 22.2.6.19 get RegExp.prototype.unicodeSets
    pub fn unicode_sets(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        regexp_has_flag(cx, this_value, RegExpFlags::UNICODE_SETS)
    }
}

// 22.2.6.4.1 RegExpHasFlag
fn regexp_has_flag(
    cx: Context,
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
    cx: Context,
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
    cx: Context,
    regexp_object: Handle<RegExpObject>,
    string_value: Handle<StringValue>,
) -> EvalResult<Handle<Value>> {
    let compiled_regexp = regexp_object.compiled_regexp();
    let string_length = string_value.len();

    let last_index = maybe!(get(cx, regexp_object.into(), cx.names.last_index()));
    let mut last_index = maybe!(to_length(cx, last_index));

    let flags = regexp_object.flags();
    let is_global = flags.is_global();
    let is_sticky = flags.is_sticky();
    let has_indices = flags.has_indices();

    if !is_global && !is_sticky {
        last_index = 0;
    }

    // Check if last index is already out of range meaning the match will always fail, resetting
    // last index under certain flags.
    if last_index > string_length as u64 {
        if is_global || is_sticky {
            let zero_value = Value::from(0).to_handle(cx);
            maybe!(set(cx, regexp_object.into(), cx.names.last_index(), zero_value, true));
        }

        return cx.null().into();
    }
    let last_index = last_index as u32;

    // Run the matching engine on the regexp and input string
    let match_ = run_matcher(compiled_regexp.get_(), string_value, last_index);

    // Handle match failure, resetting last index under sticky flag
    if match_.is_none() {
        if is_global || is_sticky {
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

/// 22.2.7.3 AdvanceStringIndex
/// Increments the index by one if not unicode-aware, and by the size of the current code point if unicode-aware.
///
/// Caller must ensure that the index is not out of bounds.
pub fn advance_string_index(
    string_value: Handle<StringValue>,
    prev_index: u32,
    is_unicode: bool,
) -> u32 {
    if !is_unicode {
        return prev_index + 1;
    }

    let num_code_units = if needs_surrogate_pair(string_value.code_point_at(prev_index)) {
        2
    } else {
        1
    };

    prev_index + num_code_units
}

/// Saem as AdvanceStringIndex, but index is expanded to the u64 range and is not guarnateed to be
/// in bounds for the string.
pub fn advance_u64_string_index(
    string_value: Handle<StringValue>,
    prev_index: u64,
    is_unicode: bool,
) -> u64 {
    let string_length = string_value.len();
    if prev_index + 1 >= string_length as u64 {
        return prev_index + 1;
    }

    advance_string_index(string_value, prev_index as u32, is_unicode) as u64
}
