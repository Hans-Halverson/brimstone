use std::collections::HashSet;

use crate::{
    common::unicode::{
        CodePoint, is_high_surrogate_code_unit, is_low_surrogate_code_unit, needs_surrogate_pair,
    },
    intrinsic_getter_methods, intrinsic_methods, must,
    parser::regexp::RegExpFlags,
    runtime::{
        Context, Handle, PropertyKey, Value,
        abstract_operations::{
            call, call_object, construct, create_data_property_or_throw, length_of_array_like, set,
            species_constructor,
        },
        alloc_error::AllocResult,
        array_object::{array_create, create_array_from_list},
        error::type_error,
        eval_result::EvalResult,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            regexp_constructor::{FlagsSource, RegExpSource, as_regexp_object, regexp_init},
            regexp_object::RegExpObject,
            regexp_string_iterator_object::RegExpStringIteratorObject,
            rust_runtime::RuntimeFunction,
            string_prototype::{ReplaceValue, SubstitutionTemplateParser},
        },
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create_without_proto,
        realm::Realm,
        regexp::matcher::run_matcher,
        string_value::StringValue,
        to_string,
        type_utilities::{
            is_callable, same_object_value, same_value, to_boolean, to_integer_or_infinity,
            to_length, to_object, to_uint32,
        },
    },
    runtime_fn,
};

pub struct RegExpPrototype;

impl RegExpPrototype {
    /// Properties of the RegExp Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-regexp-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once RegExpConstructor has been created
        intrinsic_methods!(cx, builder, {
            exec      RegExpPrototype_exec      (1),
            test      RegExpPrototype_test      (1),
            to_string RegExpPrototype_to_string (0),
        });

        intrinsic_getter_methods!(cx, builder, {
            dot_all      RegExpPrototype_dot_all,
            flags        RegExpPrototype_flags,
            global       RegExpPrototype_global,
            has_indices  RegExpPrototype_has_indices,
            ignore_case  RegExpPrototype_ignore_case,
            multiline    RegExpPrototype_multiline,
            source       RegExpPrototype_source,
            sticky       RegExpPrototype_sticky,
            unicode      RegExpPrototype_unicode,
            unicode_sets RegExpPrototype_unicode_sets,
        });

        // RegExp.prototype [ @@match ] (https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.match%)
        builder.method(cx.symbols.match_(), RuntimeFunction::RegExpPrototype_match_, 1)?;

        // RegExp.prototype [ @@matchAll ] (https://tc39.es/ecma262/#sec-regexp-prototype-%symbol.matchall%)
        builder.method(cx.symbols.match_all(), RuntimeFunction::RegExpPrototype_match_all, 1)?;

        // RegExp.prototype [ @@replace ] (https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.replace%)
        builder.method(cx.symbols.replace(), RuntimeFunction::RegExpPrototype_replace, 2)?;

        // RegExp.prototype [ @@search ] (https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.search%)
        builder.method(cx.symbols.search(), RuntimeFunction::RegExpPrototype_search, 1)?;

        // RegExp.prototype [ @@split ] (https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.split%)
        builder.method(cx.symbols.split(), RuntimeFunction::RegExpPrototype_split, 2)?;

        builder.build()
    }

    /// Additional Properties of the RegExp.prototype Object (https://tc39.es/ecma262/#sec-additional-properties-of-the-regexp.prototype-object)
    pub fn init_annex_b_methods(
        regexp_prototype: Handle<ObjectValue>,
        mut cx: Context,
        realm: Handle<Realm>,
    ) -> AllocResult<()> {
        let compile_name = cx.alloc_static_string("compile")?;
        let compile_key = PropertyKey::string_not_array_index_handle(cx, compile_name)?;

        let mut builder = IntrinsicBuilder::ordinary(cx, realm, regexp_prototype);
        builder.method(compile_key, RuntimeFunction::RegExpPrototype_compile, 2)?;
        builder.build()?;

        Ok(())
    }

    runtime_fn! {
    /// RegExp.prototype.exec (https://tc39.es/ecma262/#sec-regexp.prototype.exec)
    fn exec(cx, this_value, arguments) {
        let regexp_object = this_regexp_object(cx, this_value, "RegExp.prototype.exec")?;

        let string_arg = arguments.get(cx, 0);
        let string_value = to_string(cx, string_arg)?;

        regexp_builtin_exec(cx, regexp_object, string_value)
    }}

    runtime_fn! {
    /// get RegExp.prototype.dotAll (https://tc39.es/ecma262/#sec-get-regexp.prototype.dotAll)
    fn dot_all(cx, this_value, _) {
        regexp_has_flag(cx, this_value, RegExpFlags::DOT_ALL, "RegExp.prototype.dotAll")
    }}

    runtime_fn! {
    /// get RegExp.prototype.flags (https://tc39.es/ecma262/#sec-get-regexp.prototype.flags)
    fn flags(cx, this_value, _) {
        let this_object = this_object(cx, this_value, "RegExp.prototype.flags")?;

        let mut flags_string = String::new();

        let has_indices_value = get(cx, this_object, cx.names.has_indices())?;
        if to_boolean(*has_indices_value) {
            flags_string.push('d');
        }

        let global_value = get(cx, this_object, cx.names.global())?;
        if to_boolean(*global_value) {
            flags_string.push('g');
        }

        let ignore_case_value = get(cx, this_object, cx.names.ignore_case())?;
        if to_boolean(*ignore_case_value) {
            flags_string.push('i');
        }

        let multiline_value = get(cx, this_object, cx.names.multiline())?;
        if to_boolean(*multiline_value) {
            flags_string.push('m');
        }

        let dot_all_value = get(cx, this_object, cx.names.dot_all())?;
        if to_boolean(*dot_all_value) {
            flags_string.push('s');
        }

        let unicode_value = get(cx, this_object, cx.names.unicode())?;
        if to_boolean(*unicode_value) {
            flags_string.push('u');
        }

        let unicode_sets_value = get(cx, this_object, cx.names.unicode_sets())?;
        if to_boolean(*unicode_sets_value) {
            flags_string.push('v');
        }

        let sticky_value = get(cx, this_object, cx.names.sticky())?;
        if to_boolean(*sticky_value) {
            flags_string.push('y');
        }

        let flags_string = if flags_string.is_empty() {
            cx.names.empty_string().as_string()
        } else {
            cx.alloc_string(&flags_string)?
        };

        Ok(flags_string.as_value())
    }}

    runtime_fn! {
    /// get RegExp.prototype.global (https://tc39.es/ecma262/#sec-get-regexp.prototype.global)
    fn global(cx, this_value, _) {
        regexp_has_flag(cx, this_value, RegExpFlags::GLOBAL, "RegExp.prototype.global")
    }}

    runtime_fn! {
    /// get RegExp.prototype.hasIndices (https://tc39.es/ecma262/#sec-get-regexp.prototype.hasIndices)
    fn has_indices(cx, this_value, _) {
        regexp_has_flag(cx, this_value, RegExpFlags::HAS_INDICES, "RegExp.prototype.hasIndices")
    }}

    runtime_fn! {
    /// get RegExp.prototype.ignoreCase (https://tc39.es/ecma262/#sec-get-regexp.prototype.ignorecase)
    fn ignore_case(cx, this_value, _) {
        regexp_has_flag(cx, this_value, RegExpFlags::IGNORE_CASE, "RegExp.prototype.ignoreCase")
    }}

    runtime_fn! {
    /// RegExp.prototype [ @@match ] (https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.match%)
    fn match_(cx, this_value, arguments) {
        let regexp_object = this_object(cx, this_value, "RegExp.prototype[@@match]")?;

        let string_arg = arguments.get(cx, 0);
        let string_value = to_string(cx, string_arg)?;

        let flags_string = get(cx, regexp_object, cx.names.flags())?;
        let flags_string = to_string(cx, flags_string)?;

        let is_global = flags_string_contains(flags_string, 'g' as u32)?;
        let is_unicode = flags_string_contains(flags_string, 'u' as u32)?
            || flags_string_contains(flags_string, 'v' as u32)?;

        if !is_global {
            return regexp_exec(cx, regexp_object, string_value, "RegExp.prototype[@@match]");
        }

        let zero_value = cx.zero();
        set(cx, regexp_object, cx.names.last_index(), zero_value, true)?;

        let result_array = array_create(cx, 0, None)?;
        let mut n = 0;

        loop {
            let result = regexp_exec(cx, regexp_object, string_value, "RegExp.prototype[@@match]")?;
            if result.is_null() {
                if n == 0 {
                    return Ok(cx.null());
                } else {
                    return Ok(result_array.as_value());
                }
            }

            // RegExpExec only returns an object or null
            debug_assert!(result.is_object());
            let result_object = result.as_object();

            let zero_key = PropertyKey::array_index_handle(cx, 0)?;
            let match_string = get(cx, result_object, zero_key)?;
            let match_string = to_string(cx, match_string)?;

            let n_key = PropertyKey::array_index_handle(cx, n)?;
            must!(create_data_property_or_throw(
                cx,
                result_array.into(),
                n_key,
                match_string.into()
            ));

            if match_string.is_empty() {
                let last_index = get(cx, regexp_object, cx.names.last_index())?;
                let last_index = to_length(cx, last_index)?;

                let next_index = advance_u64_string_index(string_value, last_index, is_unicode)?;
                let next_index_value = cx.number(next_index);
                set(cx, regexp_object, cx.names.last_index(), next_index_value, true)?;
            }

            n += 1;
        }
    }}

    runtime_fn! {
    /// RegExp.prototype [ @@matchAll ] (https://tc39.es/ecma262/#sec-regexp-prototype-%symbol.matchall%)
    fn match_all(cx, this_value, arguments) {
        let regexp_object = this_object(cx, this_value, "RegExp.prototype[@@matchAll]")?;

        let string_arg = arguments.get(cx, 0);
        let string_value = to_string(cx, string_arg)?;

        let constructor = species_constructor(cx, regexp_object, Intrinsic::RegExpConstructor)?;

        let flags_string = get(cx, regexp_object, cx.names.flags())?;
        let flags_string = to_string(cx, flags_string)?;

        let matcher =
            construct(cx, constructor, &[regexp_object.into(), flags_string.into()], None)?;

        let last_index = get(cx, regexp_object, cx.names.last_index())?;
        let last_index = to_length(cx, last_index)?;
        let last_index_value = cx.number(last_index);

        set(cx, matcher, cx.names.last_index(), last_index_value, true)?;

        let is_global = flags_string_contains(flags_string, 'g' as u32)?;
        let is_unicode = flags_string_contains(flags_string, 'u' as u32)?
            || flags_string_contains(flags_string, 'v' as u32)?;

        Ok(
            RegExpStringIteratorObject::new(cx, matcher, string_value, is_global, is_unicode)?
                .as_value(),
        )
    }}

    runtime_fn! {
    /// get RegExp.prototype.multiline (https://tc39.es/ecma262/#sec-get-regexp.prototype.multiline)
    fn multiline(cx, this_value, _) {
        regexp_has_flag(cx, this_value, RegExpFlags::MULTILINE, "RegExp.prototype.multiline")
    }}

    runtime_fn! {
    /// RegExp.prototype [ @@replace ] (https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.replace%)
    fn replace(cx, this_value, arguments) {
        let regexp_object = this_object(cx, this_value, "RegExp.prototype[@@replace]")?;

        let target_string_arg = arguments.get(cx, 0);
        let target_string = to_string(cx, target_string_arg)?;

        let replace_arg = arguments.get(cx, 1);
        let replace_value = if is_callable(replace_arg) {
            ReplaceValue::Function(replace_arg.as_object())
        } else {
            ReplaceValue::String(to_string(cx, replace_arg)?)
        };

        // Get flags string, determining if RegExp is unicode or global
        let flags_value = get(cx, regexp_object, cx.names.flags())?;
        let flags_string = to_string(cx, flags_value)?;

        let mut is_unicode = false;
        let mut is_global = false;
        for code_point in flags_string.iter_code_points()? {
            if code_point == 'u' as u32 || code_point == 'v' as u32 {
                is_unicode = true;
            } else if code_point == 'g' as u32 {
                is_global = true;
            }
        }

        if is_global {
            let zero_value = cx.zero();
            set(cx, regexp_object, cx.names.last_index(), zero_value, true)?;
        }

        // Key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        let mut exec_results = vec![];

        loop {
            // Search target string, finding all matches if global
            let exec_result =
                regexp_exec(cx, regexp_object, target_string, "RegExp.prototype[@@replace]")?;
            if exec_result.is_null() {
                break;
            }

            let exec_result = exec_result.as_object();

            if !is_global {
                exec_results.push(exec_result);
                break;
            }

            // Extract matched string
            key.replace(PropertyKey::array_index(cx, 0)?);
            let matched_value = get(cx, exec_result, key)?;
            let matched_string = to_string(cx, matched_value)?;

            exec_results.push(exec_result);

            // If matched string is empty then increment last index
            if matched_string.is_empty() {
                let this_index = get(cx, regexp_object, cx.names.last_index())?;
                let this_index = to_length(cx, this_index)?;

                let next_index = advance_u64_string_index(target_string, this_index, is_unicode)?;
                let next_index_value = cx.number(next_index);
                set(cx, regexp_object, cx.names.last_index(), next_index_value, true)?;
            }
        }

        let mut string_parts = vec![];
        let mut next_source_position = 0;

        // Cached substitution template
        let mut substitution_template = None;

        for exec_result in exec_results {
            let result_length = length_of_array_like(cx, exec_result)?;
            let num_captures = result_length.saturating_sub(1);

            // Extract the matched string
            key.replace(PropertyKey::array_index(cx, 0)?);
            let matched_value = get(cx, exec_result, key)?;
            let matched_string = to_string(cx, matched_value)?;

            // Extract the position of the matched string
            let matched_position = get(cx, exec_result, cx.names.index())?;
            let matched_position = to_integer_or_infinity(cx, matched_position)?;
            let matched_position =
                f64::clamp(matched_position, 0.0, target_string.len() as f64) as u32;

            // Collect all captures by their capture index
            let mut indexed_captures = vec![];
            for i in 1..=num_captures {
                key.replace(PropertyKey::from_u64(cx, i)?);
                let capture_value = get(cx, exec_result, key)?;
                if capture_value.is_undefined() {
                    indexed_captures.push(None);
                } else {
                    let capture_string = to_string(cx, capture_value)?;
                    indexed_captures.push(Some(capture_string));
                }
            }

            let named_captures = get(cx, exec_result, cx.names.groups())?;

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
                    replacer_args.push(cx.number(matched_position));
                    replacer_args.push(target_string.into());

                    if !named_captures.is_undefined() {
                        replacer_args.push(named_captures);
                    }

                    // Call replacer function and return string
                    let replacement_value =
                        call_object(cx, replacer_function, cx.undefined(), &replacer_args)?;

                    to_string(cx, replacement_value)?
                }
                ReplaceValue::String(replace_string) => {
                    let named_captures = if named_captures.is_undefined() {
                        None
                    } else {
                        Some(to_object(cx, named_captures)?)
                    };

                    // Cache substitution template since it does not change between matches
                    if substitution_template.is_none() {
                        let new_template =
                            SubstitutionTemplateParser::new(named_captures.is_some())
                                .parse(cx, replace_string)?;
                        substitution_template = Some(new_template);
                    }

                    // Apply substitution template
                    substitution_template.as_ref().unwrap().get_substitution(
                        cx,
                        target_string,
                        matched_string,
                        matched_position,
                        &indexed_captures,
                        named_captures,
                    )?
                }
            };

            // Add unchanged part between matches, then replacement for match
            if matched_position >= next_source_position {
                let unchanged_part = target_string
                    .substring(cx, next_source_position, matched_position)?
                    .as_string();

                string_parts.push(unchanged_part);
                string_parts.push(replacement_string);

                next_source_position = matched_position + matched_string.len();
            }
        }

        // Add remaining portion of string
        if next_source_position < target_string.len() {
            let remaining_string = target_string
                .substring(cx, next_source_position, target_string.len())?
                .as_string();
            string_parts.push(remaining_string);
        }

        Ok(StringValue::concat_all(cx, &string_parts)?.as_value())
    }}

    runtime_fn! {
    /// RegExp.prototype [ @@search ] (https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.search%)
    fn search(cx, this_value, arguments) {
        let regexp_object = this_object(cx, this_value, "RegExp.prototype[@@search]")?;

        let string_arg = arguments.get(cx, 0);
        let string_value = to_string(cx, string_arg)?;

        // Save original last index, resetting to zero for search
        let previous_last_index = get(cx, regexp_object, cx.names.last_index())?;
        if !previous_last_index.is_positive_zero() {
            let zero_value = cx.zero();
            set(cx, regexp_object, cx.names.last_index(), zero_value, true)?;
        }

        // Perform RegExp search
        let result = regexp_exec(cx, regexp_object, string_value, "RegExp.prototype[@@search]")?;

        // Restore original last index
        let current_last_index = get(cx, regexp_object, cx.names.last_index())?;
        if !same_value(current_last_index, previous_last_index)? {
            set(cx, regexp_object, cx.names.last_index(), previous_last_index, true)?;
        }

        if result.is_null() {
            Ok(cx.negative_one())
        } else {
            get(cx, result.as_object(), cx.names.index())
        }
    }}

    runtime_fn! {
    /// get RegExp.prototype.source (https://tc39.es/ecma262/#sec-get-regexp.prototype.source)
    fn source(cx, this_value, _) {
        if this_value.is_object() {
            let this_object = this_value.as_object();
            if let Some(regexp_object) = this_object.as_opt::<RegExpObject>() {
                return Ok(regexp_object.escaped_pattern_source().as_value());
            } else if same_object_value(
                *this_object,
                cx.get_intrinsic_ptr(Intrinsic::RegExpPrototype),
            ) {
                return Ok(cx.alloc_static_string("(?:)")?.as_value());
            }
        }

        type_error(cx, "RegExp.prototype.source must be called on a RegExp")
    }}

    runtime_fn! {
    /// RegExp.prototype [ @@split ] (https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.split%)
    fn split(cx, this_value, arguments) {
        let regexp_object = this_object(cx, this_value, "RegExp.prototype[@@split]")?;

        let string_arg = arguments.get(cx, 0);
        let string_value = to_string(cx, string_arg)?;

        let constructor = species_constructor(cx, regexp_object, Intrinsic::RegExpConstructor)?;

        // Get flags string, determining if a unicode or sticky flag is set
        let flags_string = get(cx, regexp_object, cx.names.flags())?;
        let mut flags_string = to_string(cx, flags_string)?;

        let mut is_unicode = false;
        let mut is_sticky = false;
        for code_point in flags_string.iter_code_points()? {
            if code_point == 'u' as u32 || code_point == 'v' as u32 {
                is_unicode = true;
            } else if code_point == 'y' as u32 {
                is_sticky = true;
            }
        }

        // Make sure the sticky flag is included in the flags string
        if !is_sticky {
            let y_string = cx.alloc_static_string("y")?;
            flags_string = StringValue::concat(cx, flags_string, y_string)?;
        }

        let splitter =
            construct(cx, constructor, &[regexp_object.into(), flags_string.into()], None)?;

        let result_array = array_create(cx, 0, None)?.as_object();

        // Calculate optional limit argument
        let limit_arg = arguments.get(cx, 1);
        let limit = if limit_arg.is_undefined() {
            u32::MAX
        } else {
            to_uint32(cx, limit_arg)?
        };

        if limit == 0 {
            return Ok(result_array.as_value());
        }

        // Handle the empty string case
        if string_value.is_empty() {
            let exec_result = regexp_exec(cx, splitter, string_value, "RegExp.prototype[@@split]")?;
            if !exec_result.is_null() {
                return Ok(result_array.as_value());
            }

            let zero_key = PropertyKey::from_u8(0).to_handle(cx);
            create_data_property_or_throw(cx, result_array, zero_key, string_value.into())?;
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
            let q_value = cx.number(q);
            set(cx, splitter, cx.names.last_index(), q_value, true)?;

            // Execute RegExp at current index, advancing to next index if there is no match
            let exec_result = regexp_exec(cx, splitter, string_value, "RegExp.prototype[@@split]")?;

            if exec_result.is_null() {
                q = advance_string_index(string_value, q, is_unicode)?;
            } else {
                // Otherwise there was a match so determine end of match
                let exec_result = exec_result.as_object();

                let e = get(cx, splitter, cx.names.last_index())?;
                let e = to_length(cx, e)?;
                let e = u64::min(e, size as u64) as u32;

                // If there was a match but it is empty then advance to next index
                if e == p {
                    q = advance_string_index(string_value, q, is_unicode)?;
                } else {
                    // Add portion of the string since the last match to the result array
                    let match_slice = string_value.substring(cx, p, q)?.as_string();

                    key.replace(PropertyKey::array_index(cx, array_length)?);
                    create_data_property_or_throw(cx, result_array, key, match_slice.into())?;

                    // Check if we have hit split limit
                    array_length += 1;
                    if array_length == limit {
                        return Ok(result_array.as_value());
                    }

                    p = e;

                    // Add capture groups to the result array
                    let number_of_captures = length_of_array_like(cx, exec_result)?;
                    let number_of_captures = number_of_captures.saturating_sub(1);

                    for i in 1..=number_of_captures {
                        key.replace(PropertyKey::from_u64(cx, i)?);
                        let next_capture = get(cx, exec_result, key)?;

                        key.replace(PropertyKey::array_index(cx, array_length)?);
                        create_data_property_or_throw(cx, result_array, key, next_capture)?;

                        // Check if we have hit split limit
                        array_length += 1;
                        if array_length == limit {
                            return Ok(result_array.as_value());
                        }
                    }

                    q = p;
                }
            }
        }

        // Add remaining portion of the original string to the result array
        let remaining_string = string_value.substring(cx, p, size)?.as_string();
        key.replace(PropertyKey::array_index(cx, array_length)?);
        create_data_property_or_throw(cx, result_array, key, remaining_string.into())?;

        Ok(result_array.as_value())
    }}

    runtime_fn! {
    /// get RegExp.prototype.sticky (https://tc39.es/ecma262/#sec-get-regexp.prototype.sticky)
    fn sticky(cx, this_value, _) {
        regexp_has_flag(cx, this_value, RegExpFlags::STICKY, "RegExp.prototype.sticky")
    }}

    runtime_fn! {
    /// RegExp.prototype.test (https://tc39.es/ecma262/#sec-regexp.prototype.test)
    fn test(cx, this_value, arguments) {
        let regexp_object = this_object(cx, this_value, "test")?;

        let string_arg = arguments.get(cx, 0);
        let string_value = to_string(cx, string_arg)?;

        let exec_result = regexp_exec(cx, regexp_object, string_value, "RegExp.prototype.test")?;

        Ok(cx.bool(!exec_result.is_null()))
    }}

    runtime_fn! {
    /// RegExp.prototype.toString (https://tc39.es/ecma262/#sec-regexp.prototype.tostring)
    fn to_string(cx, this_value, _) {
        let this_object = this_object(cx, this_value, "toString")?;

        let pattern_value = get(cx, this_object, cx.names.source())?;
        let pattern_string = to_string(cx, pattern_value)?;

        let flags_value = get(cx, this_object, cx.names.flags())?;
        let flags_string = to_string(cx, flags_value)?;

        let slash_string = cx.names.slash().as_string();

        let full_string = StringValue::concat_all(
            cx,
            &[slash_string, pattern_string, slash_string, flags_string],
        )?;

        Ok(full_string.as_value())
    }}

    runtime_fn! {
    /// get RegExp.prototype.unicode (https://tc39.es/ecma262/#sec-get-regexp.prototype.unicode)
    fn unicode(cx, this_value, _) {
        regexp_has_flag(cx, this_value, RegExpFlags::UNICODE_AWARE, "RegExp.prototype.unicode")
    }}

    runtime_fn! {
    /// get RegExp.prototype.unicodeSets (https://tc39.es/ecma262/#sec-get-regexp.prototype.unicodesets)
    fn unicode_sets(cx, this_value, _) {
        regexp_has_flag(cx, this_value, RegExpFlags::UNICODE_SETS, "RegExp.prototype.unicodeSets")
    }}

    runtime_fn! {
    /// RegExp.prototype.compile (https://tc39.es/ecma262/#sec-regexp.prototype.compile)
    fn compile(cx, this_value, arguments) {
        let regexp_object = this_regexp_object(cx, this_value, "RegExp.prototype.compile")?;

        let pattern_arg = arguments.get(cx, 0);
        let flags_arg = arguments.get(cx, 1);

        let pattern_source = if let Some(pattern_regexp_object) = as_regexp_object(pattern_arg) {
            if !flags_arg.is_undefined() {
                return type_error(
                    cx,
                    "RegExp.prototype.compile cannot specify flags when pattern is a RegExp",
                );
            }

            RegExpSource::RegExpObject(pattern_regexp_object)
        } else {
            RegExpSource::PatternAndFlags(pattern_arg, FlagsSource::Value(flags_arg))
        };

        regexp_init(cx, regexp_object, pattern_source)
    }}
}

fn this_object(
    cx: Context,
    this_value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<ObjectValue>> {
    if this_value.is_object() {
        return Ok(this_value.as_object());
    }

    type_error(cx, &format!("{method_name} must be called on an object"))
}

fn this_regexp_object(
    cx: Context,
    this_value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<RegExpObject>> {
    if this_value.is_object() {
        if let Some(regexp_object) = this_value.as_opt::<RegExpObject>() {
            return Ok(regexp_object);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a RegExp"))
}

/// RegExpHasFlag (https://tc39.es/ecma262/#sec-regexphasflag)
fn regexp_has_flag(
    cx: Context,
    this_value: Handle<Value>,
    flag: RegExpFlags,
    method_name: &str,
) -> EvalResult<Handle<Value>> {
    if this_value.is_object() {
        let this_object = this_value.as_object();
        if let Some(regexp_object) = this_object.as_opt::<RegExpObject>() {
            let has_flag = regexp_object.flags().contains(flag);
            return Ok(cx.bool(has_flag));
        } else if same_object_value(*this_object, cx.get_intrinsic_ptr(Intrinsic::RegExpPrototype))
        {
            return Ok(cx.undefined());
        }
    }

    type_error(cx, &format!("{method_name} must be called on a RegExp"))
}

pub fn flags_string_contains(
    flags_string: Handle<StringValue>,
    flag: CodePoint,
) -> AllocResult<bool> {
    Ok(flags_string.iter_code_points()?.any(|c| c == flag))
}

/// RegExpExec (https://tc39.es/ecma262/#sec-regexpexec)
pub fn regexp_exec(
    cx: Context,
    regexp_object: Handle<ObjectValue>,
    string_value: Handle<StringValue>,
    method_name: &str,
) -> EvalResult<Handle<Value>> {
    let exec = get(cx, regexp_object, cx.names.exec())?;

    if is_callable(exec) {
        let exec_result = call(cx, exec, regexp_object.into(), &[string_value.into()])?;
        if !exec_result.is_null() && !exec_result.is_object() {
            return type_error(cx, &format!("{method_name} `exec` must return null or an object"));
        }

        return Ok(exec_result);
    }

    let Some(regexp_object) = regexp_object.as_opt::<RegExpObject>() else {
        return type_error(cx, &format!("{method_name} must be called on a RegExp"));
    };

    regexp_builtin_exec(cx, regexp_object, string_value)
}

/// RegExpBuiltinExec (https://tc39.es/ecma262/#sec-regexpbuiltinexec)
fn regexp_builtin_exec(
    cx: Context,
    regexp_object: Handle<RegExpObject>,
    string_value: Handle<StringValue>,
) -> EvalResult<Handle<Value>> {
    let compiled_regexp = regexp_object.compiled_regexp();
    let string_length = string_value.len();

    let last_index = get(cx, regexp_object.into(), cx.names.last_index())?;
    let mut last_index = to_length(cx, last_index)?;

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
            let zero_value = cx.zero();
            set(cx, regexp_object.into(), cx.names.last_index(), zero_value, true)?;
        }

        return Ok(cx.null());
    }
    let last_index = last_index as u32;

    // Matcher starts at the beginning of a code point in unicode mode
    let matcher_start_index = if flags.has_any_unicode_flag() {
        snap_index_to_code_point(string_value, last_index)?
    } else {
        last_index
    };

    // Run the matching engine on the regexp and input string
    let match_ = run_matcher(cx, compiled_regexp, string_value, matcher_start_index)?;

    // Handle match failure, resetting last index under sticky flag
    if match_.is_none() {
        if is_global || is_sticky {
            let zero_value = cx.zero();
            set(cx, regexp_object.into(), cx.names.last_index(), zero_value, true)?;
        }

        return Ok(cx.null());
    }

    let capture_groups = &match_.unwrap().capture_groups;

    // 0'th capture which matches entire pattern is guaranteed to exist
    let full_capture = capture_groups[0].as_ref().unwrap();

    // Update last index to point past end of capture
    if is_global || is_sticky {
        let last_index_value = cx.number(full_capture.end);
        set(cx, regexp_object.into(), cx.names.last_index(), last_index_value, true)?;
    }

    // Build result array of matches
    let result_array = must!(array_create(cx, capture_groups.len() as u64, None)).as_object();

    // Mark the start of the full match
    let index_value = cx.number(full_capture.start);
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
        ordinary_object_create_without_proto(cx)?.into()
    } else {
        cx.undefined()
    };
    must!(create_data_property_or_throw(
        cx,
        result_array,
        cx.names.groups(),
        named_groups_object
    ));

    let mut matched_group_names = HashSet::new();

    // Set up indices array to collect capture group indices, if flag is set
    let indices_result = if has_indices {
        let indices_array = must!(array_create(cx, capture_groups.len() as u64, None)).as_object();

        // Indices array contains named capture groups object if there are any named groups
        let named_groups_object = if compiled_regexp.has_named_capture_groups {
            ordinary_object_create_without_proto(cx)?.into()
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
                .substring(cx, capture.start, capture.end)?
                .as_string()
                .into()
        } else {
            cx.undefined()
        };

        let index_key = PropertyKey::from_u64_handle(cx, i as u64)?;
        must!(create_data_property_or_throw(cx, result_array, index_key, captured_value));

        // Add capture indices to indices array if present
        let match_index_pair = if let Some((indices_array, indices_groups)) = indices_result {
            let match_index_pair = if let Some(capture) = capture {
                let start_index = cx.number(capture.start);
                let end_index = cx.number(capture.end);
                create_array_from_list(cx, &[start_index, end_index])?.into()
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
                let group_name_key = PropertyKey::string_handle(cx, group_name.as_string())?;

                // Group names object is guaranteed to be an object value
                let groups = named_groups_object.as_object();

                // If this name has not yet been matched then add it
                if !matched_group_names.contains(&group_name) {
                    must!(create_data_property_or_throw(
                        cx,
                        groups,
                        group_name_key,
                        captured_value
                    ));

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

                // Mark as matched if there was not capture
                if !captured_value.is_undefined() {
                    matched_group_names.insert(group_name);
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

    Ok(result_array.as_value())
}

/// AdvanceStringIndex (https://tc39.es/ecma262/#sec-advancestringindex)
///
/// Increments the index by one if not unicode-aware, and by the size of the current code point if unicode-aware.
///
/// Caller must ensure that the index is not out of bounds.
fn advance_string_index(
    string_value: Handle<StringValue>,
    prev_index: u32,
    is_unicode: bool,
) -> AllocResult<u32> {
    if !is_unicode {
        return Ok(prev_index + 1);
    }

    let num_code_units = if needs_surrogate_pair(string_value.code_point_at(prev_index)?) {
        2
    } else {
        1
    };

    Ok(prev_index + num_code_units)
}

/// Same as AdvanceStringIndex, but index is expanded to the u64 range and is not guaranteed to be
/// in bounds for the string.
pub fn advance_u64_string_index(
    string_value: Handle<StringValue>,
    prev_index: u64,
    is_unicode: bool,
) -> AllocResult<u64> {
    let string_length = string_value.len();
    if prev_index + 1 >= string_length as u64 {
        return Ok(prev_index + 1);
    }

    Ok(advance_string_index(string_value, prev_index as u32, is_unicode)? as u64)
}

/// If the index points to the middle of a valid surrogate pair in the given string return the start
/// of the code point. Otherwise return the original index.
fn snap_index_to_code_point(string_value: Handle<StringValue>, index: u32) -> AllocResult<u32> {
    let string_length = string_value.len();
    if index >= string_length || index == 0 {
        return Ok(index);
    }

    let code_unit = string_value.code_unit_at(index)?;
    if !is_low_surrogate_code_unit(code_unit) {
        return Ok(index);
    }

    let prev_index = index - 1;
    let prev_code_unit = string_value.code_unit_at(prev_index)?;
    if !is_high_surrogate_code_unit(prev_code_unit) {
        return Ok(index);
    }

    Ok(prev_index)
}
