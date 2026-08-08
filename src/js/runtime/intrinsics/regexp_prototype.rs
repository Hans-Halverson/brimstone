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
            call, call_object, construct, create_data_property_or_throw, length_of_array_like,
            species_constructor,
        },
        alloc_error::AllocResult,
        array_object::{
            ArrayCreateShape, array_create, array_create_in_realm, create_array_from_list,
            create_dense_data_property,
        },
        common_shapes::CommonShape,
        error::type_error,
        eval_result::EvalResult,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            regexp_constructor::{FlagsSource, RegExpSource, as_regexp_object, regexp_init},
            regexp_object::RegExpObject,
            regexp_string_iterator_object::RegExpStringIteratorObject,
            rust_runtime::{RuntimeFunction, is_builtin_function},
            string_prototype::{ReplaceValue, SubstitutionTemplate, SubstitutionTemplateParser},
        },
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create_without_proto,
        realm::Realm,
        regexp::{
            fast_proto_guard::FastRegExpProtoGuard,
            matcher::{Match, run_matcher},
        },
        string_value::StringValue,
        to_string,
        type_utilities::{
            is_callable, require_object_coercible, same_value, to_boolean, to_integer_or_infinity,
            to_object, to_uint32,
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

        regexp_builtin_exec(cx, regexp_object, string_value)?.to_value(cx, string_value)
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

        // Use the fast path for raw RegExp flag access if possible
        if let Some(flags) = FastRegExpProtoGuard::try_get_fast_flags(cx, this_object)? {
            return Ok(flags_to_string_value(cx, flags)?.as_value());
        }

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

        let flags = GenericFlags::new(cx, regexp_object)?;

        let is_global = flags.is_global()?;
        let is_unicode = flags.has_any_unicode_flag()?;

        if !is_global {
            return regexp_exec(cx, regexp_object, string_value, "RegExp.prototype[@@match]")?
                .to_value(cx, string_value);
        }

        RegExpObject::maybe_fast_set_last_index(cx, regexp_object, cx.zero())?;

        let result_array = array_create(cx, 0, None)?;
        let mut n = 0;

        loop {
            let exec_result =
                regexp_exec(cx, regexp_object, string_value, "RegExp.prototype[@@match]")?;

            let match_string = match exec_result {
                // Found the last match - return collected matches if any exit
                ExecResult::NoMatch => {
                    if n == 0 {
                        return Ok(cx.null());
                    } else {
                        return Ok(result_array.as_value());
                    }
                }
                // Collect the matched string
                ExecResult::Match(_, match_) => {
                    let match_bounds = match_.full_capture();
                    string_value
                        .substring(cx, match_bounds.start, match_bounds.end)?
                        .as_string()
                }
                ExecResult::Object(exec_result) => {
                    let zero_key = PropertyKey::array_index_handle(cx, 0)?;
                    let match_string = get(cx, exec_result, zero_key)?;
                    to_string(cx, match_string)?
                }
            };

            let n_key = PropertyKey::array_index_handle(cx, n)?;
            must!(create_data_property_or_throw(
                cx,
                result_array.into(),
                n_key,
                match_string.into()
            ));

            if match_string.is_empty() {
                let last_index = RegExpObject::maybe_fast_last_index_as_length(cx, regexp_object)?;

                let next_index = advance_u64_string_index(string_value, last_index, is_unicode)?;
                let next_index_value = cx.number(next_index);
                RegExpObject::maybe_fast_set_last_index(cx, regexp_object, next_index_value)?;
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

        let flags = GenericFlags::new(cx, regexp_object)?;
        let flags_string = flags.to_flags_string(cx)?;

        let matcher =
            construct(cx, constructor, &[regexp_object.into(), flags_string.into()], None)?;

        let last_index = RegExpObject::maybe_fast_last_index_as_length(cx, regexp_object)?;
        let last_index_value = cx.number(last_index);

        RegExpObject::maybe_fast_set_last_index(cx, matcher, last_index_value)?;

        let is_global = flags.is_global()?;
        let is_unicode = flags.has_any_unicode_flag()?;

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

        // Get flags, determining if RegExp is unicode or global
        let flags = GenericFlags::new(cx, regexp_object)?;
        let is_unicode = flags.has_any_unicode_flag()?;
        let is_global = flags.is_global()?;

        if is_global {
            RegExpObject::maybe_fast_set_last_index(cx, regexp_object, cx.zero())?;
        }

        // Key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        enum MatchKind {
            Raw(Match),
            Object(Handle<ObjectValue>),
        }

        let mut exec_results = vec![];

        loop {
            // Search target string, finding all matches if global
            let exec_result =
                regexp_exec(cx, regexp_object, target_string, "RegExp.prototype[@@replace]")?;

            let exec_result = match exec_result {
                ExecResult::NoMatch => break,
                // Any matching named capture groups requires taking the slow path.
                ExecResult::Match(matched_regexp, match_)
                    if matched_regexp.compiled_regexp().has_named_capture_groups
                        && replace_value.may_reference_named_captures()? =>
                {
                    let result = build_match_object(cx, matched_regexp, target_string, &match_)?;
                    MatchKind::Object(result)
                }
                ExecResult::Match(_, match_) => MatchKind::Raw(match_),
                ExecResult::Object(exec_result) => MatchKind::Object(exec_result),
            };

            if !is_global {
                exec_results.push(exec_result);
                break;
            }

            // Extract matched string to determine whether the match was empty
            let is_empty_match = match &exec_result {
                MatchKind::Raw(match_) => match_.full_capture().is_empty(),
                MatchKind::Object(exec_result) => {
                    key.replace(PropertyKey::array_index(cx, 0)?);
                    let matched_value = get(cx, *exec_result, key)?;
                    to_string(cx, matched_value)?.is_empty()
                }
            };

            exec_results.push(exec_result);

            // If matched string is empty then increment last index
            if is_empty_match {
                let this_index = RegExpObject::maybe_fast_last_index_as_length(cx, regexp_object)?;

                let next_index = advance_u64_string_index(target_string, this_index, is_unicode)?;
                let next_index_value = cx.number(next_index);
                RegExpObject::maybe_fast_set_last_index(cx, regexp_object, next_index_value)?;
            }
        }

        let mut string_parts = vec![];
        let mut next_source_position = 0;

        // Cached substitution templates
        let mut cached_substitution_templates = ParsedSubstitutionTemplateCache::new();

        for exec_result in exec_results {
            let Replacement { replacement_string, matched_position, matched_length } =
                match exec_result {
                    MatchKind::Raw(match_) => replacement_for_raw_match(
                        cx,
                        &match_,
                        target_string,
                        replace_value,
                        &mut cached_substitution_templates,
                    )?,
                    MatchKind::Object(exec_result) => replacement_for_match_object(
                        cx,
                        exec_result,
                        target_string,
                        replace_value,
                        key,
                        &mut cached_substitution_templates,
                    )?,
                };

            // Add unchanged part between matches, then replacement for match
            if matched_position >= next_source_position {
                let unchanged_part = target_string
                    .substring(cx, next_source_position, matched_position)?
                    .as_string();

                string_parts.push(unchanged_part);
                string_parts.push(replacement_string);

                next_source_position = matched_position + matched_length;
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
        let previous_last_index = RegExpObject::maybe_fast_last_index(cx, regexp_object)?;
        if !previous_last_index.is_positive_zero() {
            let zero_value = cx.zero();
            RegExpObject::maybe_fast_set_last_index(cx, regexp_object, zero_value)?;
        }

        // Perform RegExp search
        let exec_result =
            regexp_exec(cx, regexp_object, string_value, "RegExp.prototype[@@search]")?;

        // Restore original last index
        let current_last_index = RegExpObject::maybe_fast_last_index(cx, regexp_object)?;
        if !same_value(current_last_index, previous_last_index)? {
            RegExpObject::maybe_fast_set_last_index(cx, regexp_object, previous_last_index)?;
        }

        // Return index of the match, or -1 if no match was found
        match exec_result {
            ExecResult::NoMatch => Ok(cx.negative_one()),
            ExecResult::Match(_, match_) => Ok(cx.number(match_.full_capture().start)),
            ExecResult::Object(exec_result) => get(cx, exec_result, cx.names.index()),
        }
    }}

    runtime_fn! {
    /// get RegExp.prototype.source (https://tc39.es/ecma262/#sec-get-regexp.prototype.source)
    fn source(cx, this_value, _) {
        if this_value.is_object() {
            let this_object = this_value.as_object();
            if let Some(regexp_object) = this_object.as_opt::<RegExpObject>() {
                return Ok(regexp_object.escaped_pattern_source().as_value());
            } else if cx.is_intrinsic(*this_object, Intrinsic::RegExpPrototype) {
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

        // Get flags, determining if a unicode flag is set
        let flags = GenericFlags::new(cx, regexp_object)?;
        let is_unicode = flags.has_any_unicode_flag()?;

        // Make sure the sticky flag is included in the flags string
        let flags_with_sticky = flags.with_sticky_flag(cx)?;
        let flags_string = flags_with_sticky.to_flags_string(cx)?;

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
            if exec_result.is_match() {
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
            RegExpObject::maybe_fast_set_last_index(cx, splitter, q_value)?;

            enum MatchKind {
                Raw(Match),
                Object(Handle<ObjectValue>),
            }

            // Execute RegExp at current index, advancing to next index if there is no match
            let exec_result = regexp_exec(cx, splitter, string_value, "RegExp.prototype[@@split]")?;

            let captures = match exec_result {
                ExecResult::NoMatch => {
                    q = advance_string_index(string_value, q, is_unicode)?;
                    continue;
                }
                ExecResult::Match(_, match_) => MatchKind::Raw(match_),
                ExecResult::Object(exec_result) => MatchKind::Object(exec_result),
            };

            // Otherwise there was a match so determine end of match
            let e = RegExpObject::maybe_fast_last_index_as_length(cx, splitter)?;
            let e = u64::min(e, size as u64) as u32;

            // If there was a match but it is empty then advance to next index
            if e == p {
                q = advance_string_index(string_value, q, is_unicode)?;
                continue;
            }

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

            macro_rules! set_capture {
                ($capture_string:expr) => {
                    key.replace(PropertyKey::array_index(cx, array_length)?);
                    create_data_property_or_throw(cx, result_array, key, $capture_string)?;

                    // Check if we have hit split limit
                    array_length += 1;
                    if array_length == limit {
                        return Ok(result_array.as_value());
                    }
                }
            }

            // Add capture groups to the result array
            match captures {
                MatchKind::Raw(match_) => {
                    for capture in &match_.capture_groups[1..] {
                        // Extract captured substring from the original string
                        let next_capture = match capture {
                            None => cx.undefined(),
                            Some(capture) => string_value
                                .substring(cx, capture.start, capture.end)?
                                .as_string()
                                .into(),
                        };

                        set_capture!(next_capture);
                    }
                }
                MatchKind::Object(exec_result) => {
                    let number_of_captures = length_of_array_like(cx, exec_result)?;
                    let number_of_captures = number_of_captures.saturating_sub(1);

                    for i in 1..=number_of_captures {
                        key.replace(PropertyKey::from_u64(cx, i)?);
                        let next_capture = get(cx, exec_result, key)?;

                        set_capture!(next_capture);
                    }
                }
            }

            q = p;
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

        Ok(cx.bool(exec_result.is_match()))
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

            RegExpSource::CompiledRegExp(pattern_regexp_object.compiled_regexp())
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
        } else if cx.is_intrinsic(*this_object, Intrinsic::RegExpPrototype) {
            return Ok(cx.undefined());
        }
    }

    type_error(cx, &format!("{method_name} must be called on a RegExp"))
}

/// Abstraction over various sources of RegExp flags for generic access
#[derive(Clone, Copy)]
pub enum GenericFlags {
    /// Raw flags read directly from a RegExp.
    Raw(RegExpFlags),
    /// The string returned by the `flags` getter.
    String(Handle<StringValue>),
}

impl GenericFlags {
    /// Get the flags for a RegExp-like object, taking the fast path for raw flags access if
    /// possible.
    fn new(cx: Context, object: Handle<ObjectValue>) -> EvalResult<GenericFlags> {
        if let Some(flags) = FastRegExpProtoGuard::try_get_fast_flags(cx, object)? {
            return Ok(GenericFlags::Raw(flags));
        }

        // Slow path, fall back to calling `flags` getter
        let flags_value = get(cx, object, cx.names.flags())?;
        let flags_string = to_string(cx, flags_value)?;

        Ok(GenericFlags::String(flags_string))
    }

    /// Get the flags for a RegExp-like object, additionally checking that the value returned by the
    /// slow path `flags` getter is not nullish.
    pub fn new_require_coercible(
        cx: Context,
        object: Handle<ObjectValue>,
    ) -> EvalResult<GenericFlags> {
        if let Some(flags) = FastRegExpProtoGuard::try_get_fast_flags(cx, object)? {
            return Ok(GenericFlags::Raw(flags));
        }

        // Slow path, fall back to calling `flags` getter
        let flags_value = get(cx, object, cx.names.flags())?;
        require_object_coercible(cx, flags_value)?;
        let flags_string = to_string(cx, flags_value)?;

        Ok(GenericFlags::String(flags_string))
    }

    pub fn is_global(&self) -> AllocResult<bool> {
        match self {
            Self::Raw(flags) => Ok(flags.is_global()),
            Self::String(flags_string) => flags_string_contains(*flags_string, 'g' as u32),
        }
    }

    fn has_any_unicode_flag(&self) -> AllocResult<bool> {
        match self {
            Self::Raw(flags) => Ok(flags.has_any_unicode_flag()),
            Self::String(flags_string) => Ok(flags_string_contains(*flags_string, 'u' as u32)?
                || flags_string_contains(*flags_string, 'v' as u32)?),
        }
    }

    /// The flags as a string, matching what the `flags` getter would have returned.
    fn to_flags_string(self, cx: Context) -> EvalResult<Handle<StringValue>> {
        match self {
            Self::Raw(flags) => flags_to_string_value(cx, flags),
            Self::String(flags_string) => Ok(flags_string),
        }
    }

    /// Return the same flags but with the sticky flag set.
    fn with_sticky_flag(&self, mut cx: Context) -> EvalResult<GenericFlags> {
        match self {
            Self::Raw(flags) => Ok(Self::Raw(*flags | RegExpFlags::STICKY)),
            Self::String(flags_string) => {
                if flags_string_contains(*flags_string, 'y' as u32)? {
                    return Ok(Self::String(*flags_string));
                }

                // Sticky flag is last in a flags string
                let y_string = cx.alloc_static_string("y")?;
                let new_flags_string = StringValue::concat(cx, *flags_string, y_string)?;

                Ok(Self::String(new_flags_string))
            }
        }
    }
}

pub fn flags_string_contains(
    flags_string: Handle<StringValue>,
    flag: CodePoint,
) -> AllocResult<bool> {
    Ok(flags_string.iter_code_points()?.any(|c| c == flag))
}

fn flags_to_string_value(mut cx: Context, flags: RegExpFlags) -> EvalResult<Handle<StringValue>> {
    if flags.is_empty() {
        return Ok(cx.names.empty_string().as_string());
    }

    cx.alloc_string(&flags.to_string())
}

/// The replacement for a single match in `RegExp.prototype[@@replace]`, along with the position
/// and length of the substring of the target string that it replaces.
struct Replacement {
    replacement_string: Handle<StringValue>,
    matched_position: u32,
    matched_length: u32,
}

/// Compute a single `RegExp.prototype[@@replace]` replacement for a raw match.
///
/// Cannot have named capture groups.
fn replacement_for_raw_match(
    cx: Context,
    match_: &Match,
    target_string: Handle<StringValue>,
    replace_value: ReplaceValue,
    cached_substitution_templates: &mut ParsedSubstitutionTemplateCache,
) -> EvalResult<Replacement> {
    let full_capture = match_.full_capture();

    // Extract the matched string
    let matched_string = target_string
        .substring(cx, full_capture.start, full_capture.end)?
        .as_string();

    // Extract the position of the matched string
    let matched_position = full_capture.start;
    let matched_length = full_capture.end - full_capture.start;

    // All captures (excluding the implicit full match capture)
    let captures = &match_.capture_groups[1..];

    let replacement_string = match replace_value {
        ReplaceValue::Function(replacer_function) => {
            // Construct arguments for replacer function. Matched substrings can be reconstructed
            // from capture bounds.
            let mut replacer_args = vec![matched_string.into()];
            for capture in captures {
                let capture_value = match capture {
                    None => cx.undefined(),
                    Some(capture) => target_string
                        .substring(cx, capture.start, capture.end)?
                        .as_string()
                        .into(),
                };
                replacer_args.push(capture_value);
            }

            replacer_args.push(cx.number(matched_position));
            replacer_args.push(target_string.into());

            // No named capture groups can appear, so always exclude the `groups` argument

            // Call replacer function and return string
            let replacement_value =
                call_object(cx, replacer_function, cx.undefined(), &replacer_args)?;

            to_string(cx, replacement_value)?
        }
        ReplaceValue::String(replace_string) => {
            // Named captures are not allowed since there are no named groups
            let substitution_template =
                cached_substitution_templates.get(cx, replace_string, false)?;

            // Create matched capture strings for all captures
            let mut indexed_captures = Vec::with_capacity(captures.len());
            for (i, capture) in captures.iter().enumerate() {
                let capture_string = match capture {
                    Some(capture)
                        if substitution_template.references_capture(i + 1, captures.len()) =>
                    {
                        Some(
                            target_string
                                .substring(cx, capture.start, capture.end)?
                                .as_string(),
                        )
                    }
                    _ => None,
                };
                indexed_captures.push(capture_string);
            }

            // Apply substitution template
            substitution_template.get_substitution(
                cx,
                target_string,
                matched_string,
                matched_position,
                &indexed_captures,
                None,
            )?
        }
    };

    Ok(Replacement { replacement_string, matched_position, matched_length })
}

/// Compute a single `RegExp.prototype[@@replace]` replacement for a match result object.
fn replacement_for_match_object(
    cx: Context,
    exec_result: Handle<ObjectValue>,
    target_string: Handle<StringValue>,
    replace_value: ReplaceValue,
    mut key: Handle<PropertyKey>,
    cached_substitution_templates: &mut ParsedSubstitutionTemplateCache,
) -> EvalResult<Replacement> {
    let result_length = length_of_array_like(cx, exec_result)?;
    let num_captures = result_length.saturating_sub(1);

    // Extract the matched string
    key.replace(PropertyKey::array_index(cx, 0)?);
    let matched_value = get(cx, exec_result, key)?;
    let matched_string = to_string(cx, matched_value)?;

    // Extract the position of the matched string
    let matched_position = get(cx, exec_result, cx.names.index())?;
    let matched_position = to_integer_or_infinity(cx, matched_position)?;
    let matched_position = f64::clamp(matched_position, 0.0, target_string.len() as f64) as u32;

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
            let mut replacer_args = vec![matched_string.into()];
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

            let allow_named_captures = named_captures.is_some();
            let substitution_template =
                cached_substitution_templates.get(cx, replace_string, allow_named_captures)?;

            // Apply substitution template
            substitution_template.get_substitution(
                cx,
                target_string,
                matched_string,
                matched_position,
                &indexed_captures,
                named_captures,
            )?
        }
    };

    Ok(Replacement {
        replacement_string,
        matched_position,
        matched_length: matched_string.len() as u32,
    })
}

/// Result of RegExpExec. Returns the raw match for use (without converting to an object) where
/// possible. Still must support an arbitrary user-defined `exec` that returns a full object.
pub enum ExecResult {
    /// There was no match.
    NoMatch,
    /// A raw match found for the given RegExp.
    Match(Handle<RegExpObject>, Match),
    /// The object returned by a user-provided `exec` method.
    Object(Handle<ObjectValue>),
}

impl ExecResult {
    fn is_match(&self) -> bool {
        !matches!(self, ExecResult::NoMatch)
    }

    /// Return the result of RegExpExec converted to a value: a match object or null.
    pub fn to_value(
        self,
        cx: Context,
        string_value: Handle<StringValue>,
    ) -> EvalResult<Handle<Value>> {
        match self {
            ExecResult::NoMatch => Ok(cx.null()),
            ExecResult::Match(regexp_object, match_) => {
                Ok(build_match_object(cx, regexp_object, string_value, &match_)?.as_value())
            }
            ExecResult::Object(exec_result) => Ok(exec_result.as_value()),
        }
    }
}

/// RegExpExec (https://tc39.es/ecma262/#sec-regexpexec)
///
/// Returns the raw match instead of a full match object when possible.
pub fn regexp_exec(
    cx: Context,
    regexp_object: Handle<ObjectValue>,
    string_value: Handle<StringValue>,
    method_name: &str,
) -> EvalResult<ExecResult> {
    // If the `exec` property is known to be the builtin `RegExp.prototype.exec` then we can skip
    // looking it up at all, and perform RegExpBuiltinExec directly.
    if FastRegExpProtoGuard::has_fast_builtin_exec(cx, regexp_object)? {
        return regexp_builtin_exec(cx, regexp_object.cast::<RegExpObject>(), string_value);
    }

    let exec = get(cx, regexp_object, cx.names.exec())?;

    // If the `exec` property is the builtin `RegExp.prototype.exec` then we can skip the call and
    // perform RegExpBuiltinExec directly.
    if is_builtin_regexp_exec(cx, exec)
        && let Some(regexp_object) = regexp_object.as_opt::<RegExpObject>()
    {
        return regexp_builtin_exec(cx, regexp_object, string_value);
    }

    // Otherwise is a user-defined `exec` method, so we must call it and handle the result.
    if is_callable(exec) {
        let exec_result = call(cx, exec, regexp_object.into(), &[string_value.into()])?;
        if exec_result.is_null() {
            return Ok(ExecResult::NoMatch);
        } else if !exec_result.is_object() {
            return type_error(cx, &format!("{method_name} `exec` must return null or an object"));
        }

        return Ok(ExecResult::Object(exec_result.as_object()));
    }

    // No `exec` method so perform RegExpBuiltinExec directly.
    let Some(regexp_object) = regexp_object.as_opt::<RegExpObject>() else {
        return type_error(cx, &format!("{method_name} must be called on a RegExp"));
    };

    regexp_builtin_exec(cx, regexp_object, string_value)
}

/// Whether the value is the builtin `RegExp.prototype.exec` function of the current realm.
///
/// Realm must match since RegExpBuiltinExec creates the match result array in the realm of the
/// `exec` function that is called.
fn is_builtin_regexp_exec(cx: Context, value: Handle<Value>) -> bool {
    is_builtin_function(*value, RuntimeFunction::RegExpPrototype_exec, Some(cx.current_realm_ptr()))
}

/// RegExpBuiltinExec (https://tc39.es/ecma262/#sec-regexpbuiltinexec)
///
/// Returns the raw match directly instead of converting it to a match object.
fn regexp_builtin_exec(
    cx: Context,
    regexp_object: Handle<RegExpObject>,
    string_value: Handle<StringValue>,
) -> EvalResult<ExecResult> {
    let compiled_regexp = regexp_object.compiled_regexp();
    let string_length = string_value.len();

    let mut last_index = RegExpObject::maybe_fast_last_index_as_length(cx, regexp_object.into())?;

    let flags = regexp_object.flags();
    let is_global = flags.is_global();
    let is_sticky = flags.is_sticky();

    if !is_global && !is_sticky {
        last_index = 0;
    }

    // Check if last index is already out of range meaning the match will always fail, resetting
    // last index under certain flags.
    if last_index > string_length as u64 {
        if is_global || is_sticky {
            RegExpObject::maybe_fast_set_last_index(cx, regexp_object.into(), cx.zero())?;
        }

        return Ok(ExecResult::NoMatch);
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
    let Some(match_) = match_ else {
        if is_global || is_sticky {
            RegExpObject::maybe_fast_set_last_index(cx, regexp_object.into(), cx.zero())?;
        }

        return Ok(ExecResult::NoMatch);
    };

    // Update last index to point past end of capture
    if is_global || is_sticky {
        let last_index_value = cx.number(match_.full_capture().end);
        RegExpObject::maybe_fast_set_last_index(cx, regexp_object.into(), last_index_value)?;
    }

    Ok(ExecResult::Match(regexp_object, match_))
}

/// Build the match object for a raw match found by RegExpExec.
fn build_match_object(
    cx: Context,
    regexp_object: Handle<RegExpObject>,
    string_value: Handle<StringValue>,
    match_: &Match,
) -> EvalResult<Handle<ObjectValue>> {
    let compiled_regexp = regexp_object.compiled_regexp();
    let capture_groups = &match_.capture_groups;
    let full_capture = match_.full_capture();
    let has_indices = regexp_object.flags().has_indices();

    // Build result array of matches
    let realm = cx.current_realm();
    let mut result_array = must!(array_create_in_realm(
        cx,
        realm,
        capture_groups.len() as u64,
        ArrayCreateShape::Common(CommonShape::RegExpMatch)
    ))
    .as_object();

    // Match result always has:
    // - `index` property which marks the start of the full match
    // - `input` property which contains the original string
    // - `groups` property which contains named capture groups
    let index_value = cx.number(full_capture.start);
    let named_groups_object = if compiled_regexp.has_named_capture_groups {
        ordinary_object_create_without_proto(cx)?.into()
    } else {
        cx.undefined()
    };

    result_array.init_inline_properties(&[index_value, string_value.into(), named_groups_object]);

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

        create_dense_data_property(cx, result_array, i as u64, captured_value)?;

        // Add capture indices to indices array if present
        let match_index_pair = if let Some((indices_array, indices_groups)) = indices_result {
            let match_index_pair = if let Some(capture) = capture {
                let start_index = cx.number(capture.start);
                let end_index = cx.number(capture.end);
                create_array_from_list(cx, &[start_index, end_index])?.into()
            } else {
                cx.undefined()
            };

            create_dense_data_property(cx, indices_array, i as u64, match_index_pair)?;
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

    Ok(result_array)
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

/// Substitution templates may be parsed differently depending on if named capture groups are
/// allowed, so we cache both versions.
struct ParsedSubstitutionTemplateCache {
    with_named_groups: Option<SubstitutionTemplate>,
    without_named_groups: Option<SubstitutionTemplate>,
}

impl ParsedSubstitutionTemplateCache {
    fn new() -> Self {
        Self { with_named_groups: None, without_named_groups: None }
    }

    fn cache(
        cache: &mut Option<SubstitutionTemplate>,
        cx: Context,
        template_string: Handle<StringValue>,
        allow_named_groups: bool,
    ) -> AllocResult<&SubstitutionTemplate> {
        if cache.is_none() {
            *cache = Some(
                SubstitutionTemplateParser::new(allow_named_groups).parse(cx, template_string)?,
            );
        }

        Ok(cache.as_ref().unwrap())
    }

    fn get(
        &mut self,
        cx: Context,
        template_string: Handle<StringValue>,
        allow_named_groups: bool,
    ) -> AllocResult<&SubstitutionTemplate> {
        if allow_named_groups {
            Self::cache(&mut self.with_named_groups, cx, template_string, true)
        } else {
            Self::cache(&mut self.without_named_groups, cx, template_string, false)
        }
    }
}
