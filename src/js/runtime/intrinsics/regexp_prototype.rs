use crate::{
    js::{
        parser::regexp::RegExpFlags,
        runtime::{
            completion::EvalResult,
            error::type_error_,
            get,
            interned_strings::InternedStrings,
            object_value::ObjectValue,
            realm::Realm,
            string_value::StringValue,
            to_string,
            type_utilities::{same_object_value, to_boolean},
            Context, Handle, Value,
        },
    },
    maybe,
};

use super::{intrinsics::Intrinsic, regexp_constructor::RegExpObject};

pub struct RegExpPrototype;

impl RegExpPrototype {
    // 22.2.6 Properties of the RegExp Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once RegExpConstructor has been created
        object.intrinsic_getter(cx, cx.names.dot_all(), Self::dot_all, realm);
        object.intrinsic_getter(cx, cx.names.flags(), Self::flags, realm);
        object.intrinsic_getter(cx, cx.names.global(), Self::global, realm);
        object.intrinsic_getter(cx, cx.names.has_indices(), Self::has_indices, realm);
        object.intrinsic_getter(cx, cx.names.ignore_case(), Self::ignore_case, realm);
        object.intrinsic_getter(cx, cx.names.multiline(), Self::multiline, realm);
        object.intrinsic_getter(cx, cx.names.source(), Self::source, realm);
        object.intrinsic_getter(cx, cx.names.sticky(), Self::sticky, realm);
        object.intrinsic_getter(cx, cx.names.to_string(), Self::to_string, realm);
        object.intrinsic_getter(cx, cx.names.unicode(), Self::unicode, realm);

        object
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

        cx.alloc_string(&flags_string).into()
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
