use crate::{
    parser::regexp::RegExpFlags,
    runtime::{
        Context, EvalResult, Handle,
        accessor::Accessor,
        alloc_error::AllocResult,
        common_shapes::CommonShape,
        gc::HeapVisitor,
        intrinsics::{
            intrinsics::Intrinsic,
            regexp_object::RegExpObject,
            rust_runtime::{RuntimeFunction, is_builtin_function},
        },
        object_value::ObjectValue,
        shape::ValidityGuard,
    },
};

/// A guard that allows for cached access to information about the RegExp prototype object.
pub enum FastRegExpProtoGuard {
    /// Guard is lazily initialized.
    Uninitialized,
    /// Which properties of the RegExp prototype object are guaranteed to still be set to their
    /// builtin functions. Cached for as long as the validity guard is valid, meaning this realm's
    /// RegExp prototype is unchanged.
    Cached {
        builtin_properties: BuiltinProtoProperties,
        validity_guard: ValidityGuard,
    },
}

#[derive(Clone, Copy)]
pub struct BuiltinProtoProperties {
    /// Whether all of the flag accessors are set to their builtin functions.
    all_builtin_flag_getters: bool,
    /// Whether the `exec` method is set to its builtin function.
    has_builtin_exec: bool,
}

impl FastRegExpProtoGuard {
    fn all_builtin_flag_getters(&self) -> Option<bool> {
        Some(self.builtin_properties()?.all_builtin_flag_getters)
    }

    fn has_builtin_exec(&self) -> Option<bool> {
        Some(self.builtin_properties()?.has_builtin_exec)
    }

    fn builtin_properties(&self) -> Option<BuiltinProtoProperties> {
        match self {
            Self::Cached { builtin_properties, validity_guard } if validity_guard.is_valid() => {
                Some(*builtin_properties)
            }
            _ => None,
        }
    }

    /// Fast path for getting raw flags from a RegExp object.
    ///
    /// Fast path is used if the object has the RegExp common shape.
    pub fn try_get_fast_flags(
        cx: Context,
        object: Handle<ObjectValue>,
    ) -> EvalResult<Option<RegExpFlags>> {
        let realm = cx.current_realm_ptr();

        // Quick, conservative check to verify that the object
        // - Is a RegExpObject
        // - Has RegExp.prototype as its prototype
        // - Does not have any own properties that could shadow the flag getters on
        //   RegExp.prototype, since the common shape's only own property is `lastIndex`.
        if !realm.is_common_shape(object.shape_ptr(), CommonShape::RegExp) {
            return Ok(None);
        }

        let regexp_object = object.cast::<RegExpObject>();

        // Fast path is gated by a cached value protected by a validity guard on RegExp.prototype
        if let Some(all_builtin_flag_getters) =
            realm.regexp_proto_guard().all_builtin_flag_getters()
        {
            return Ok(all_builtin_flag_getters.then(|| regexp_object.flags()));
        }

        // Guard was invalid or uninitialized, so recompute the guarded values
        let all_builtin_flag_getters = Self::recompute(cx)?.all_builtin_flag_getters;

        Ok(all_builtin_flag_getters.then(|| regexp_object.flags()))
    }

    /// Fast path for looking up the `exec` method, returning whether the lookup is guaranteed to
    /// return the builtin `RegExp.prototype.exec` of the current realm.
    ///
    /// Fast path is used if the object has the RegExp common shape.
    pub fn has_fast_builtin_exec(cx: Context, object: Handle<ObjectValue>) -> EvalResult<bool> {
        let realm = cx.current_realm_ptr();

        // Quick, conservative check to verify that the object
        // - Is a RegExpObject
        // - Has RegExp.prototype as its prototype
        // - Does not have an own `exec` property that could shadow the one on RegExp.prototype,
        //   since the common shape's only own property is `lastIndex`.
        if !realm.is_common_shape(object.shape_ptr(), CommonShape::RegExp) {
            return Ok(false);
        }

        // Fast path is gated by a cached value protected by a validity guard on RegExp.prototype
        if let Some(has_builtin_exec) = realm.regexp_proto_guard().has_builtin_exec() {
            return Ok(has_builtin_exec);
        }

        // Guard was invalid or uninitialized, so recompute the guarded values
        Ok(Self::recompute(cx)?.has_builtin_exec)
    }

    /// Recompute the guarded properties of the RegExp prototype object, caching them protected by
    /// a new validity guard.
    fn recompute(cx: Context) -> AllocResult<BuiltinProtoProperties> {
        let mut regexp_prototype = cx.get_intrinsic(Intrinsic::RegExpPrototype);

        let all_builtin_flag_getters =
            Self::recompute_all_builtin_flag_getters(cx, regexp_prototype);
        let has_builtin_exec = Self::recompute_has_builtin_exec(cx, regexp_prototype);
        let builtin_properties =
            BuiltinProtoProperties { all_builtin_flag_getters, has_builtin_exec };

        // Fetch and create guard for the recomputed values
        let validity_guard = regexp_prototype.request_own_validity_guard(cx)?;

        // Requesting the guard allocates so refetch realm
        let mut realm = cx.current_realm_ptr();
        realm.set_regexp_proto_guard(FastRegExpProtoGuard::Cached {
            validity_guard,
            builtin_properties,
        });

        Ok(builtin_properties)
    }

    /// Recompute whether all of the builtin flag getters are present on RegExp.prototype.
    fn recompute_all_builtin_flag_getters(
        cx: Context,
        regexp_prototype: Handle<ObjectValue>,
    ) -> bool {
        let flag_getters = [
            (cx.names.flags(), RuntimeFunction::RegExpPrototype_flags),
            (cx.names.has_indices(), RuntimeFunction::RegExpPrototype_has_indices),
            (cx.names.global(), RuntimeFunction::RegExpPrototype_global),
            (cx.names.ignore_case(), RuntimeFunction::RegExpPrototype_ignore_case),
            (cx.names.multiline(), RuntimeFunction::RegExpPrototype_multiline),
            (cx.names.dot_all(), RuntimeFunction::RegExpPrototype_dot_all),
            (cx.names.unicode(), RuntimeFunction::RegExpPrototype_unicode),
            (cx.names.unicode_sets(), RuntimeFunction::RegExpPrototype_unicode_sets),
            (cx.names.sticky(), RuntimeFunction::RegExpPrototype_sticky),
        ];

        for (name, builtin) in flag_getters {
            // Own property with the given name exists
            let Some(property) = regexp_prototype.get_property(cx, name) else {
                return false;
            };

            // Property is a getter
            if !property.is_accessor() {
                return false;
            }

            let Some(getter) = Accessor::from_value(*property.value()).get else {
                return false;
            };

            // Getter is the expected builtin function. Flag getters do not depend on the realm.
            if !is_builtin_function(getter.into(), builtin, None) {
                return false;
            }
        }

        true
    }

    /// Recompute whether the builtin `exec` method is present on RegExp.prototype.
    fn recompute_has_builtin_exec(cx: Context, regexp_prototype: Handle<ObjectValue>) -> bool {
        // Own data property with the name `exec` exists
        let Some(property) = regexp_prototype.get_property(cx, cx.names.exec()) else {
            return false;
        };

        if property.is_accessor() {
            return false;
        }

        // Value is the expected builtin function. Realm must match since RegExpBuiltinExec creates
        // the match result array in the realm of the `exec` function that is called.
        is_builtin_function(
            *property.value(),
            RuntimeFunction::RegExpPrototype_exec,
            Some(cx.current_realm_ptr()),
        )
    }

    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        if let Self::Cached { validity_guard, .. } = self {
            validity_guard.visit_pointers(visitor);
        }
    }
}
