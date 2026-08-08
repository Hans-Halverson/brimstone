use crate::{
    parser::regexp::RegExpFlags,
    runtime::{
        Context, EvalResult, Handle,
        accessor::Accessor,
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

/// A guard that allows for fast access to the `flags` property of a RegExp object.
pub enum FastRegExpFlagsGuard {
    /// Guard is lazily initialized.
    Uninitialized,
    /// Whether the RegExp prototype object is guaranteed to have all `flags` accessors set to their
    /// builtin functions. Cached for as long as the validity guard is valid, meaning this realm's
    /// RegExp prototype is unchanged.
    Cached {
        all_builtin_flag_getters: bool,
        validity_guard: ValidityGuard,
    },
}

impl FastRegExpFlagsGuard {
    fn all_builtin_flag_getters(&self) -> Option<bool> {
        match self {
            Self::Cached { all_builtin_flag_getters, validity_guard }
                if validity_guard.is_valid() =>
            {
                Some(*all_builtin_flag_getters)
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
        if !realm
            .common_shapes
            .is_common_shape(object.shape_ptr(), CommonShape::RegExp)
        {
            return Ok(None);
        }

        let regexp_object = object.cast::<RegExpObject>();

        // Fast path is gated by a cached value protected by a validity guard on RegExp.prototype
        if let Some(all_builtin_flag_getters) =
            realm.regexp_flags_guard().all_builtin_flag_getters()
        {
            return Ok(all_builtin_flag_getters.then(|| regexp_object.flags()));
        }

        // Guard was invalid or uninitialized, so recompute the guarded value
        let mut regexp_prototype = cx.get_intrinsic(Intrinsic::RegExpPrototype);
        let all_builtin_flag_getters =
            Self::recompute_all_builtin_flag_getters(cx, regexp_prototype);

        // Fetch and create guard for the recomputed value
        let validity_guard = regexp_prototype.request_own_validity_guard(cx)?;

        // Requesting the guard allocates so refetch realm
        let mut realm = cx.current_realm_ptr();
        realm.set_regexp_flags_guard(FastRegExpFlagsGuard::Cached {
            all_builtin_flag_getters,
            validity_guard,
        });

        Ok(all_builtin_flag_getters.then(|| regexp_object.flags()))
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

    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        if let Self::Cached { validity_guard, .. } = self {
            validity_guard.visit_pointers(visitor);
        }
    }
}
