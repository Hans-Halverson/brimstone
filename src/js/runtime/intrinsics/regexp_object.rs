use std::mem::size_of;

use crate::{
    extend_object, must, must_a,
    parser::regexp::RegExpFlags,
    runtime::{
        Context, HeapItemKind, HeapPtr, PropertyDescriptor,
        abstract_operations::{define_property_or_throw, set},
        alloc_error::AllocResult,
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        regexp::compiled_regexp::CompiledRegExp,
        string_value::StringValue,
    },
    set_uninit,
};

extend_object! {
    /// RegExp (Regular Expression) Objects (https://tc39.es/ecma262/#sec-regexp-regular-expression-objects)
    pub struct RegExpObject {
        compiled_regexp: HeapPtr<CompiledRegExp>,
    }
}

impl RegExpObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<RegExpObject>> {
        let mut object = object_create_from_constructor::<RegExpObject>(
            cx,
            constructor,
            HeapItemKind::RegExpObject,
            Intrinsic::RegExpPrototype,
        )?;

        // Initialize with default values as allocation may occur before real values are set, so
        // we must ensure the RegExpObject is in a valid state.
        set_uninit!(object.compiled_regexp, HeapPtr::uninit());

        let object = object.to_handle();

        Self::define_last_index_property(cx, object)?;

        Ok(object)
    }

    pub fn new_from_compiled_regexp(
        cx: Context,
        compiled_regexp: Handle<CompiledRegExp>,
    ) -> EvalResult<Handle<RegExpObject>> {
        let regexp_constructor = cx.get_intrinsic(Intrinsic::RegExpConstructor);
        let mut object = must!(object_create_from_constructor::<RegExpObject>(
            cx,
            regexp_constructor,
            HeapItemKind::RegExpObject,
            Intrinsic::RegExpPrototype
        ));

        set_uninit!(object.compiled_regexp, *compiled_regexp);

        let object = object.to_handle();

        Self::define_last_index_property(cx, object)?;

        // Initialize last index property
        let zero_value = cx.zero();
        must!(set(cx, object.into(), cx.names.last_index(), zero_value, true));

        Ok(object)
    }

    fn define_last_index_property(
        cx: Context,
        regexp_object: Handle<RegExpObject>,
    ) -> AllocResult<()> {
        let last_index_desc = PropertyDescriptor::data(cx.undefined(), true, false, false);
        must_a!(define_property_or_throw(
            cx,
            regexp_object.into(),
            cx.names.last_index(),
            last_index_desc
        ));

        Ok(())
    }

    #[inline]
    pub fn compiled_regexp_ptr(&self) -> HeapPtr<CompiledRegExp> {
        self.compiled_regexp
    }

    #[inline]
    pub fn compiled_regexp(&self) -> Handle<CompiledRegExp> {
        self.compiled_regexp_ptr().to_handle()
    }

    #[inline]
    pub fn set_compiled_regexp(&mut self, compiled_regexp: HeapPtr<CompiledRegExp>) {
        self.compiled_regexp = compiled_regexp;
    }

    #[inline]
    pub fn flags(&self) -> RegExpFlags {
        self.compiled_regexp.flags
    }

    #[inline]
    pub fn escaped_pattern_source(&self) -> Handle<StringValue> {
        self.compiled_regexp.escaped_pattern_source()
    }
}

impl HeapItem for RegExpObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<RegExpObject>()
    }

    fn visit_pointers(mut regexp_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        regexp_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut regexp_object.compiled_regexp);
    }
}
