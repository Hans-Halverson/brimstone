use crate::{
    extend_object, must_a,
    parser::regexp::RegExpFlags,
    runtime::{
        Context, HeapPtr, PropertyDescriptor, PropertyFlags, Value,
        abstract_operations::define_property_or_throw,
        alloc_error::AllocResult,
        common_shapes::CommonShape,
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::{ObjectBuilder, get_prototype_from_constructor},
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
        let proto = get_prototype_from_constructor(cx, constructor, Intrinsic::RegExpPrototype)?;

        Ok(Self::new_with_proto(cx, proto, cx.undefined())?)
    }

    pub fn new_from_compiled_regexp(
        cx: Context,
        compiled_regexp: Handle<CompiledRegExp>,
    ) -> EvalResult<Handle<RegExpObject>> {
        let proto = cx.get_intrinsic(Intrinsic::RegExpPrototype);
        let mut object = Self::new_with_proto(cx, proto, cx.zero())?;

        object.set_compiled_regexp(*compiled_regexp);

        Ok(object)
    }

    /// Create a RegExp object with the given prototype, defining the `lastIndex` property on it.
    fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        last_index: Handle<Value>,
    ) -> AllocResult<Handle<RegExpObject>> {
        // Fast path using common shape for RegExps created with this realm's `RegExp.prototype`
        let has_common_shape = cx.is_intrinsic(*proto, Intrinsic::RegExpPrototype);

        let builder = ObjectBuilder::<RegExpObject>::new(cx);
        let mut object = if has_common_shape {
            builder.common_shape(CommonShape::RegExp)?.build()?
        } else {
            builder.proto(proto).build()?
        };

        // Compiled RegExp is set after creation
        set_uninit!(object.compiled_regexp, HeapPtr::uninit());

        let object = object.to_handle();

        // Fast path for common shapes
        if has_common_shape {
            object.as_object().init_inline_properties(&[last_index]);
        } else {
            let last_index_desc =
                PropertyDescriptor::data(last_index, PropertyFlags::empty().writable());
            must_a!(define_property_or_throw(
                cx,
                object.into(),
                cx.names.last_index(),
                last_index_desc
            ));
        }

        Ok(object)
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
    fn byte_size(regexp_object: HeapPtr<Self>) -> usize {
        regexp_object.object_byte_size()
    }

    fn visit_pointers(mut regexp_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        regexp_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut regexp_object.compiled_regexp);
    }
}
