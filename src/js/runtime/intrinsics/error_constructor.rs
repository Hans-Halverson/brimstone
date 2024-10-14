use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::{create_non_enumerable_data_property_or_throw, get, has_property},
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        realm::Realm,
        stack_trace::attach_stack_trace_to_error,
        type_utilities::to_string,
        Context, Handle, HeapPtr, Value,
    },
};

use super::intrinsics::Intrinsic;

extend_object! {
    pub struct ErrorObject {}
}

impl ErrorObject {
    fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ErrorObject>> {
        let object = object_create_from_constructor::<ErrorObject>(
            cx,
            constructor,
            ObjectKind::ErrorObject,
            Intrinsic::ErrorPrototype,
        )?;

        Ok(object.to_handle())
    }
}

pub struct ErrorConstructor;

impl ErrorConstructor {
    /// Properties of the Error Constructor (https://tc39.es/ecma262/#sec-properties-of-the-error-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.error(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::ErrorPrototype).into(),
        );

        func
    }

    /// Error (https://tc39.es/ecma262/#sec-error-message)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            cx.current_function()
        };

        let object = ErrorObject::new_from_constructor(cx, new_target)?;

        let message = get_argument(cx, arguments, 0);
        if !message.is_undefined() {
            let message_string = to_string(cx, message)?;
            create_non_enumerable_data_property_or_throw(
                cx,
                object.into(),
                cx.names.message(),
                message_string.into(),
            );
        }

        attach_stack_trace_to_error(cx, object, /* skip_current_frame */ true);

        let options_arg = get_argument(cx, arguments, 1);
        install_error_cause(cx, object, options_arg)?;

        Ok(object.as_value())
    }
}

/// InstallErrorCause (https://tc39.es/ecma262/#sec-installerrorcause)
pub fn install_error_cause(
    cx: Context,
    object: Handle<ErrorObject>,
    options: Handle<Value>,
) -> EvalResult<()> {
    if options.is_object() {
        let options = options.as_object();
        if has_property(cx, options, cx.names.cause())? {
            let cause = get(cx, options, cx.names.cause())?;
            create_non_enumerable_data_property_or_throw(
                cx,
                object.into(),
                cx.names.cause(),
                cause,
            );
        }
    }

    Ok(())
}

impl HeapObject for HeapPtr<ErrorObject> {
    fn byte_size(&self) -> usize {
        size_of::<ErrorObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}
