use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::{Handle, HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{
            object_create, object_create_from_constructor, object_create_with_proto,
        },
        realm::Realm,
        type_utilities::to_boolean,
        Context, HeapPtr, Value,
    },
    maybe, set_uninit,
};

use super::intrinsics::Intrinsic;

// Boolean Objects (https://tc39.es/ecma262/#sec-boolean-objects)
extend_object! {
    pub struct BooleanObject {
        // The boolean value wrapped by this object
        boolean_data: bool,
    }
}

impl BooleanObject {
    pub fn new(cx: Context, boolean_data: bool) -> Handle<BooleanObject> {
        let mut object = object_create::<BooleanObject>(
            cx,
            ObjectKind::BooleanObject,
            Intrinsic::BooleanPrototype,
        );

        set_uninit!(object.boolean_data, boolean_data);

        object.to_handle()
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        boolean_data: bool,
    ) -> EvalResult<Handle<BooleanObject>> {
        let mut object = maybe!(object_create_from_constructor::<BooleanObject>(
            cx,
            constructor,
            ObjectKind::BooleanObject,
            Intrinsic::BooleanPrototype
        ));

        set_uninit!(object.boolean_data, boolean_data);

        object.to_handle().into()
    }

    pub fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        boolean_data: bool,
    ) -> Handle<BooleanObject> {
        let mut object =
            object_create_with_proto::<BooleanObject>(cx, ObjectKind::BooleanObject, proto);

        set_uninit!(object.boolean_data, boolean_data);

        object.to_handle()
    }

    pub fn boolean_data(&self) -> bool {
        self.boolean_data
    }

    pub fn set_boolean_data(&mut self, boolean_data: bool) {
        self.boolean_data = boolean_data;
    }
}

pub struct BooleanConstructor;

impl BooleanConstructor {
    /// Properties of the Boolean Constructor (https://tc39.es/ecma262/#sec-properties-of-the-boolean-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.boolean(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::BooleanPrototype).into(),
        );

        func
    }

    /// Boolean (https://tc39.es/ecma262/#sec-boolean-constructor-boolean-value)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let bool_value = to_boolean(get_argument(cx, arguments, 0).get());

        match new_target {
            None => cx.bool(bool_value).into(),
            Some(new_target) => {
                maybe!(BooleanObject::new_from_constructor(cx, new_target, bool_value)).into()
            }
        }
    }
}

impl HeapObject for HeapPtr<BooleanObject> {
    fn byte_size(&self) -> usize {
        size_of::<BooleanObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}
