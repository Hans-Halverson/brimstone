use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        function::get_argument,
        gc::Gc,
        object_descriptor::ObjectKind,
        object_value::{HasObject, Object, ObjectValue},
        ordinary_object::{object_ordinary_init, object_ordinary_init_from_constructor},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::to_boolean,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 20.3 Boolean Objects
extend_object! {
    pub struct BooleanObject {
        // The boolean value wrapped by this object
        boolean_data: bool,
    }
}

impl BooleanObject {
    pub fn new_with_proto(
        cx: &mut Context,
        proto: Gc<ObjectValue>,
        boolean_data: bool,
    ) -> Gc<BooleanObject> {
        let mut object = cx.heap.alloc_uninit::<BooleanObject>();
        object.descriptor = cx.base_descriptors.get(ObjectKind::BooleanObject);

        object_ordinary_init(object.object_mut(), proto);

        object.boolean_data = boolean_data;

        object
    }

    pub fn new_from_value(cx: &mut Context, boolean_data: bool) -> Gc<BooleanObject> {
        let proto = cx
            .current_realm()
            .get_intrinsic(Intrinsic::BooleanPrototype);

        Self::new_with_proto(cx, proto, boolean_data)
    }

    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Gc<ObjectValue>,
        boolean_data: bool,
    ) -> EvalResult<Gc<BooleanObject>> {
        let mut object = cx.heap.alloc_uninit::<BooleanObject>();
        object.descriptor = cx.base_descriptors.get(ObjectKind::BooleanObject);

        maybe!(object_ordinary_init_from_constructor(
            cx,
            object.object_mut(),
            constructor,
            Intrinsic::BooleanPrototype
        ));

        object.boolean_data = boolean_data;

        object.into()
    }

    pub fn boolean_data(&self) -> bool {
        self.boolean_data
    }
}

#[wrap_ordinary_object]
impl Object for BooleanObject {
    fn is_bool_object(&self) -> bool {
        true
    }
}

pub struct BooleanConstructor;

impl BooleanConstructor {
    // 20.3.2 Properties of the Boolean Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            &cx.names.boolean(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::BooleanPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 20.3.1.1 Boolean
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let bool_value = to_boolean(get_argument(arguments, 0));

        match new_target {
            None => bool_value.into(),
            Some(new_target) => {
                maybe!(BooleanObject::new_from_constructor(cx, new_target, bool_value)).into()
            }
        }
    }
}
