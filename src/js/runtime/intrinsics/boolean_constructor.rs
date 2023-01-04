use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{ordinary_create_from_constructor, OrdinaryObject},
        property::Property,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::to_boolean,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

#[repr(C)]
pub struct BooleanObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    value: bool,
}

impl GcDeref for BooleanObject {}

impl_gc_into!(BooleanObject, ObjectValue);

// 20.3 Boolean Objects
impl BooleanObject {
    const VTABLE: *const () = extract_object_vtable::<BooleanObject>();

    pub fn new(object: OrdinaryObject, value: bool) -> BooleanObject {
        BooleanObject {
            _vtable: Self::VTABLE,
            object,
            value,
        }
    }

    pub fn value(&self) -> bool {
        self.value
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
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
        let mut func =
            BuiltinFunction::create(cx, Self::construct, 1, "Boolean", Some(realm), None, None);

        func.set_is_constructor();
        func.set_property(
            "prototype".to_owned(),
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
                let object = maybe!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::BooleanPrototype
                ));

                cx.heap.alloc(BooleanObject::new(object, bool_value)).into()
            }
        }
    }
}
