use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{ordinary_object_create, OrdinaryObject},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        value::{Value, ValueSet},
        Context,
    },
};

use super::intrinsics::Intrinsic;

// 24.2 Set Objects
#[repr(C)]
pub struct SetObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    set_data: ValueSet,
}

impl GcDeref for SetObject {}

impl_gc_into!(SetObject, ObjectValue);

impl SetObject {
    const VTABLE: *const () = extract_object_vtable::<SetObject>();

    pub fn new_from_value(cx: &mut Context) -> Gc<SetObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::SetPrototype);
        let object = ordinary_object_create(proto);

        let set_object = SetObject { _vtable: Self::VTABLE, object, set_data: ValueSet::new() };
        cx.heap.alloc(set_object)
    }

    pub fn set_data(&mut self) -> &mut ValueSet {
        &mut self.set_data
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
impl Object for SetObject {
    fn is_set_object(&self) -> bool {
        true
    }
}

pub struct SetConstructor;

impl SetConstructor {
    // 24.2.1 The Set Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            &cx.names.set(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::SetPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 24.2.1.1 Set
    fn construct(
        _: &mut Context,
        _: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        unimplemented!("Set constructor")
    }
}
