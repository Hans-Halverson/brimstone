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
        value::{Value, ValueMap},
        Context,
    },
};

use super::intrinsics::Intrinsic;

// 24.1 Map Objects
#[repr(C)]
pub struct MapObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    map_data: ValueMap<Value>,
}

impl GcDeref for MapObject {}

impl_gc_into!(MapObject, ObjectValue);

impl MapObject {
    const VTABLE: *const () = extract_object_vtable::<MapObject>();

    pub fn new_from_value(cx: &mut Context) -> Gc<MapObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::MapPrototype);
        let object = ordinary_object_create(proto);

        let map_object = MapObject { _vtable: Self::VTABLE, object, map_data: ValueMap::new() };
        cx.heap.alloc(map_object)
    }

    pub fn map_data(&mut self) -> &mut ValueMap<Value> {
        &mut self.map_data
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
impl Object for MapObject {
    fn is_map_object(&self) -> bool {
        true
    }
}

pub struct MapConstructor;

impl MapConstructor {
    // 24.1.1 The Map Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            &cx.names.map(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::MapPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 24.1.1.1 Map
    fn construct(
        _: &mut Context,
        _: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        unimplemented!("Map constructor")
    }
}
