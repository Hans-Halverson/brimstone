use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        error::type_error_,
        function::get_argument,
        gc::{Gc, GcDeref},
        get,
        iterator::iter_iterator_values,
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{ordinary_create_from_constructor, OrdinaryObject},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::is_callable,
        value::{Value, ValueSet},
        Completion, Context,
    },
    maybe,
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

    pub fn new(cx: &mut Context, object: OrdinaryObject) -> Gc<SetObject> {
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

        let species_key = PropertyKey::symbol(cx.well_known_symbols.species);
        func.intrinsic_getter(cx, &species_key, Self::get_species, realm);

        func
    }

    // 24.2.1.1 Set
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error_(cx, "Set constructor must be called with new");
        };

        let object =
            maybe!(ordinary_create_from_constructor(cx, new_target, Intrinsic::SetPrototype));
        let set_object: Gc<ObjectValue> = SetObject::new(cx, object).into();

        let iterable = get_argument(arguments, 0);
        if iterable.is_nullish() {
            return set_object.into();
        }

        let adder = maybe!(get(cx, set_object, &cx.names.add()));
        if !is_callable(adder) {
            return type_error_(cx, "set must contain an add method");
        }

        let adder = adder.as_object();
        let set_value = set_object.into();

        let completion = iter_iterator_values(cx, iterable, &mut |cx, value| {
            let result = call_object(cx, adder, set_value, &[value]);
            match result {
                EvalResult::Ok(_) => None,
                EvalResult::Throw(thrown_value) => Some(Completion::throw(thrown_value)),
            }
        });

        maybe!(completion.into_eval_result());

        set_value.into()
    }

    // 24.2.2.2 get Set [ @@species ]
    fn get_species(
        _: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_value.into()
    }
}
