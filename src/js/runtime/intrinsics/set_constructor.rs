use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        get,
        iterator::iter_iterator_values,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_ordinary_init_from_constructor,
        property::Property,
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
extend_object! {
    pub struct SetObject {
        set_data: ValueSet,
    }
}

impl SetObject {
    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Gc<ObjectValue>,
    ) -> EvalResult<Gc<SetObject>> {
        let mut object = cx.heap.alloc_uninit::<SetObject>();
        object.descriptor = cx.base_descriptors.get(ObjectKind::SetObject);

        maybe!(object_ordinary_init_from_constructor(
            cx,
            object.object(),
            constructor,
            Intrinsic::SetPrototype
        ));

        object.set_data = ValueSet::new();

        object.into()
    }

    pub fn set_data(&mut self) -> &mut ValueSet {
        &mut self.set_data
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

        let set_object: Gc<ObjectValue> =
            maybe!(SetObject::new_from_constructor(cx, new_target)).into();

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
