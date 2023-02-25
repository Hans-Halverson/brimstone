use crate::js::runtime::{
    completion::EvalResult,
    error::type_error_,
    function::get_argument,
    gc::Gc,
    object_value::{Object, ObjectValue},
    ordinary_object::OrdinaryObject,
    property::Property,
    property_key::PropertyKey,
    realm::Realm,
    value::Value,
    Context,
};

use super::{intrinsics::Intrinsic, set_constructor::SetObject};

pub struct SetPrototype;

impl SetPrototype {
    // 24.2.3 Properties of the Set Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once SetConstructor has been created
        object.intrinsic_func(cx, &cx.names.add(), Self::add, 1, realm);
        object.intrinsic_func(cx, &cx.names.clear(), Self::clear, 0, realm);
        object.intrinsic_func(cx, &cx.names.delete(), Self::delete, 1, realm);
        object.intrinsic_func(cx, &cx.names.has(), Self::has, 1, realm);
        object.intrinsic_getter(cx, &cx.names.size(), Self::size, realm);

        // 24.2.3.12 Set.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.set_property(
            &to_string_tag_key,
            Property::data(cx.names.set().as_string().into(), false, false, true),
        );

        cx.heap.alloc(object).into()
    }

    // 24.2.3.1 Set.prototype.add
    fn add(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "add method must be called on set");
        };

        // Convert negative zero to positive zero in set
        let mut value = get_argument(arguments, 0);
        if value.is_negative_zero() {
            value = Value::number(0.0);
        }

        set.set_data().insert(value);

        this_value.into()
    }

    // 24.2.3.2 Set.prototype.clear
    fn clear(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "clear method must be called on set");
        };

        set.set_data().clear();

        Value::undefined().into()
    }

    // 24.2.3.4 Set.prototype.delete
    fn delete(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "delete method must be called on set");
        };

        let key = get_argument(arguments, 0);
        let existed = set.set_data().remove(key);

        existed.into()
    }

    // 24.2.3.7 Set.prototype.has
    fn has(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "has method must be called on set");
        };

        let value = get_argument(arguments, 0);

        set.set_data().contains(value).into()
    }

    // 24.2.3.9 get Set.prototype.size
    fn size(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "size accessor must be called on set");
        };

        Value::from_u64(set.set_data().len() as u64).into()
    }
}

fn this_set_value(value: Value) -> Option<Gc<SetObject>> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_set_object() {
        return None;
    }

    Some(object.cast::<SetObject>())
}
