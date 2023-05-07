use crate::{
    js::runtime::{
        completion::EvalResult, error::type_error_, gc::HandleValue, object_value::ObjectValue,
        realm::Realm, Context, Handle,
    },
    maybe,
};

use super::{boolean_constructor::BooleanObject, intrinsics::Intrinsic};

pub struct BooleanPrototype;

impl BooleanPrototype {
    // 20.3.3 Properties of the Boolean Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let object = BooleanObject::new_with_proto(cx, object_proto, false);

        // Constructor property is added once BooleanConstructor has been created
        object
            .object()
            .intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        object
            .object()
            .intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        object.into()
    }

    // 20.3.3.2 Boolean.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let bool_value = maybe!(this_boolean_value(cx, this_value));
        let string_value = if bool_value { "true" } else { "false" };

        cx.alloc_string(String::from(string_value)).into()
    }

    // 20.3.3.3 Boolean.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let bool_value = maybe!(this_boolean_value(cx, this_value));
        bool_value.into()
    }
}

fn this_boolean_value(cx: &mut Context, value: HandleValue) -> EvalResult<bool> {
    if value.is_bool() {
        return value.as_bool().into();
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_bool_object() {
            return object_value.cast::<BooleanObject>().boolean_data().into();
        }
    }

    type_error_(cx, "value cannot be converted to boolean")
}
