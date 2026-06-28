use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{boolean_constructor::BooleanObject, intrinsics::Intrinsic},
        object_value::ObjectValue,
        realm::Realm,
    },
    runtime_fn,
};

pub struct BooleanPrototype;

impl BooleanPrototype {
    /// Properties of the Boolean Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-boolean-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let object = BooleanObject::new_with_proto(cx, object_proto, false)?.as_object();
        let mut builder = IntrinsicBuilder::new(cx, realm, object);

        // Constructor property is added once BooleanConstructor has been created
        intrinsic_methods!(cx, builder, {
            to_string BooleanPrototype_to_string (0),
            value_of  BooleanPrototype_value_of  (0),
        });

        builder.build()
    }

    runtime_fn! {
    /// Boolean.prototype.toString (https://tc39.es/ecma262/#sec-boolean.prototype.tostring)
    fn to_string(cx, this_value, _) {
        let bool_value = this_boolean_value(cx, this_value, "toString")?;
        let string_value = if bool_value { "true" } else { "false" };

        Ok(cx.alloc_string(string_value)?.as_value())
    }}

    runtime_fn! {
    /// Boolean.prototype.valueOf (https://tc39.es/ecma262/#sec-boolean.prototype.valueof)
    fn value_of(cx, this_value, _) {
        let bool_value = this_boolean_value(cx, this_value, "valueOf")?;
        Ok(cx.bool(bool_value))
    }}
}

fn this_boolean_value(cx: Context, value: Handle<Value>, method_name: &str) -> EvalResult<bool> {
    if value.is_bool() {
        return Ok(value.as_bool());
    }

    if value.is_object() {
        let object_value = value.as_object();
        if let Some(boolean_object) = object_value.as_boolean_object() {
            return Ok(boolean_object.boolean_data());
        }
    }

    type_error(cx, &format!("Boolean.prototype.{method_name} must be called on a boolean"))
}
