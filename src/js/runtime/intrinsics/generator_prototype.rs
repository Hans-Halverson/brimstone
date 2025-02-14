use crate::js::runtime::{
    abstract_operations::define_property_or_throw,
    bytecode::function::Closure,
    eval_result::EvalResult,
    function::get_argument,
    generator_object::{generator_resume, generator_resume_abrupt, GeneratorCompletionType},
    object_descriptor::ObjectKind,
    object_value::ObjectValue,
    ordinary_object::object_create,
    property::Property,
    realm::Realm,
    value::Value,
    Context, Handle, PropertyDescriptor,
};

use super::intrinsics::Intrinsic;

pub struct GeneratorPrototype;

impl GeneratorPrototype {
    /// The %GeneratorPrototype% Object (https://tc39.es/ecma262/#sec-properties-of-generator-prototype)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        // Constructor property is added once GeneratorFunctionPrototype is created

        object.intrinsic_func(cx, cx.names.next(), Self::next, 1, realm);
        object.intrinsic_func(cx, cx.names.return_(), Self::return_, 1, realm);
        object.intrinsic_func(cx, cx.names.throw(), Self::throw, 1, realm);

        // %GeneratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-generator.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.generator().as_string().into(), false, false, true),
        );

        object
    }

    /// %GeneratorPrototype%.next (https://tc39.es/ecma262/#sec-generator.prototype.next)
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        generator_resume(cx, this_value, value)
    }

    /// %GeneratorPrototype%.return (https://tc39.es/ecma262/#sec-generator.prototype.return)
    pub fn return_(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        generator_resume_abrupt(cx, this_value, value, GeneratorCompletionType::Return)
    }

    /// %GeneratorPrototype%.throw (https://tc39.es/ecma262/#sec-generator.prototype.throw)
    pub fn throw(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let error = get_argument(cx, arguments, 0);
        generator_resume_abrupt(cx, this_value, error, GeneratorCompletionType::Throw)
    }

    /// Every generator function has a prototype property referencing an instance of the generator
    /// prototype. Install this property on a generator function.
    pub fn install_on_generator_function(cx: Context, closure: Handle<Closure>) -> EvalResult<()> {
        let proto = object_create::<ObjectValue>(
            cx,
            ObjectKind::OrdinaryObject,
            Intrinsic::GeneratorPrototype,
        )
        .to_handle();

        let proto_desc = PropertyDescriptor::data(proto.to_handle().into(), true, false, false);
        define_property_or_throw(cx, closure.into(), cx.names.prototype(), proto_desc)?;

        Ok(())
    }
}
