use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, HeapItemKind, PropertyDescriptor,
        abstract_operations::define_property_or_throw,
        alloc_error::AllocResult,
        bytecode::function::ClosureObject,
        eval_result::EvalResult,
        generator_object::{GeneratorCompletionType, generator_resume, generator_resume_abrupt},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create,
        realm::Realm,
    },
    runtime_fn,
};

pub struct GeneratorPrototype;

impl GeneratorPrototype {
    /// The %GeneratorPrototype% Object (https://tc39.es/ecma262/#sec-properties-of-generator-prototype)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::IteratorPrototype)?;

        // Constructor property is added once GeneratorFunctionPrototype has been created
        intrinsic_methods!(cx, builder, {
            next    GeneratorPrototype_next    (1),
            return_ GeneratorPrototype_return_ (1),
            throw   GeneratorPrototype_throw   (1),
        });

        // %GeneratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-generator.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.generator())?;

        builder.build()
    }

    runtime_fn! {
    /// %GeneratorPrototype%.next (https://tc39.es/ecma262/#sec-generator.prototype.next)
    fn next(cx, this_value, arguments) {
        let value = arguments.get(cx, 0);
        generator_resume(cx, this_value, value)
    }}

    runtime_fn! {
    /// %GeneratorPrototype%.return (https://tc39.es/ecma262/#sec-generator.prototype.return)
    fn return_(cx, this_value, arguments) {
        let value = arguments.get(cx, 0);
        generator_resume_abrupt(cx, this_value, value, GeneratorCompletionType::Return)
    }}

    runtime_fn! {
    /// %GeneratorPrototype%.throw (https://tc39.es/ecma262/#sec-generator.prototype.throw)
    fn throw(cx, this_value, arguments) {
        let error = arguments.get(cx, 0);
        generator_resume_abrupt(cx, this_value, error, GeneratorCompletionType::Throw)
    }}

    /// Every generator function has a prototype property referencing an instance of the generator
    /// prototype. Install this property on a generator function.
    pub fn install_on_generator_function(
        cx: Context,
        closure: Handle<ClosureObject>,
    ) -> EvalResult<()> {
        let proto = object_create::<ObjectValue>(
            cx,
            HeapItemKind::OrdinaryObject,
            Intrinsic::GeneratorPrototype,
        )?
        .to_handle();

        let proto_desc = PropertyDescriptor::data(proto.to_handle().into(), true, false, false);
        define_property_or_throw(cx, closure.into(), cx.names.prototype(), proto_desc)?;

        Ok(())
    }
}
