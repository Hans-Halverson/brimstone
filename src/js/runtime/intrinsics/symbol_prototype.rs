use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        string_value::StringValue,
        value::SymbolValue,
    },
    runtime_fn,
};

pub struct SymbolPrototype;

impl SymbolPrototype {
    /// Properties of the Symbol Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-symbol-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once SymbolConstructor has been created
        intrinsic_methods!(cx, builder, {
            to_string SymbolPrototype_to_string (0),
            value_of  SymbolPrototype_value_of  (0),
        });

        builder.getter(cx.names.description(), RuntimeFunction::SymbolPrototype_get_description)?;

        // [Symbol.toPrimitive] property
        let to_primitive_key = cx.symbols.to_primitive();
        let to_primitive_func =
            builder.function(RuntimeFunction::SymbolPrototype_value_of, 1, to_primitive_key)?;
        builder.property(
            to_primitive_key,
            Property::data(to_primitive_func.into(), false, false, true),
        )?;

        // [Symbol.toStringTag] property
        builder.to_string_tag(cx.names.symbol())?;

        builder.build()
    }

    runtime_fn! {
    /// get Symbol.prototype.description (https://tc39.es/ecma262/#sec-symbol.prototype.description)
    fn get_description(cx, this_value, _) {
        let symbol_value = this_symbol_value(cx, this_value, "description")?;
        match symbol_value.as_symbol().description() {
            None => Ok(cx.undefined()),
            Some(desc) => Ok(desc.as_value()),
        }
    }}

    runtime_fn! {
    /// Symbol.prototype.toString (https://tc39.es/ecma262/#sec-symbol.prototype.tostring)
    fn to_string(cx, this_value, _) {
        let symbol_value = this_symbol_value(cx, this_value, "toString")?;
        Ok(symbol_descriptive_string(cx, symbol_value.as_symbol())?.as_value())
    }}

    runtime_fn! {
    /// Symbol.prototype.valueOf (https://tc39.es/ecma262/#sec-symbol.prototype.valueof)
    fn value_of(cx, this_value, _) {
        this_symbol_value(cx, this_value, "valueOf")
    }}
}

fn this_symbol_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<Value>> {
    if value.is_symbol() {
        return Ok(value);
    }

    if value.is_object() {
        let object_value = value.as_object();
        if let Some(symbol_object) = object_value.as_symbol_object() {
            return Ok(symbol_object.symbol_data().into());
        }
    }

    type_error(cx, &format!("Symbol.prototype.{} must be called on a symbol", method_name))
}

/// SymbolDescriptiveString (https://tc39.es/ecma262/#sec-symboldescriptivestring)
pub fn symbol_descriptive_string(
    mut cx: Context,
    symbol: Handle<SymbolValue>,
) -> EvalResult<Handle<StringValue>> {
    match symbol.description() {
        None => Ok(cx.alloc_static_string("Symbol()")?),
        Some(description) => {
            let symbol_prefix = cx.alloc_static_string("Symbol(")?;
            let symbol_suffix = cx.alloc_static_string(")")?;

            StringValue::concat_all(cx, &[symbol_prefix, description.as_string(), symbol_suffix])
        }
    }
}
