use crate::js::runtime::{
    builtin_function::BuiltinFunction, error::type_error, eval_result::EvalResult,
    object_value::ObjectValue, property::Property, realm::Realm, string_value::StringValue,
    value::SymbolValue, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

pub struct SymbolPrototype;

impl SymbolPrototype {
    /// Properties of the Symbol Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-symbol-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once SymbolConstructor has been created
        object.intrinsic_getter(cx, cx.names.description(), Self::get_description, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        // [Symbol.toPrimitive] property
        let to_primitive_key = cx.well_known_symbols.to_primitive();
        let to_primitive_func = BuiltinFunction::create(
            cx,
            Self::to_primitive,
            1,
            cx.names.symbol_to_primitive(),
            realm,
            None,
            None,
        )
        .into();
        object.set_property(
            cx,
            to_primitive_key,
            Property::data(to_primitive_func, false, false, true),
        );

        // [Symbol.toStringTag] property
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.symbol().as_string().into(), false, false, true),
        );

        object
    }

    /// get Symbol.prototype.description (https://tc39.es/ecma262/#sec-symbol.prototype.description)
    pub fn get_description(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let symbol_value = this_symbol_value(cx, this_value)?;
        match symbol_value.as_symbol().description() {
            None => Ok(cx.undefined()),
            Some(desc) => Ok(desc.as_value()),
        }
    }

    /// Symbol.prototype.toString (https://tc39.es/ecma262/#sec-symbol.prototype.tostring)
    pub fn to_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let symbol_value = this_symbol_value(cx, this_value)?;
        Ok(symbol_descriptive_string(cx, symbol_value.as_symbol()).as_value())
    }

    /// Symbol.prototype.valueOf (https://tc39.es/ecma262/#sec-symbol.prototype.valueof)
    pub fn value_of(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_symbol_value(cx, this_value)
    }

    /// Symbol.prototype [ @@toPrimitive ] (https://tc39.es/ecma262/#sec-symbol.prototype-%symbol.toprimitive%)
    pub fn to_primitive(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_symbol_value(cx, this_value)
    }
}

fn this_symbol_value(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    if value.is_symbol() {
        return Ok(value);
    }

    if value.is_object() {
        let object_value = value.as_object();
        if let Some(symbol_object) = object_value.as_symbol_object() {
            return Ok(symbol_object.symbol_data().into());
        }
    }

    type_error(cx, "value cannot be converted to symbol")
}

/// SymbolDescriptiveString (https://tc39.es/ecma262/#sec-symboldescriptivestring)
pub fn symbol_descriptive_string(
    mut cx: Context,
    symbol: Handle<SymbolValue>,
) -> Handle<StringValue> {
    match symbol.description() {
        None => cx.alloc_string("Symbol()").as_string(),
        Some(description) => {
            let symbol_prefix = cx.alloc_string("Symbol(").as_string();
            let symbol_suffix = cx.alloc_string(")").as_string();

            StringValue::concat_all(cx, &[symbol_prefix, description.as_string(), symbol_suffix])
        }
    }
}
