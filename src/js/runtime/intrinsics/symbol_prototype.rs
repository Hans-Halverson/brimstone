use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction, completion::EvalResult, error::type_error,
        object_value::ObjectValue, property::Property, realm::Realm, string_value::StringValue,
        value::SymbolValue, Context, Handle, Value,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, symbol_constructor::SymbolObject};

pub struct SymbolPrototype;

impl SymbolPrototype {
    // 20.4.3 Properties of the Symbol Prototype Object
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

    // 20.4.3.2 get Symbol.prototype.description
    pub fn get_description(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let symbol_value = maybe!(this_symbol_value(cx, this_value));
        match symbol_value.as_symbol().description() {
            None => cx.undefined().into(),
            Some(desc) => desc.as_string().into(),
        }
    }

    // 20.4.3.3 Symbol.prototype.toString
    pub fn to_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let symbol_value = maybe!(this_symbol_value(cx, this_value));
        symbol_descriptive_string(cx, symbol_value.as_symbol()).into()
    }

    // 20.4.3.4 Symbol.prototype.valueOf
    pub fn value_of(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_symbol_value(cx, this_value)
    }

    // 20.4.3.5 Symbol.prototype [ @@toPrimitive ]
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
        return value.into();
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_symbol_object() {
            return object_value.cast::<SymbolObject>().symbol_data().into();
        }
    }

    type_error(cx, "value cannot be converted to symbol")
}

// 20.4.3.3.1 SymbolDescriptiveString
pub fn symbol_descriptive_string(
    mut cx: Context,
    symbol: Handle<SymbolValue>,
) -> Handle<StringValue> {
    match symbol.description() {
        None => cx.alloc_string("Symbol()"),
        Some(description) => {
            let symbol_prefix = cx.alloc_string("Symbol(");
            let symbol_suffix = cx.alloc_string(")");

            StringValue::concat_all(cx, &[symbol_prefix, description.as_string(), symbol_suffix])
        }
    }
}
