use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction, completion::EvalResult, error::type_error_,
        gc::HandleValue, object_value::ObjectValue, property::Property, property_key::PropertyKey,
        realm::Realm, string_value::StringValue, value::SymbolValue, Context, Handle,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, symbol_constructor::SymbolObject};

pub struct SymbolPrototype;

impl SymbolPrototype {
    // 20.4.3 Properties of the Symbol Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once SymbolConstructor has been created
        object.intrinsic_getter(cx, cx.names.description(), Self::get_description, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        // [Symbol.toPrimitive] property
        let to_primitive_key = cx.well_known_symbols.to_primitive();
        let to_primitive_name = cx.alloc_string(String::from("[Symbol.toPrimitive]"));
        let to_primitive_name_key = PropertyKey::string(cx, to_primitive_name);
        let to_primitive_func = BuiltinFunction::create(
            cx,
            Self::to_primitive,
            1,
            to_primitive_name_key,
            Some(realm),
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
    fn get_description(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let symbol_value = maybe!(this_symbol_value(cx, this_value));
        match symbol_value.as_symbol().description() {
            None => cx.undefined().into(),
            Some(desc) => desc.into(),
        }
    }

    // 20.4.3.3 Symbol.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let symbol_value = maybe!(this_symbol_value(cx, this_value));
        symbol_descriptive_string(cx, symbol_value.as_symbol()).into()
    }

    // 20.4.3.4 Symbol.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        this_symbol_value(cx, this_value)
    }

    // 20.4.3.5 Symbol.prototype [ @@toPrimitive ]
    fn to_primitive(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        this_symbol_value(cx, this_value)
    }
}

fn this_symbol_value(cx: &mut Context, value: HandleValue) -> EvalResult<HandleValue> {
    if value.is_symbol() {
        return value.into();
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_symbol_object() {
            return object_value.cast::<SymbolObject>().symbol_data().into();
        }
    }

    type_error_(cx, "value cannot be converted to symbol")
}

// 20.4.3.3.1 SymbolDescriptiveString
pub fn symbol_descriptive_string(
    cx: &mut Context,
    symbol: Handle<SymbolValue>,
) -> Handle<StringValue> {
    match symbol.description() {
        None => cx.alloc_string(String::from("Symbol()")),
        Some(description) => {
            let symbol_prefix = cx.alloc_string(String::from("Symbol("));
            let symbol_suffix = cx.alloc_string(String::from(")"));

            StringValue::concat_all(cx, &[symbol_prefix, description, symbol_suffix])
        }
    }
}
