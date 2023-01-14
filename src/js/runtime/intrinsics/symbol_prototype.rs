use crate::{
    js::runtime::{
        completion::EvalResult,
        error::type_error_,
        gc::Gc,
        object_value::ObjectValue,
        ordinary_object::OrdinaryObject,
        realm::Realm,
        value::{StringValue, SymbolValue, Value},
        Context,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, symbol_constructor::SymbolObject};

pub struct SymbolPrototype;

impl SymbolPrototype {
    // 20.4.3 Properties of the Symbol Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once SymbolConstructor has been created
        object.intrinsic_getter(cx, cx.names.description, Self::get_description, realm);
        object.intrinsic_func(cx, cx.names.to_string, Self::to_string, 0, realm);
        object.intrinsic_func(cx, cx.names.value_of, Self::value_of, 0, realm);

        cx.heap.alloc(object).into()
    }

    // 20.4.3.2 get Symbol.prototype.description
    fn get_description(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let symbol_value = maybe!(this_symbol_value(cx, this_value));
        match symbol_value.as_symbol().description() {
            None => Value::undefined().into(),
            Some(desc) => cx.heap.alloc_string(String::from(desc)).into(),
        }
    }

    // 20.4.3.3 Symbol.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let symbol_value = maybe!(this_symbol_value(cx, this_value));
        symbol_descriptive_string(cx, symbol_value.as_symbol()).into()
    }

    // 20.4.3.4 Symbol.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_symbol_value(cx, this_value)
    }
}

fn this_symbol_value(cx: &mut Context, value: Value) -> EvalResult<Value> {
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
pub fn symbol_descriptive_string(cx: &mut Context, symbol: Gc<SymbolValue>) -> Gc<StringValue> {
    let description = symbol.description().unwrap_or("");
    let descriptive_string = format!("Symbol({})", description);
    cx.heap.alloc_string(descriptive_string).into()
}
