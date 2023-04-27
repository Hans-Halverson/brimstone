use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_ordinary_init,
        realm::Realm,
        type_utilities::to_string,
        value::{SymbolValue, Value},
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 20.4 Symbol Objects
extend_object! {
    pub struct SymbolObject {
        // The symbol value wrapped by this object
        symbol_data: Gc<SymbolValue>,
    }
}

impl SymbolObject {
    pub fn new_from_value(cx: &mut Context, symbol_data: Gc<SymbolValue>) -> Gc<SymbolObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::SymbolPrototype);

        let mut object = cx.heap.alloc_uninit::<SymbolObject>();
        object_ordinary_init(cx, object.object(), ObjectKind::SymbolObject, proto);

        object.symbol_data = symbol_data;

        object
    }

    pub fn symbol_data(&self) -> Gc<SymbolValue> {
        self.symbol_data
    }
}

pub struct SymbolConstructor;

impl SymbolConstructor {
    // 20.4.2 Properties of the Symbol Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            0,
            cx.names.symbol(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::SymbolPrototype).into(),
        );

        // Well known symbols
        let async_iterator = cx.well_known_symbols.async_iterator;
        func.intrinsic_frozen_property(cx, cx.names.async_iterator(), async_iterator.into());

        let has_instance = cx.well_known_symbols.has_instance;
        func.intrinsic_frozen_property(cx, cx.names.has_instance(), has_instance.into());

        let is_concat_spreadable = cx.well_known_symbols.is_concat_spreadable;
        func.intrinsic_frozen_property(
            cx,
            cx.names.is_concat_spreadable(),
            is_concat_spreadable.into(),
        );

        let iterator = cx.well_known_symbols.iterator;
        func.intrinsic_frozen_property(cx, cx.names.iterator(), iterator.into());

        let match_ = cx.well_known_symbols.match_;
        func.intrinsic_frozen_property(cx, cx.names.match_(), match_.into());

        let match_all = cx.well_known_symbols.match_all;
        func.intrinsic_frozen_property(cx, cx.names.match_all(), match_all.into());

        let replace = cx.well_known_symbols.replace;
        func.intrinsic_frozen_property(cx, cx.names.replace(), replace.into());

        let search = cx.well_known_symbols.search;
        func.intrinsic_frozen_property(cx, cx.names.search(), search.into());

        let species = cx.well_known_symbols.species;
        func.intrinsic_frozen_property(cx, cx.names.species(), species.into());

        let split = cx.well_known_symbols.split;
        func.intrinsic_frozen_property(cx, cx.names.split(), split.into());

        let to_primitive = cx.well_known_symbols.to_primitive;
        func.intrinsic_frozen_property(cx, cx.names.to_primitive(), to_primitive.into());

        let to_string_tag = cx.well_known_symbols.to_string_tag;
        func.intrinsic_frozen_property(cx, cx.names.to_string_tag(), to_string_tag.into());

        let unscopables = cx.well_known_symbols.unscopables;
        func.intrinsic_frozen_property(cx, cx.names.unscopables(), unscopables.into());

        func.intrinsic_func(cx, cx.names.for_(), Self::for_, 1, realm);
        func.intrinsic_func(cx, cx.names.key_for(), Self::key_for, 1, realm);

        func
    }

    // 20.4.1.1 Symbol
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if new_target.is_some() {
            return type_error_(cx, "Symbol is not a constructor");
        }

        let description_arg = get_argument(arguments, 0);
        let description_value = if description_arg.is_undefined() {
            None
        } else {
            Some(maybe!(to_string(cx, description_arg)))
        };

        SymbolValue::new(cx, description_value).into()
    }

    // 20.4.2.2 Symbol.for
    fn for_(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let string_key = maybe!(to_string(cx, get_argument(arguments, 0)));
        if let Some(symbol_value) = cx.global_symbol_registry.get(&string_key) {
            return (*symbol_value).into();
        }

        let new_symbol = SymbolValue::new(cx, Some(string_key));
        cx.global_symbol_registry.insert(string_key, new_symbol);

        new_symbol.into()
    }

    // 20.4.2.6 Symbol.keyFor
    fn key_for(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let symbol_value = get_argument(arguments, 0);
        if !symbol_value.is_symbol() {
            return type_error_(cx, "expected symbol value");
        }
        let symbol_value = symbol_value.as_symbol();

        for (string, symbol) in &cx.global_symbol_registry {
            if symbol.ptr_eq(&symbol_value) {
                return (*string).into();
            }
        }

        Value::undefined().into()
    }
}
