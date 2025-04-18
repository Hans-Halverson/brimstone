use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        builtin_function::BuiltinFunction,
        collections::BsHashMapField,
        error::type_error,
        eval_result::EvalResult,
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        realm::Realm,
        type_utilities::to_string,
        value::SymbolValue,
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

// Symbol Objects (https://tc39.es/ecma262/#sec-symbol-objects)
extend_object! {
    pub struct SymbolObject {
        // The symbol value wrapped by this object
        symbol_data: HeapPtr<SymbolValue>,
    }
}

impl SymbolObject {
    pub fn new_from_value(cx: Context, symbol_data: Handle<SymbolValue>) -> Handle<SymbolObject> {
        let mut object =
            object_create::<SymbolObject>(cx, ObjectKind::SymbolObject, Intrinsic::SymbolPrototype);

        set_uninit!(object.symbol_data, *symbol_data);

        object.to_handle()
    }

    pub fn symbol_data(&self) -> Handle<SymbolValue> {
        self.symbol_data.to_handle()
    }
}

pub struct SymbolConstructor;

impl SymbolConstructor {
    /// Properties of the Symbol Constructor (https://tc39.es/ecma262/#sec-properties-of-the-symbol-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.symbol(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::SymbolPrototype).into(),
        );

        // Well known symbols
        let async_iterator = cx.well_known_symbols.async_iterator().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.async_iterator(), async_iterator.into());

        let has_instance = cx.well_known_symbols.has_instance().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.has_instance(), has_instance.into());

        let is_concat_spreadable = cx.well_known_symbols.is_concat_spreadable().as_symbol();
        func.intrinsic_frozen_property(
            cx,
            cx.names.is_concat_spreadable(),
            is_concat_spreadable.into(),
        );

        let iterator = cx.well_known_symbols.iterator().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.iterator_(), iterator.into());

        let match_ = cx.well_known_symbols.match_().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.match_(), match_.into());

        let match_all = cx.well_known_symbols.match_all().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.match_all(), match_all.into());

        let replace = cx.well_known_symbols.replace().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.replace(), replace.into());

        let search = cx.well_known_symbols.search().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.search(), search.into());

        let species = cx.well_known_symbols.species().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.species(), species.into());

        let split = cx.well_known_symbols.split().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.split(), split.into());

        let to_primitive = cx.well_known_symbols.to_primitive().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.to_primitive(), to_primitive.into());

        let to_string_tag = cx.well_known_symbols.to_string_tag().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.to_string_tag(), to_string_tag.into());

        let unscopables = cx.well_known_symbols.unscopables().as_symbol();
        func.intrinsic_frozen_property(cx, cx.names.unscopables(), unscopables.into());

        func.intrinsic_func(cx, cx.names.for_(), Self::for_, 1, realm);
        func.intrinsic_func(cx, cx.names.key_for(), Self::key_for, 1, realm);

        func
    }

    /// Symbol (https://tc39.es/ecma262/#sec-symbol-description)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if cx.current_new_target().is_some() {
            return type_error(cx, "Symbol is not a constructor");
        }

        let description_arg = get_argument(cx, arguments, 0);
        let description_value = if description_arg.is_undefined() {
            None
        } else {
            Some(to_string(cx, description_arg)?)
        };

        Ok(SymbolValue::new(cx, description_value, /* is_private */ false).into())
    }

    /// Symbol.for (https://tc39.es/ecma262/#sec-symbol.for)
    pub fn for_(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let string_key = to_string(cx, argument)?.flatten();
        if let Some(symbol_value) = cx.global_symbol_registry().get(&string_key) {
            return Ok(symbol_value.to_handle().into());
        }

        let new_symbol =
            SymbolValue::new(cx, Some(string_key.as_string()), /* is_private */ false);
        cx.global_symbol_registry_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(*string_key, *new_symbol);

        Ok(new_symbol.into())
    }

    /// Symbol.keyFor (https://tc39.es/ecma262/#sec-symbol.keyfor)
    pub fn key_for(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let symbol_value = get_argument(cx, arguments, 0);
        if !symbol_value.is_symbol() {
            return type_error(cx, "expected symbol value");
        }
        let symbol_value = *symbol_value.as_symbol();

        for (string, symbol) in cx.global_symbol_registry().iter_gc_unsafe() {
            if symbol.ptr_eq(&symbol_value) {
                return Ok(string.to_handle().as_value());
            }
        }

        Ok(cx.undefined())
    }
}

impl HeapObject for HeapPtr<SymbolObject> {
    fn byte_size(&self) -> usize {
        size_of::<SymbolObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.symbol_data);
    }
}
