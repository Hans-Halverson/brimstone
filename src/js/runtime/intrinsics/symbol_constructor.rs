use std::mem::size_of;

use crate::{
    extend_object, intrinsic_methods,
    runtime::{
        Context, Handle, HeapPtr,
        alloc_error::AllocResult,
        collections::BsHashMapField,
        error::type_error,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemKind,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        ordinary_object::object_create,
        realm::Realm,
        type_utilities::to_string,
        value::SymbolValue,
    },
    runtime_fn, set_uninit,
};

// Symbol Objects (https://tc39.es/ecma262/#sec-symbol-objects)
extend_object! {
    pub struct SymbolObject {
        // The symbol value wrapped by this object
        symbol_data: HeapPtr<SymbolValue>,
    }
}

impl SymbolObject {
    pub fn new_from_value(
        cx: Context,
        symbol_data: Handle<SymbolValue>,
    ) -> AllocResult<Handle<SymbolObject>> {
        let mut object = object_create::<SymbolObject>(
            cx,
            HeapItemKind::SymbolObject,
            Intrinsic::SymbolPrototype,
        )?;

        set_uninit!(object.symbol_data, *symbol_data);

        Ok(object.to_handle())
    }

    pub fn symbol_data(&self) -> Handle<SymbolValue> {
        self.symbol_data.to_handle()
    }
}

pub struct SymbolConstructor;

impl SymbolConstructor {
    /// Properties of the Symbol Constructor (https://tc39.es/ecma262/#sec-properties-of-the-symbol-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::SymbolConstructor_construct,
            0,
            cx.names.symbol(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::SymbolPrototype)?;

        intrinsic_methods!(cx, builder, {
            for_    SymbolConstructor_for_    (1),
            key_for SymbolConstructor_key_for (1),
        });

        // Well known symbols
        builder
            .frozen(cx.names.async_iterator(), cx.symbols.async_iterator().as_symbol().into())?;
        builder.frozen(cx.names.has_instance(), cx.symbols.has_instance().as_symbol().into())?;
        builder.frozen(
            cx.names.is_concat_spreadable(),
            cx.symbols.is_concat_spreadable().as_symbol().into(),
        )?;
        builder.frozen(cx.names.iterator_(), cx.symbols.iterator().as_symbol().into())?;
        builder.frozen(cx.names.match_(), cx.symbols.match_().as_symbol().into())?;
        builder.frozen(cx.names.match_all(), cx.symbols.match_all().as_symbol().into())?;
        builder.frozen(cx.names.replace(), cx.symbols.replace().as_symbol().into())?;
        builder.frozen(cx.names.search(), cx.symbols.search().as_symbol().into())?;
        builder.frozen(cx.names.species(), cx.symbols.species().as_symbol().into())?;
        builder.frozen(cx.names.split(), cx.symbols.split().as_symbol().into())?;
        builder.frozen(cx.names.to_primitive(), cx.symbols.to_primitive().as_symbol().into())?;
        builder.frozen(cx.names.to_string_tag(), cx.symbols.to_string_tag().as_symbol().into())?;
        builder.frozen(cx.names.unscopables(), cx.symbols.unscopables().as_symbol().into())?;

        builder.build()
    }

    runtime_fn! {
    /// Symbol (https://tc39.es/ecma262/#sec-symbol-description)
    fn construct(cx, _, arguments) {
        if cx.current_new_target().is_some() {
            return type_error(cx, "Symbol constructor must be called with new");
        }

        let description_arg = arguments.get(cx, 0);
        let description_value = if description_arg.is_undefined() {
            None
        } else {
            Some(to_string(cx, description_arg)?)
        };

        Ok(SymbolValue::new(cx, description_value, /* is_private */ false)?.into())
    }}

    runtime_fn! {
    /// Symbol.for (https://tc39.es/ecma262/#sec-symbol.for)
    fn for_(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let string_key = to_string(cx, argument)?.flatten()?;
        if let Some(symbol_value) = cx.global_symbol_registry().get(&string_key) {
            return Ok(symbol_value.to_handle().into());
        }

        let new_symbol =
            SymbolValue::new(cx, Some(string_key.as_string()), /* is_private */ false)?;
        cx.global_symbol_registry_field()
            .maybe_grow_for_insertion(cx)?
            .insert_without_growing(*string_key, *new_symbol);

        Ok(new_symbol.into())
    }}

    runtime_fn! {
    /// Symbol.keyFor (https://tc39.es/ecma262/#sec-symbol.keyfor)
    fn key_for(cx, _, arguments) {
        let symbol_value = arguments.get(cx, 0);
        if !symbol_value.is_symbol() {
            return type_error(cx, "Symbol.keyFor argument must be a symbol");
        }
        let symbol_value = *symbol_value.as_symbol();

        for (string, symbol) in cx.global_symbol_registry().iter_gc_unsafe() {
            if symbol.ptr_eq(&symbol_value) {
                return Ok(string.to_handle().as_value());
            }
        }

        Ok(cx.undefined())
    }}
}

impl HeapItem for HeapPtr<SymbolObject> {
    fn byte_size(&self) -> usize {
        size_of::<SymbolObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.symbol_data);
    }
}
