use crate::{
    must,
    runtime::{
        abstract_operations::{call_object, get_method, length_of_array_like, set},
        builtin_function::BuiltinFunction,
        error::type_error,
        eval_result::EvalResult,
        function::get_argument,
        get,
        iterator::iter_iterator_method_values,
        object_value::ObjectValue,
        to_string,
        type_utilities::{
            is_callable, is_constructor_value, is_integral_number, to_number, to_object,
        },
        value::Value,
        Context, Handle, PropertyKey, Realm,
    },
};

use super::{
    intrinsics::Intrinsic,
    rust_runtime::return_this,
    typed_array::DynTypedArray,
    typed_array_prototype::{
        is_typed_array_out_of_bounds, make_typed_array_with_buffer_witness_record,
        typed_array_create_from_constructor, typed_array_create_from_constructor_object,
        typed_array_length,
    },
};

/// The %TypedArray% Intrinsic Object (https://tc39.es/ecma262/#sec-%typedarray%-intrinsic-object)
pub struct TypedArrayConstructor;

impl TypedArrayConstructor {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            0,
            cx.names.typed_array(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::TypedArrayPrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.from(), Self::from, 1, realm);
        func.intrinsic_func(cx, cx.names.of(), Self::of, 0, realm);

        // get %TypedArray% [ @@species ] (https://tc39.es/ecma262/#sec-get-%typedarray%-%symbol.species%)
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, return_this, realm);

        func
    }

    /// %TypedArray% (https://tc39.es/ecma262/#sec-%typedarray%)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "TypedArray constructor is abstract and cannot be called")
    }

    /// %TypedArray%.from (https://tc39.es/ecma262/#sec-%typedarray%.from)
    pub fn from(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if !is_constructor_value(this_value) {
            return type_error(cx, "TypedArray.from must be called on constructor");
        }

        let this_constructor = this_value.as_object();

        let map_function = {
            let argument = get_argument(cx, arguments, 1);
            if argument.is_undefined() {
                None
            } else if !is_callable(argument) {
                return type_error(cx, "map function must be a function");
            } else {
                Some(argument.as_object())
            }
        };

        let source = get_argument(cx, arguments, 0);
        let this_argument = get_argument(cx, arguments, 2);

        let iterator_key = cx.well_known_symbols.iterator();
        let iterator = get_method(cx, source, iterator_key)?;

        // If source is iterable then add all values from iterator
        if let Some(iterator) = iterator {
            // Collect all values from iterator
            let mut values = vec![];
            iter_iterator_method_values(cx, source, iterator, &mut |_, value| {
                values.push(value);
                None
            })?;

            let length = values.len();

            let length_value = Value::from(length).to_handle(cx);
            let target_object =
                typed_array_create_from_constructor_object(cx, this_constructor, &[length_value])?;

            // Shared between iterations
            let mut index_key = PropertyKey::uninit().to_handle(cx);
            let mut index_value = Value::uninit().to_handle(cx);

            for (i, value) in values.into_iter().enumerate() {
                index_key.replace(PropertyKey::from_u64(cx, i as u64));

                let value = if let Some(map_function) = map_function {
                    index_value.replace(Value::from(i));
                    call_object(cx, map_function, this_argument, &[value, index_value])?
                } else {
                    value
                };

                set(cx, target_object, index_key, value, true)?;
            }

            return Ok(target_object.as_value());
        }

        // Otherwise treat source like an array and add its values
        let array_like = must!(to_object(cx, source));
        let length = length_of_array_like(cx, array_like)? as usize;

        let length_value = Value::from(length).to_handle(cx);
        let target_object =
            typed_array_create_from_constructor_object(cx, this_constructor, &[length_value])?;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));

            let value = get(cx, array_like, index_key)?;

            let value = if let Some(map_function) = map_function {
                index_value.replace(Value::from(i));
                call_object(cx, map_function, this_argument, &[value, index_value])?
            } else {
                value
            };

            set(cx, target_object, index_key, value, true)?;
        }

        Ok(target_object.as_value())
    }

    /// %TypedArray%.of (https://tc39.es/ecma262/#sec-%typedarray%.of)
    pub fn of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if !is_constructor_value(this_value) {
            return type_error(cx, "TypedArray.of must be called on constructor");
        }

        let this_constructor = this_value.as_object();
        let length = arguments.len();
        let length_value = Value::from(length).to_handle(cx);

        let typed_array =
            typed_array_create_from_constructor(cx, this_constructor, &[length_value])?;
        let object = typed_array.into_object_value();

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for (i, value) in arguments.iter().enumerate() {
            key.replace(PropertyKey::from_u64(cx, i as u64));
            set(cx, object, key, *value, true)?;
        }

        Ok(object.as_value())
    }
}

#[macro_export]
macro_rules! create_typed_array_constructor {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $content_type:expr, $prototype:ident, $constructor:ident, $to_element:ident, $from_element:ident) => {
        macro_rules! element_size {
            () => {
                std::mem::size_of::<$element_type>()
            };
        }

        extend_object! {
            pub struct $typed_array {
                viewed_array_buffer: HeapPtr<ArrayBufferObject>,
                byte_length: Option<usize>,
                array_length: Option<usize>,
                byte_offset: usize,
            }
        }

        impl $typed_array {
            pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();
            pub const TYPED_ARRAY_VTABLE: *const () = extract_typed_array_vtable::<Self>();

            fn new_with_proto(
                cx: Context,
                proto: Handle<ObjectValue>,
                viewed_array_buffer: Handle<ArrayBufferObject>,
                byte_length: Option<usize>,
                byte_offset: usize,
                array_length: Option<usize>,
            ) -> Handle<ObjectValue> {
                let mut object =
                    object_create_with_proto::<$typed_array>(cx, ObjectKind::$typed_array, proto);

                set_uninit!(object.viewed_array_buffer, *viewed_array_buffer);
                set_uninit!(object.byte_length, byte_length);
                set_uninit!(object.array_length, array_length);
                set_uninit!(object.byte_offset, byte_offset);

                object.to_handle().into()
            }

            #[inline]
            fn write_element(
                mut array_buffer: HeapPtr<ArrayBufferObject>,
                byte_index: usize,
                value: $element_type,
            ) {
                unsafe {
                    let byte_ptr = array_buffer.data().as_mut_ptr().add(byte_index);
                    let element_ptr = byte_ptr.cast::<$element_type>();

                    element_ptr.write(value)
                }
            }
        }

        #[wrap_ordinary_object]
        impl VirtualObject for Handle<$typed_array> {
            /// [[GetOwnProperty]] (https://tc39.es/ecma262/#sec-typedarray-getownproperty)
            fn get_own_property(
                &self,
                cx: Context,
                key: Handle<PropertyKey>,
            ) -> EvalResult<Option<PropertyDescriptor>> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array()) {
                    CanonicalIndexType::NotAnIndex => {
                        Ok(ordinary_get_own_property(cx, self.as_object(), key))
                    }
                    CanonicalIndexType::Invalid => Ok(None),
                    CanonicalIndexType::Valid(index) => {
                        let array_buffer_ptr = self.viewed_array_buffer_ptr();
                        let byte_index = index * element_size!() + self.byte_offset;

                        let value = self.read_element_value(cx, array_buffer_ptr, byte_index);

                        let desc = PropertyDescriptor::data(value, true, true, true);

                        Ok(Some(desc))
                    }
                }
            }

            /// [[HasProperty]] (https://tc39.es/ecma262/#sec-typedarray-hasproperty)
            fn has_property(&self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array()) {
                    CanonicalIndexType::NotAnIndex => {
                        ordinary_has_property(cx, self.as_object(), key)
                    }
                    CanonicalIndexType::Invalid => Ok(false),
                    CanonicalIndexType::Valid(_) => Ok(true),
                }
            }

            /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-typedarray-defineownproperty)
            fn define_own_property(
                &mut self,
                cx: Context,
                key: Handle<PropertyKey>,
                desc: PropertyDescriptor,
            ) -> EvalResult<bool> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array()) {
                    CanonicalIndexType::NotAnIndex => {
                        ordinary_define_own_property(cx, self.as_object(), key, desc)
                    }
                    CanonicalIndexType::Invalid => Ok(false),
                    CanonicalIndexType::Valid(index) => {
                        if let Some(false) = desc.is_configurable {
                            return Ok(false);
                        } else if let Some(false) = desc.is_enumerable {
                            return Ok(false);
                        } else if desc.is_accessor_descriptor() {
                            return Ok(false);
                        } else if let Some(false) = desc.is_writable {
                            return Ok(false);
                        }

                        let value = if let Some(value) = desc.value {
                            value
                        } else {
                            return Ok(true);
                        };

                        // May allocate and invoke arbitrary user code
                        let element_value = $to_element(cx, value)?;

                        let array_buffer_ptr = self.viewed_array_buffer_ptr();

                        // Fast path - if typed array has known size and underlying ArrayBuffer is
                        // fixed then the bounds checks in `canonical_numeric_index_string` could
                        // not have been invalidated by the call to `$to_element`.
                        //
                        // The only additional case we need to check is if the ArrayBuffer was
                        // detached by the call to `$to_element`.
                        let index = if array_buffer_ptr.is_fixed_length()
                            && self.array_length().is_some()
                        {
                            if array_buffer_ptr.is_detached() {
                                return Ok(true);
                            }

                            index
                        } else {
                            // Slow path - all bets are off and we must redo all bounds checks.
                            match canonical_numeric_index_string(cx, key, self.as_typed_array()) {
                                CanonicalIndexType::Valid(index) => index,
                                _ => return Ok(true),
                            }
                        };

                        let byte_index = index * element_size!() + self.byte_offset;

                        $typed_array::write_element(array_buffer_ptr, byte_index, element_value);

                        Ok(true)
                    }
                }
            }

            /// [[Get]] (https://tc39.es/ecma262/#sec-typedarray-get)
            fn get(
                &self,
                cx: Context,
                key: Handle<PropertyKey>,
                receiver: Handle<Value>,
            ) -> EvalResult<Handle<Value>> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array()) {
                    CanonicalIndexType::NotAnIndex => {
                        ordinary_get(cx, self.as_object(), key, receiver)
                    }
                    CanonicalIndexType::Invalid => Ok(cx.undefined()),
                    CanonicalIndexType::Valid(index) => {
                        let array_buffer_ptr = self.viewed_array_buffer_ptr();
                        let byte_index = index * element_size!() + self.byte_offset;

                        Ok(self.read_element_value(cx, array_buffer_ptr, byte_index))
                    }
                }
            }

            /// [[Set]] (https://tc39.es/ecma262/#sec-typedarray-set)
            fn set(
                &mut self,
                cx: Context,
                key: Handle<PropertyKey>,
                value: Handle<Value>,
                receiver: Handle<Value>,
            ) -> EvalResult<bool> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array()) {
                    CanonicalIndexType::NotAnIndex => {
                        ordinary_set(cx, self.as_object(), key, value, receiver)
                    }
                    result @ (CanonicalIndexType::Invalid | CanonicalIndexType::Valid(_)) => {
                        // Check if this is not the same object as the specified receiver
                        if !receiver.is_object()
                            || !(*receiver.as_object()).ptr_eq(&self.as_object())
                        {
                            if matches!(result, CanonicalIndexType::Valid(_)) {
                                return ordinary_set(cx, self.as_object(), key, value, receiver);
                            } else {
                                return Ok(true);
                            }
                        }

                        // May allocate and invoke arbitrary user code
                        let element_value = $to_element(cx, value)?;

                        let array_buffer_ptr = self.viewed_array_buffer_ptr();

                        // Fast path - if typed array has known size and underlying ArrayBuffer is
                        // fixed then the bounds checks in `canonical_numeric_index_string` could
                        // not have been invalidated by the call to `$to_element`.
                        //
                        // The only additional case we need to check is if the ArrayBuffer was
                        // detached by the call to `$to_element`.
                        let index = if array_buffer_ptr.is_fixed_length()
                            && self.array_length().is_some()
                        {
                            if array_buffer_ptr.is_detached() {
                                return Ok(true);
                            }

                            match result {
                                CanonicalIndexType::Valid(index) => index,
                                _ => return Ok(true),
                            }
                        } else {
                            // Slow path - all bets are off and we must redo all bounds checks.
                            match canonical_numeric_index_string(cx, key, self.as_typed_array()) {
                                CanonicalIndexType::Valid(index) => index,
                                _ => return Ok(true),
                            }
                        };

                        let byte_index = index * element_size!() + self.byte_offset;

                        $typed_array::write_element(array_buffer_ptr, byte_index, element_value);

                        Ok(true)
                    }
                }
            }

            /// [[Delete]] (https://tc39.es/ecma262/#sec-typedarray-delete)
            fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
                match canonical_numeric_index_string(cx, key, self.as_typed_array()) {
                    CanonicalIndexType::NotAnIndex => ordinary_delete(cx, self.as_object(), key),
                    CanonicalIndexType::Invalid => Ok(true),
                    CanonicalIndexType::Valid(_) => Ok(false),
                }
            }

            /// [[OwnPropertyKeys]] (https://tc39.es/ecma262/#sec-typedarray-ownpropertykeys)
            fn own_property_keys(&self, mut cx: Context) -> EvalResult<Vec<Handle<Value>>> {
                let typed_array_record =
                    make_typed_array_with_buffer_witness_record(self.as_typed_array());

                let mut keys = vec![];

                if !is_typed_array_out_of_bounds(&typed_array_record) {
                    let length = typed_array_length(&typed_array_record);
                    for i in 0..length {
                        let index_string = cx.alloc_string(&i.to_string());
                        keys.push(index_string.into());
                    }
                }

                ordinary_own_string_symbol_property_keys(self.as_object(), &mut keys);

                Ok(keys)
            }

            fn as_typed_array(&self) -> DynTypedArray {
                self.into_dyn_typed_array()
            }
        }

        impl TypedArray for Handle<$typed_array> {
            fn array_length(&self) -> Option<usize> {
                self.array_length
            }

            fn byte_length(&self) -> Option<usize> {
                self.byte_length
            }

            fn byte_offset(&self) -> usize {
                self.byte_offset
            }

            fn viewed_array_buffer_ptr(&self) -> HeapPtr<ArrayBufferObject> {
                self.viewed_array_buffer
            }

            fn viewed_array_buffer(&self) -> Handle<ArrayBufferObject> {
                self.viewed_array_buffer.to_handle()
            }

            fn name(&self, cx: Context) -> Handle<StringValue> {
                cx.names.$rust_name().as_string()
            }

            fn content_type(&self) -> ContentType {
                $content_type
            }

            fn kind(&self) -> TypedArrayKind {
                TypedArrayKind::$typed_array
            }

            fn element_size(&self) -> usize {
                element_size!()
            }

            #[inline]
            fn read_element_value(
                &self,
                cx: Context,
                mut array_buffer: HeapPtr<ArrayBufferObject>,
                byte_index: usize,
            ) -> Handle<Value> {
                let element = unsafe {
                    let byte_ptr = array_buffer.data().as_ptr().add(byte_index);
                    byte_ptr.cast::<$element_type>().read()
                };

                // May allocate
                $from_element(cx, element)
            }

            #[inline]
            fn write_element_value(
                &mut self,
                cx: Context,
                byte_index: usize,
                value: Handle<Value>,
            ) -> EvalResult<()> {
                // May allocate, so call before accessing array buffer
                let element_value = $to_element(cx, value)?;

                unsafe {
                    let byte_ptr = self
                        .viewed_array_buffer_ptr()
                        .data()
                        .as_mut_ptr()
                        .add(byte_index);
                    let element_ptr = byte_ptr.cast::<$element_type>();

                    element_ptr.write(element_value)
                }

                Ok(())
            }

            #[inline]
            fn write_element_value_unchecked(
                &mut self,
                cx: Context,
                array_index: u64,
                value: Handle<Value>,
            ) -> EvalResult<()> {
                // May allocate, so call before accessing array buffer
                let element_value = $to_element(cx, value)?;

                // Call to `$to_element` may invoke user cade. Check if array has become detached
                // or out of bounds.
                let typed_array_record =
                    make_typed_array_with_buffer_witness_record(self.as_typed_array());
                if is_typed_array_out_of_bounds(&typed_array_record) {
                    return Ok(());
                }

                // Then check if index has become out of bounds
                let length = typed_array_length(&typed_array_record);
                if array_index >= length as u64 {
                    return Ok(());
                }

                let array_buffer_ptr = self.viewed_array_buffer_ptr();
                let byte_index = (array_index as usize) * element_size!() + self.byte_offset;

                $typed_array::write_element(array_buffer_ptr, byte_index, element_value);

                Ok(())
            }
        }

        pub struct $constructor;

        impl $constructor {
            /// Properties of the TypedArray Constructors (https://tc39.es/ecma262/#sec-properties-of-the-typedarray-constructors)
            pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
                let prototype = realm.get_intrinsic(Intrinsic::TypedArrayConstructor);
                let mut func = BuiltinFunction::intrinsic_constructor(
                    cx,
                    Self::construct,
                    3,
                    cx.names.$rust_name(),
                    realm,
                    Some(prototype),
                );

                func.intrinsic_frozen_property(
                    cx,
                    cx.names.prototype(),
                    realm.get_intrinsic(Intrinsic::$prototype).into(),
                );

                let element_size_value = cx.smi(element_size!() as i32);
                func.intrinsic_frozen_property(
                    cx,
                    cx.names.bytes_per_element(),
                    element_size_value,
                );

                func
            }

            /// TypedArray (https://tc39.es/ecma262/#sec-typedarray)
            pub fn construct(
                mut cx: Context,
                _: Handle<Value>,
                arguments: &[Handle<Value>],
            ) -> EvalResult<Handle<Value>> {
                let new_target = if let Some(new_target) = cx.current_new_target() {
                    new_target
                } else {
                    return type_error(
                        cx,
                        &format!("{} constructor must be called with new", cx.names.$rust_name()),
                    );
                };

                if arguments.is_empty() {
                    return Self::allocate_with_length(cx, new_target, 0);
                }

                let argument = get_argument(cx, arguments, 0);
                if !argument.is_object() {
                    let length = to_index(cx, argument)?;
                    return Self::allocate_with_length(cx, new_target, length);
                }

                let proto = get_prototype_from_constructor(cx, new_target, Intrinsic::$prototype)?;

                let argument = argument.as_object();
                if argument.is_typed_array() {
                    return Self::initialize_typed_array_from_typed_array(
                        cx,
                        proto,
                        argument.as_typed_array(),
                    );
                } else if let Some(argument) = argument.as_array_buffer() {
                    let byte_offset = get_argument(cx, arguments, 1);
                    let length = get_argument(cx, arguments, 2);

                    return Self::initialize_typed_array_from_array_buffer(
                        cx,
                        proto,
                        argument,
                        byte_offset,
                        length,
                    );
                }

                let iterator_key = cx.well_known_symbols.iterator();
                let iterator = get_method(cx, argument.into(), iterator_key)?;

                if let Some(iterator) = iterator {
                    Self::initialize_typed_array_from_list(cx, proto, argument.into(), iterator)
                } else {
                    Self::initialize_typed_array_from_array_like(cx, proto, argument)
                }
            }

            /// AllocateTypedArray (https://tc39.es/ecma262/#sec-allocatetypedarray)
            /// AllocateTypedArrayBuffer (https://tc39.es/ecma262/#sec-allocatetypedarraybuffer)
            fn allocate_with_length(
                cx: Context,
                new_target: Handle<ObjectValue>,
                length: usize,
            ) -> EvalResult<Handle<Value>> {
                let proto = get_prototype_from_constructor(cx, new_target, Intrinsic::$prototype)?;

                Ok(Self::allocate_from_object_with_length(cx, proto, length)?.as_value())
            }

            #[inline]
            fn allocate_from_object_with_length(
                cx: Context,
                proto: Handle<ObjectValue>,
                length: usize,
            ) -> EvalResult<Handle<ObjectValue>> {
                let byte_length = element_size!() * length;

                let array_buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
                let array_buffer = ArrayBufferObject::new(
                    cx,
                    array_buffer_constructor,
                    byte_length,
                    /* max_byte_length */ None,
                    /* data */ None,
                )?;

                let typed_array = $typed_array::new_with_proto(
                    cx,
                    proto,
                    array_buffer,
                    Some(byte_length),
                    0,
                    Some(length),
                );

                Ok(typed_array)
            }

            /// InitializeTypedArrayFromTypedArray (https://tc39.es/ecma262/#sec-initializetypedarrayfromtypedarray)
            fn initialize_typed_array_from_typed_array(
                cx: Context,
                proto: Handle<ObjectValue>,
                source_typed_array: DynTypedArray,
            ) -> EvalResult<Handle<Value>> {
                let source_data = source_typed_array.viewed_array_buffer();
                let source_element_size = source_typed_array.element_size();
                let source_byte_offset = source_typed_array.byte_offset();

                let target_element_size = element_size!();

                let source_record = make_typed_array_with_buffer_witness_record(source_typed_array);
                if is_typed_array_out_of_bounds(&source_record) {
                    return type_error(cx, "typed array is out of bounds");
                }

                let source_array_length = typed_array_length(&source_record);
                let byte_length = source_array_length * element_size!();

                if source_typed_array.kind() == TypedArrayKind::$typed_array {
                    // If arrays have the same type then directly copy array buffer
                    let data =
                        clone_array_buffer(cx, source_data, source_byte_offset, byte_length)?;

                    let typed_array = $typed_array::new_with_proto(
                        cx,
                        proto,
                        data,
                        Some(byte_length),
                        0,
                        Some(source_array_length),
                    );

                    Ok(typed_array.as_value())
                } else {
                    // Otherwise arrays have different type, so allocate buffer that holds the same
                    // number of elements as the source array.
                    let buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
                    let data = ArrayBufferObject::new(
                        cx,
                        buffer_constructor,
                        byte_length,
                        /* max_byte_length */ None,
                        /* data */ None,
                    )?;

                    if source_typed_array.content_type() != $content_type {
                        return type_error(
                            cx,
                            "typed arrays must both contain either numbers or BigInts",
                        );
                    }

                    // Copy elements one at a time from source to target array, converting types
                    let mut source_byte_index = source_byte_offset;
                    let mut target_byte_index = 0;

                    for _ in 0..source_array_length {
                        let value = source_typed_array.read_element_value(
                            cx,
                            *source_data,
                            source_byte_index,
                        );

                        // Convert element to target type. May allocate but may does not invoke
                        // user code.
                        let element_value = $to_element(cx, value)?;

                        $typed_array::write_element(*data, target_byte_index, element_value);

                        source_byte_index += source_element_size;
                        target_byte_index += target_element_size;
                    }

                    let typed_array = $typed_array::new_with_proto(
                        cx,
                        proto,
                        data,
                        Some(byte_length),
                        0,
                        Some(source_array_length),
                    );

                    Ok(typed_array.as_value())
                }
            }

            /// InitializeTypedArrayFromArrayBuffer (https://tc39.es/ecma262/#sec-initializetypedarrayfromarraybuffer)
            fn initialize_typed_array_from_array_buffer(
                cx: Context,
                proto: Handle<ObjectValue>,
                array_buffer: Handle<ArrayBufferObject>,
                byte_offset: Handle<Value>,
                length: Handle<Value>,
            ) -> EvalResult<Handle<Value>> {
                let offset = to_index(cx, byte_offset)?;
                if offset % element_size!() != 0 {
                    return range_error(
                        cx,
                        &format!(
                            "byte offset must be a multiple of {} but found {}",
                            element_size!(),
                            offset
                        ),
                    );
                }

                let buffer_is_fixed_length = array_buffer.is_fixed_length();

                let mut new_length = 0;
                if !length.is_undefined() {
                    new_length = to_index(cx, length)?;
                }

                if array_buffer.is_detached() {
                    return type_error(cx, "cannot create typed array from detached array buffer");
                }

                let byte_length = array_buffer.byte_length();
                let result_new_byte_length;
                let result_new_array_length;

                if length.is_undefined() && !buffer_is_fixed_length {
                    if offset > byte_length {
                        return range_error(cx, "byte offset larger than array buffer length");
                    }

                    result_new_byte_length = None;
                    result_new_array_length = None;
                } else if length.is_undefined() {
                    if byte_length % element_size!() != 0 {
                        return range_error(
                            cx,
                            &format!(
                                "array buffer length must be a multiple of {} but found {}",
                                element_size!(),
                                byte_length
                            ),
                        );
                    }

                    let maybe_negative_new_byte_length = byte_length as i64 - offset as i64;

                    if maybe_negative_new_byte_length < 0 {
                        return range_error(cx, "byte offset larger than array buffer length");
                    }

                    let new_byte_length = maybe_negative_new_byte_length as usize;

                    result_new_byte_length = Some(new_byte_length);
                    result_new_array_length = Some(new_byte_length / element_size!());
                } else {
                    let new_byte_length = new_length * element_size!();

                    if offset + new_byte_length as usize > byte_length {
                        return range_error(cx, "byte offset larger than array buffer length");
                    }

                    result_new_byte_length = Some(new_byte_length);
                    result_new_array_length = Some(new_length);
                };

                let typed_array = $typed_array::new_with_proto(
                    cx,
                    proto,
                    array_buffer,
                    result_new_byte_length,
                    offset,
                    result_new_array_length,
                );

                Ok(typed_array.as_value())
            }

            /// InitializeTypedArrayFromList (https://tc39.es/ecma262/#sec-initializetypedarrayfromlist)
            fn initialize_typed_array_from_list(
                cx: Context,
                proto: Handle<ObjectValue>,
                iterable: Handle<Value>,
                iterator: Handle<ObjectValue>,
            ) -> EvalResult<Handle<Value>> {
                // Collect all values from iterator
                let mut values = vec![];
                iter_iterator_method_values(cx, iterable, iterator, &mut |_, value| {
                    values.push(value);
                    None
                })?;

                // Allocated typed array
                let length = values.len();
                let typed_array_object = Self::allocate_from_object_with_length(cx, proto, length)?;

                // Shared between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);

                // Add each value from iterator into typed array
                for (i, value) in values.into_iter().enumerate() {
                    key.replace(PropertyKey::from_u64(cx, i as u64));
                    set(cx, typed_array_object, key, value, true)?;
                }

                Ok(typed_array_object.as_value())
            }

            /// InitializeTypedArrayFromArrayLike (https://tc39.es/ecma262/#sec-initializetypedarrayfromarraylike)
            fn initialize_typed_array_from_array_like(
                cx: Context,
                proto: Handle<ObjectValue>,
                array_like: Handle<ObjectValue>,
            ) -> EvalResult<Handle<Value>> {
                // Allocated typed array
                let length = length_of_array_like(cx, array_like)?;
                let typed_array_object =
                    Self::allocate_from_object_with_length(cx, proto, length as usize)?;

                // Shared between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);

                // Add each value from array into typed array
                for i in 0..length {
                    key.replace(PropertyKey::from_u64(cx, i));
                    let value = get(cx, array_like, key)?;
                    set(cx, typed_array_object, key, value, true)?;
                }

                Ok(typed_array_object.as_value())
            }
        }

        impl HeapObject for HeapPtr<$typed_array> {
            fn byte_size(&self) -> usize {
                size_of::<$typed_array>()
            }

            fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
                self.visit_object_pointers(visitor);
                visitor.visit_pointer(&mut self.viewed_array_buffer);
            }
        }
    };
}

pub enum CanonicalIndexType {
    /// Key is not a canonical numeric index
    NotAnIndex,
    /// Key is a canonical numeric index but not valid for some reason.
    Invalid,
    /// Key is a valid canonical numeric index, which is an int
    Valid(usize),
}

/// CanonicalNumericIndexString (https://tc39.es/ecma262/#sec-canonicalnumericindexstring)
///
/// Determines if the given key is a canonical numeric index string, and validates that it can be
/// used as the index into a typed array with a particular length.
pub fn canonical_numeric_index_string(
    cx: Context,
    key: Handle<PropertyKey>,
    typed_array: DynTypedArray,
) -> CanonicalIndexType {
    if key.is_array_index() {
        // Fast path for array indices
        let array_index = key.as_array_index() as usize;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return CanonicalIndexType::Invalid;
        }
        let array_length = typed_array_length(&typed_array_record);

        if array_index < array_length {
            CanonicalIndexType::Valid(array_index)
        } else {
            CanonicalIndexType::Invalid
        }
    } else if key.is_string() {
        // Otherwise must convert to number then back to string
        let key_string = key.as_string();
        let number_value = must!(to_number(cx, key_string.into()));

        // If string representations are equal, must be canonical numeric index
        let number_string = must!(to_string(cx, number_value));
        if key_string.eq(&number_string) {
            if !is_integral_number(*number_value) {
                return CanonicalIndexType::Invalid;
            }

            let number = number_value.as_number();
            if number.is_sign_negative() {
                return CanonicalIndexType::Invalid;
            }

            let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
            if is_typed_array_out_of_bounds(&typed_array_record) {
                return CanonicalIndexType::Invalid;
            }
            let array_length = typed_array_length(&typed_array_record);

            let number = number as usize;
            if number >= array_length {
                return CanonicalIndexType::Invalid;
            }

            CanonicalIndexType::Valid(number)
        } else if (*key_string)
            .as_flat()
            .eq(&cx.names.negative_zero.as_string().as_flat())
        {
            // The string "-0" is a canonical numeric index but is never valid as an index
            CanonicalIndexType::Invalid
        } else {
            CanonicalIndexType::NotAnIndex
        }
    } else {
        CanonicalIndexType::NotAnIndex
    }
}
