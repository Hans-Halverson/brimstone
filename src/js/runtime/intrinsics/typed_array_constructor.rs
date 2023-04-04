use crate::{
    js::runtime::{
        abstract_operations::{call_object, get_method, length_of_array_like, set},
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        get,
        intrinsics::typed_array_prototype::typed_array_create_object,
        iterator::iter_iterator_method_values,
        object_value::ObjectValue,
        property::Property,
        type_utilities::{is_callable, is_constructor, to_object},
        value::Value,
        Context, PropertyKey, Realm,
    },
    maybe, must,
};

use super::{intrinsics::Intrinsic, typed_array_prototype::typed_array_create};

// 23.2.1 The %TypedArray% Intrinsic Object
pub struct TypedArrayConstructor;

impl TypedArrayConstructor {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            0,
            &cx.names.typed_array(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::TypedArrayPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.intrinsic_func(cx, &cx.names.from(), Self::from, 1, realm);
        func.intrinsic_func(cx, &cx.names.of(), Self::of, 0, realm);

        // 23.2.2.4 get %TypedArray% [ @@species ]
        let species_key = PropertyKey::symbol(cx.well_known_symbols.species);
        func.intrinsic_getter(cx, &species_key, Self::get_species, realm);

        func.into()
    }

    // 23.2.1.1 %TypedArray%
    fn construct(
        cx: &mut Context,
        _: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        type_error_(cx, "TypedArray constructor is abstract and cannot be called")
    }

    // 23.2.2.1 %TypedArray%.from
    fn from(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if !is_constructor(this_value) {
            return type_error_(cx, "TypedArray.from must be called on constructor");
        }

        let this_constructor = this_value.as_object();

        let map_function = {
            let argument = get_argument(arguments, 1);
            if argument.is_undefined() {
                None
            } else if !is_callable(argument) {
                return type_error_(cx, "map function must be a function");
            } else {
                Some(argument.as_object())
            }
        };

        let source = get_argument(arguments, 0);
        let this_argument = get_argument(arguments, 2);

        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        let iterator = maybe!(get_method(cx, source, &iterator_key));

        // If source is iterable then add all values from iterator
        if let Some(iterator) = iterator {
            // Collect all values from iterator
            let mut values = vec![];
            let completion = iter_iterator_method_values(cx, source, iterator, &mut |_, value| {
                values.push(value);
                None
            });

            maybe!(completion.into_eval_result());

            let length = values.len();

            let target_object = maybe!(typed_array_create_object(
                cx,
                this_constructor,
                &[Value::from(length)],
                Some(length)
            ));

            for (i, value) in values.into_iter().enumerate() {
                let key = PropertyKey::from_u64(cx, i as u64);

                let value = if let Some(map_function) = map_function {
                    maybe!(call_object(cx, map_function, this_argument, &[value, Value::from(i)]))
                } else {
                    value
                };

                maybe!(set(cx, target_object, &key, value, true));
            }

            return target_object.into();
        }

        // Otherwise treat source like an array and add its values
        let array_like = must!(to_object(cx, source));
        let length = maybe!(length_of_array_like(cx, array_like)) as usize;

        let target_object = maybe!(typed_array_create_object(
            cx,
            this_constructor,
            &[Value::from(length)],
            Some(length)
        ));

        for i in 0..length {
            let key = PropertyKey::from_u64(cx, i as u64);

            let value = maybe!(get(cx, array_like, &key));

            let value = if let Some(map_function) = map_function {
                maybe!(call_object(cx, map_function, this_argument, &[value, Value::from(i)]))
            } else {
                value
            };

            maybe!(set(cx, target_object, &key, value, true));
        }

        target_object.into()
    }

    // 23.2.2.2 %TypedArray%.of
    fn of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if !is_constructor(this_value) {
            return type_error_(cx, "TypedArray.of must be called on constructor");
        }

        let this_constructor = this_value.as_object();
        let length = arguments.len();

        let typed_array =
            maybe!(typed_array_create(cx, this_constructor, &[Value::from(length)], Some(length)));
        let object = typed_array.into_object_value();

        for (i, value) in arguments.iter().enumerate() {
            let key = PropertyKey::from_u64(cx, i as u64);
            maybe!(set(cx, object, &key, *value, true));
        }

        object.into()
    }

    // 23.2.2.4 get %TypedArray% [ @@species ]
    fn get_species(
        _: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_value.into()
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
                viewed_array_buffer: Gc<ArrayBufferObject>,
                byte_length: usize,
                byte_offset: usize,
                array_length: usize,
            }
        }

        impl $typed_array {
            fn new_with_proto(
                cx: &mut Context,
                proto: Gc<ObjectValue>,
                viewed_array_buffer: Gc<ArrayBufferObject>,
                byte_length: usize,
                byte_offset: usize,
                array_length: usize,
            ) -> Gc<ObjectValue> {
                let mut object = cx.heap.alloc_uninit::<$typed_array>();
                object.descriptor = cx.base_descriptors.get(ObjectKind::$typed_array);

                object_ordinary_init(object.object_mut(), proto);

                object.viewed_array_buffer = viewed_array_buffer;
                object.byte_length = byte_length;
                object.byte_offset = byte_offset;
                object.array_length = array_length;

                object.into()
            }

            #[inline]
            fn write_element(
                mut array_buffer: Gc<ArrayBufferObject>,
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
        impl Object for $typed_array {
            // 10.4.5.1 [[GetOwnProperty]]
            fn get_own_property(
                &self,
                cx: &mut Context,
                key: &PropertyKey,
            ) -> EvalResult<Option<PropertyDescriptor>> {
                match canonical_numeric_index_string(key) {
                    None => ordinary_get_own_property(self.object(), key).into(),
                    Some(index) => {
                        let array_buffer = self.viewed_array_buffer;
                        if array_buffer.is_detached() || (index as usize) >= self.array_length {
                            return None.into();
                        }

                        let byte_index = (index as usize) * element_size!() + self.byte_offset;

                        let value = self.read_element_value(cx, array_buffer, byte_index);

                        let desc = PropertyDescriptor::data(value, true, true, true);

                        (Some(desc)).into()
                    }
                }
            }

            // 10.4.5.2 [[HasProperty]]
            fn has_property(&self, cx: &mut Context, key: &PropertyKey) -> EvalResult<bool> {
                match canonical_numeric_index_string(key) {
                    None => ordinary_has_property(cx, self.into(), key),
                    Some(index) => {
                        let is_valid_index = !self.viewed_array_buffer.is_detached()
                            && (index as usize) < self.array_length;

                        is_valid_index.into()
                    }
                }
            }

            // 10.4.5.3 [[DefineOwnProperty]]
            fn define_own_property(
                &mut self,
                cx: &mut Context,
                key: &PropertyKey,
                desc: PropertyDescriptor,
            ) -> EvalResult<bool> {
                match canonical_numeric_index_string(key) {
                    None => ordinary_define_own_property(cx, self.into(), key, desc),
                    Some(index) => {
                        let array_buffer = self.viewed_array_buffer;
                        if array_buffer.is_detached() || (index as usize) >= self.array_length {
                            return false.into();
                        }

                        if let Some(false) = desc.is_configurable {
                            return false.into();
                        } else if let Some(false) = desc.is_enumerable {
                            return false.into();
                        } else if desc.is_accessor_descriptor() {
                            return false.into();
                        } else if let Some(false) = desc.is_writable {
                            return false.into();
                        }

                        if let Some(value) = desc.value {
                            let element_value = maybe!($to_element(cx, value));

                            // The element conversion could have detached the array buffer as a side
                            // effect, so check again.
                            if !array_buffer.is_detached() {
                                let byte_index =
                                    (index as usize) * element_size!() + self.byte_offset;

                                Self::write_element(array_buffer, byte_index, element_value);
                            }
                        }

                        true.into()
                    }
                }
            }

            // 10.4.5.4 [[Get]]
            fn get(
                &self,
                cx: &mut Context,
                key: &PropertyKey,
                receiver: Value,
            ) -> EvalResult<Value> {
                match canonical_numeric_index_string(key) {
                    None => ordinary_get(cx, self.into(), key, receiver),
                    Some(index) => {
                        let array_buffer = self.viewed_array_buffer;
                        if array_buffer.is_detached() || (index as usize) >= self.array_length {
                            return Value::undefined().into();
                        }

                        let byte_index = (index as usize) * element_size!() + self.byte_offset;

                        self.read_element_value(cx, array_buffer, byte_index).into()
                    }
                }
            }

            // 10.4.5.5 [[Set]]
            fn set(
                &mut self,
                cx: &mut Context,
                key: &PropertyKey,
                value: Value,
                receiver: Value,
            ) -> EvalResult<bool> {
                match canonical_numeric_index_string(key) {
                    None => ordinary_set(cx, self.into(), key, value, receiver),
                    Some(index) => {
                        let element_value = maybe!($to_element(cx, value));

                        let array_buffer = self.viewed_array_buffer;
                        if array_buffer.is_detached() || (index as usize) >= self.array_length {
                            return true.into();
                        }

                        let byte_index = (index as usize) * element_size!() + self.byte_offset;

                        Self::write_element(array_buffer, byte_index, element_value);

                        true.into()
                    }
                }
            }

            // 10.4.5.6 [[Delete]]
            fn delete(&mut self, cx: &mut Context, key: &PropertyKey) -> EvalResult<bool> {
                match canonical_numeric_index_string(key) {
                    None => ordinary_delete(cx, self.into(), key),
                    Some(index) => {
                        let is_invalid_index = self.viewed_array_buffer.is_detached()
                            || (index as usize) >= self.array_length;

                        is_invalid_index.into()
                    }
                }
            }

            // 10.4.5.7 [[OwnPropertyKeys]]
            fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<Value>> {
                let mut keys = vec![];

                if !self.viewed_array_buffer.is_detached() {
                    for i in 0..self.array_length {
                        let index_string = cx.heap.alloc_string(i.to_string());
                        keys.push(Value::string(index_string));
                    }
                }

                ordinary_own_string_symbol_property_keys(cx, self.object(), &mut keys);

                keys.into()
            }

            fn is_typed_array(&self) -> bool {
                true
            }

            fn as_typed_array(&self) -> Gc<dyn TypedArray> {
                Gc::from_ptr(self as *const dyn TypedArray as *mut dyn TypedArray)
            }
        }

        impl TypedArray for $typed_array {
            fn array_length(&self) -> usize {
                self.array_length
            }

            fn byte_length(&self) -> usize {
                self.byte_length
            }

            fn byte_offset(&self) -> usize {
                self.byte_offset
            }

            fn viewed_array_buffer(&self) -> Gc<ArrayBufferObject> {
                self.viewed_array_buffer
            }

            fn name(&self, cx: &mut Context) -> Gc<StringValue> {
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
                cx: &mut Context,
                mut array_buffer: Gc<ArrayBufferObject>,
                byte_index: usize,
            ) -> Value {
                let element = unsafe {
                    let byte_ptr = array_buffer.data().as_ptr().add(byte_index);
                    byte_ptr.cast::<$element_type>().read()
                };

                $from_element(cx, element)
            }

            #[inline]
            fn into_object_value(&self) -> Gc<ObjectValue> {
                self.into()
            }
        }

        pub struct $constructor;

        impl $constructor {
            // 23.2.6 Properties of the TypedArray Constructors
            pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
                let prototype = realm.get_intrinsic(Intrinsic::TypedArrayConstructor);
                let mut func = BuiltinFunction::create(
                    cx,
                    Self::construct,
                    3,
                    &cx.names.$rust_name(),
                    Some(realm),
                    Some(prototype),
                    None,
                );

                func.set_is_constructor();
                func.set_property(
                    &cx.names.prototype(),
                    Property::data(
                        realm.get_intrinsic(Intrinsic::$prototype).into(),
                        false,
                        false,
                        false,
                    ),
                );

                func.set_property(
                    &cx.names.bytes_per_element(),
                    Property::data(Value::smi(element_size!() as i32), false, false, false),
                );

                func
            }

            // 23.2.5.1 TypedArray
            fn construct(
                cx: &mut Context,
                _: Value,
                arguments: &[Value],
                new_target: Option<Gc<ObjectValue>>,
            ) -> EvalResult<Value> {
                let new_target = if let Some(new_target) = new_target {
                    new_target
                } else {
                    return type_error_(
                        cx,
                        &format!("{} constructor must be called with new", cx.names.$rust_name()),
                    );
                };

                if arguments.is_empty() {
                    return Self::allocate_with_length(cx, new_target, 0);
                }

                let proto =
                    maybe!(get_prototype_from_constructor(cx, new_target, Intrinsic::$prototype));

                let argument = get_argument(arguments, 0);
                if !argument.is_object() {
                    let length = maybe!(to_index(cx, argument));
                    return Self::allocate_with_length(cx, new_target, length);
                }

                let argument = argument.as_object();
                if argument.is_typed_array() {
                    return Self::initialize_typed_array_from_typed_array(
                        cx,
                        proto,
                        argument.as_typed_array(),
                    );
                } else if argument.is_array_buffer() {
                    let byte_offset = get_argument(arguments, 1);
                    let length = get_argument(arguments, 2);

                    return Self::initialize_typed_array_from_array_buffer(
                        cx,
                        proto,
                        argument.cast::<ArrayBufferObject>(),
                        byte_offset,
                        length,
                    );
                }

                let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
                let iterator = maybe!(get_method(cx, argument.into(), &iterator_key));

                if let Some(iterator) = iterator {
                    Self::initialize_typed_array_from_list(cx, proto, argument.into(), iterator)
                } else {
                    Self::initialize_typed_array_from_array_like(cx, proto, argument)
                }
            }

            // 23.2.5.1.1 AllocateTypedArray
            // 23.2.5.1.6 AllocateTypedArrayBuffer
            fn allocate_with_length(
                cx: &mut Context,
                new_target: Gc<ObjectValue>,
                length: usize,
            ) -> EvalResult<Value> {
                let proto =
                    maybe!(get_prototype_from_constructor(cx, new_target, Intrinsic::$prototype));

                maybe!(Self::allocate_from_object_with_length(cx, proto, length)).into()
            }

            #[inline]
            fn allocate_from_object_with_length(
                cx: &mut Context,
                proto: Gc<ObjectValue>,
                length: usize,
            ) -> EvalResult<Gc<ObjectValue>> {
                let byte_length = element_size!() * length;

                let array_buffer_constructor = cx
                    .current_realm()
                    .get_intrinsic(Intrinsic::ArrayBufferConstructor);
                let array_buffer =
                    maybe!(ArrayBufferObject::new(cx, array_buffer_constructor, byte_length));

                $typed_array::new_with_proto(cx, proto, array_buffer, byte_length, 0, length).into()
            }

            // 23.2.5.1.2 InitializeTypedArrayFromTypedArray
            fn initialize_typed_array_from_typed_array(
                cx: &mut Context,
                proto: Gc<ObjectValue>,
                source_typed_array: Gc<dyn TypedArray>,
            ) -> EvalResult<Value> {
                let source_data = source_typed_array.viewed_array_buffer();
                if source_data.is_detached() {
                    return type_error_(cx, "cannot create typed array from detached array buffer");
                }

                // TODO: Handle SharedArrayBuffers

                let source_byte_offset = source_typed_array.byte_offset();
                let source_array_length = source_typed_array.array_length();
                let byte_length = source_array_length * element_size!();

                if source_typed_array.kind() == TypedArrayKind::$typed_array {
                    // If arrays have the same type then directly copy array buffer
                    let data = maybe!(clone_array_buffer(
                        cx,
                        source_data,
                        source_byte_offset,
                        byte_length,
                    ));

                    $typed_array::new_with_proto(
                        cx,
                        proto,
                        data,
                        byte_length,
                        0,
                        source_array_length,
                    )
                    .into()
                } else {
                    // Otherwise arrays have different type, so allocate buffer that holds the same
                    // number of elements as the source array.
                    let buffer_constructor = cx
                        .current_realm()
                        .get_intrinsic(Intrinsic::ArrayBufferConstructor);
                    let data = maybe!(ArrayBufferObject::new(cx, buffer_constructor, byte_length));

                    if source_data.is_detached() {
                        return type_error_(
                            cx,
                            "cannot create typed array from detached array buffer",
                        );
                    }

                    if source_typed_array.content_type() != $content_type {
                        return type_error_(
                            cx,
                            "typed arrays must both contain either numbers or BigInts",
                        );
                    }

                    // Copy elements one at a time from source to target array, converting types
                    let mut source_byte_index = source_byte_offset;
                    let mut target_byte_index = 0;

                    for _ in 0..source_array_length {
                        // Read element from source array
                        let value = source_typed_array.read_element_value(
                            cx,
                            source_data,
                            source_byte_index,
                        );

                        // Convert element to target type
                        let target_element_value = maybe!($to_element(cx, value));

                        // Write element to target array
                        $typed_array::write_element(data, target_byte_index, target_element_value);

                        source_byte_index += element_size!();
                        target_byte_index += element_size!();
                    }

                    $typed_array::new_with_proto(
                        cx,
                        proto,
                        data,
                        byte_length,
                        0,
                        source_array_length,
                    )
                    .into()
                }
            }

            // 23.2.5.1.3 InitializeTypedArrayFromArrayBuffer
            fn initialize_typed_array_from_array_buffer(
                cx: &mut Context,
                proto: Gc<ObjectValue>,
                mut array_buffer: Gc<ArrayBufferObject>,
                byte_offset: Value,
                length: Value,
            ) -> EvalResult<Value> {
                let offset = maybe!(to_index(cx, byte_offset));
                if offset % element_size!() != 0 {
                    return range_error_(
                        cx,
                        &format!(
                            "byte offset must be a multiple of {} but found {}",
                            element_size!(),
                            offset
                        ),
                    );
                }

                let mut new_length = 0;
                if !length.is_undefined() {
                    new_length = maybe!(to_index(cx, length));
                }

                if array_buffer.is_detached() {
                    return type_error_(cx, "cannot create typed array from detached array buffer");
                }

                let byte_length = array_buffer.data().len();
                let new_byte_length;

                if length.is_undefined() {
                    if byte_length % element_size!() != 0 {
                        return range_error_(
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
                        return range_error_(cx, "byte offset larger than array buffer length");
                    }

                    new_byte_length = maybe_negative_new_byte_length as usize;
                } else {
                    new_byte_length = new_length * element_size!();

                    if offset + new_byte_length as usize > byte_length {
                        return range_error_(cx, "byte offset larger than array buffer length");
                    }
                };

                $typed_array::new_with_proto(
                    cx,
                    proto,
                    array_buffer,
                    new_byte_length,
                    offset,
                    new_byte_length / element_size!(),
                )
                .into()
            }

            // 23.2.5.1.4 InitializeTypedArrayFromList
            fn initialize_typed_array_from_list(
                cx: &mut Context,
                proto: Gc<ObjectValue>,
                iterable: Value,
                iterator: Gc<ObjectValue>,
            ) -> EvalResult<Value> {
                // Collect all values from iterator
                let mut values = vec![];
                let completion =
                    iter_iterator_method_values(cx, iterable, iterator, &mut |_, value| {
                        values.push(value);
                        None
                    });

                maybe!(completion.into_eval_result());

                // Allocated typed array
                let length = values.len();
                let typed_array_object =
                    maybe!(Self::allocate_from_object_with_length(cx, proto, length));

                // Add each value from iterator into typed array
                for (i, value) in values.into_iter().enumerate() {
                    let key = PropertyKey::from_u64(cx, i as u64);
                    maybe!(set(cx, typed_array_object, &key, value, true));
                }

                typed_array_object.into()
            }

            // 23.2.5.1.5 InitializeTypedArrayFromArrayLike
            fn initialize_typed_array_from_array_like(
                cx: &mut Context,
                proto: Gc<ObjectValue>,
                array_like: Gc<ObjectValue>,
            ) -> EvalResult<Value> {
                // Allocated typed array
                let length = maybe!(length_of_array_like(cx, array_like));
                let typed_array_object =
                    maybe!(Self::allocate_from_object_with_length(cx, proto, length as usize));

                // Add each value from array into typed array
                for i in 0..length {
                    let key = PropertyKey::from_u64(cx, i);
                    let value = maybe!(get(cx, array_like, &key));
                    maybe!(set(cx, typed_array_object, &key, value, true));
                }

                typed_array_object.into()
            }
        }
    };
}
