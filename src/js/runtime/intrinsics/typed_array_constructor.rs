use crate::js::runtime::{
    builtin_function::BuiltinFunction, completion::EvalResult, error::type_error_, gc::Gc,
    object_value::ObjectValue, property::Property, value::Value, Context, Realm,
};

use super::intrinsics::Intrinsic;

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
}

#[macro_export]
macro_rules! create_typed_array_constructor {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $prototype:ident, $constructor:ident, $to_element:ident, $from_element:ident) => {
        #[repr(C)]
        pub struct $typed_array {
            _vtable: ObjectValueVtable,
            object: OrdinaryObject,
            viewed_array_buffer: Gc<ArrayBufferObject>,
            byte_length: usize,
            byte_offset: usize,
            array_length: usize,
        }

        impl_gc_into!($typed_array, ObjectValue);

        impl $typed_array {
            const VTABLE: *const () = extract_object_vtable::<$typed_array>();

            fn new(
                object: OrdinaryObject,
                viewed_array_buffer: Gc<ArrayBufferObject>,
                byte_length: usize,
                byte_offset: usize,
                array_length: usize,
            ) -> $typed_array {
                $typed_array {
                    _vtable: Self::VTABLE,
                    object,
                    viewed_array_buffer,
                    byte_length,
                    byte_offset,
                    array_length,
                }
            }

            #[inline]
            fn object(&self) -> &OrdinaryObject {
                &self.object
            }

            #[inline]
            fn object_mut(&mut self) -> &mut OrdinaryObject {
                &mut self.object
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
                    None => ordinary_get_own_property(&self.object, key).into(),
                    Some(index) => {
                        let mut array_buffer = self.viewed_array_buffer;
                        if array_buffer.is_detached() || (index as usize) >= self.array_length {
                            return None.into();
                        }

                        let byte_index = (index as usize) * std::mem::size_of::<$element_type>()
                            + self.byte_offset;

                        let element = unsafe {
                            let byte_ptr = array_buffer.data().as_ptr().add(byte_index);
                            byte_ptr.cast::<$element_type>().read()
                        };

                        let value = $from_element(cx, element);
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
                        let mut array_buffer = self.viewed_array_buffer;
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

                            let byte_index = (index as usize)
                                * std::mem::size_of::<$element_type>()
                                + self.byte_offset;

                            unsafe {
                                let byte_ptr = array_buffer.data().as_mut_ptr().add(byte_index);
                                let element_ptr = byte_ptr.cast::<$element_type>();

                                element_ptr.write(element_value)
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
                        let mut array_buffer = self.viewed_array_buffer;
                        if array_buffer.is_detached() || (index as usize) >= self.array_length {
                            return Value::undefined().into();
                        }

                        let byte_index = (index as usize) * std::mem::size_of::<$element_type>()
                            + self.byte_offset;

                        let element = unsafe {
                            let byte_ptr = array_buffer.data().as_ptr().add(byte_index);
                            byte_ptr.cast::<$element_type>().read()
                        };

                        $from_element(cx, element).into()
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

                        let mut array_buffer = self.viewed_array_buffer;
                        if array_buffer.is_detached() || (index as usize) >= self.array_length {
                            return true.into();
                        }

                        let byte_index = (index as usize) * std::mem::size_of::<$element_type>()
                            + self.byte_offset;

                        unsafe {
                            let byte_ptr = array_buffer.data().as_mut_ptr().add(byte_index);
                            let element_ptr = byte_ptr.cast::<$element_type>();

                            element_ptr.write(element_value)
                        }

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

                ordinary_own_string_symbol_property_keys(cx, &self.object, &mut keys);

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
                if std::mem::size_of::<$element_type>() == 8 {
                    ContentType::BigInt
                } else {
                    ContentType::Number
                }
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
                    Property::data(
                        Value::smi(std::mem::size_of::<$element_type>() as i32),
                        false,
                        false,
                        false,
                    ),
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

                let argument = get_argument(arguments, 0);
                if !argument.is_object() {
                    let length = maybe!(to_index(cx, argument));
                    return Self::allocate_with_length(cx, new_target, length);
                }

                unimplemented!(
                    "{} constructor from ArrayBuffer, TypedArray, Array, or iterable",
                    &cx.names.$rust_name()
                );
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
                let object = ordinary_object_create(proto);

                let byte_length = std::mem::size_of::<$element_type>() * length;

                let array_buffer_constructor = cx
                    .current_realm()
                    .get_intrinsic(Intrinsic::ArrayBufferConstructor);
                let array_buffer =
                    maybe!(ArrayBufferObject::new(cx, array_buffer_constructor, byte_length));

                let typed_array = $typed_array::new(object, array_buffer, byte_length, 0, length);
                let typed_array_object: Gc<ObjectValue> = cx.heap.alloc(typed_array).into();

                return typed_array_object.into();
            }
        }
    };
}
