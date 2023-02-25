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
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $prototype:ident, $constructor:ident) => {
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

                let array_buffer_constructor = cx
                    .current_realm()
                    .get_intrinsic(Intrinsic::ArrayBufferConstructor);
                let array_buffer = maybe!(ArrayBufferObject::new(cx, array_buffer_constructor, 0));

                let byte_length = std::mem::size_of::<$element_type>() * length;

                let typed_array = $typed_array::new(object, array_buffer, byte_length, 0, length);
                let typed_array_object: Gc<ObjectValue> = cx.heap.alloc(typed_array).into();

                return typed_array_object.into();
            }
        }
    };
}
