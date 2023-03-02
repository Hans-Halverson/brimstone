use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        error::{range_error_, type_error_},
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{ordinary_create_from_constructor, OrdinaryObject},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::to_index,
        value::Value,
        Context,
    },
    maybe,
};

use super::{array_buffer_constructor::ArrayBufferObject, intrinsics::Intrinsic};

// 25.3 DataView Objects
#[repr(C)]
pub struct DataViewObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    viewed_array_buffer: Gc<ArrayBufferObject>,
    byte_length: usize,
    byte_offset: usize,
}

impl GcDeref for DataViewObject {}

impl_gc_into!(DataViewObject, ObjectValue);

impl DataViewObject {
    const VTABLE: *const () = extract_object_vtable::<DataViewObject>();

    pub fn new(
        object: OrdinaryObject,
        viewed_array_buffer: Gc<ArrayBufferObject>,
        byte_length: usize,
        byte_offset: usize,
    ) -> DataViewObject {
        DataViewObject {
            _vtable: Self::VTABLE,
            object,
            viewed_array_buffer,
            byte_length,
            byte_offset,
        }
    }

    pub fn viewed_array_buffer(&self) -> Gc<ArrayBufferObject> {
        self.viewed_array_buffer
    }

    pub fn byte_length(&self) -> usize {
        self.byte_length
    }

    pub fn byte_offset(&self) -> usize {
        self.byte_offset
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
impl Object for DataViewObject {
    fn is_data_view(&self) -> bool {
        true
    }
}

pub struct DataViewConstructor;

impl DataViewConstructor {
    // 25.3.3 Properties of the DataView Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            &cx.names.data_view(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::FunctionPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 25.3.2.1 DataView
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error_(cx, "DataView constructor must be called with new");
        };

        let buffer_argument = get_argument(arguments, 0);
        if !buffer_argument.is_object() {
            return type_error_(cx, "first argument must be an array buffer");
        }

        let buffer_object = buffer_argument.as_object();
        if !buffer_object.is_array_buffer() {
            return type_error_(cx, "first argument must be an array buffer");
        }

        let mut buffer_object = buffer_object.cast::<ArrayBufferObject>();

        let offset = maybe!(to_index(cx, get_argument(arguments, 1)));

        if buffer_object.is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        let buffer_byte_length = buffer_object.data().len();
        if offset > buffer_byte_length {
            return range_error_(
                cx,
                &format!(
                    "offset {} must is out of bounds for buffer with byte length {}",
                    offset, buffer_byte_length
                ),
            );
        }

        let byte_length_argument = get_argument(arguments, 2);
        let view_byte_length = if byte_length_argument.is_undefined() {
            buffer_byte_length - offset
        } else {
            let view_byte_length = maybe!(to_index(cx, byte_length_argument));

            if offset + view_byte_length > buffer_byte_length {
                return range_error_(cx, "data view byte length is too large for this buffer");
            }

            view_byte_length
        };

        let object =
            maybe!(ordinary_create_from_constructor(cx, new_target, Intrinsic::DataViewPrototype));

        if buffer_object.is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        let data_view = DataViewObject::new(object, buffer_object, view_byte_length, offset);
        let data_view_object: Gc<ObjectValue> = cx.heap.alloc(data_view).into();

        data_view_object.into()
    }
}
