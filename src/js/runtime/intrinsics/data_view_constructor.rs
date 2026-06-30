use crate::{
    runtime::{
        Context, Handle,
        alloc_error::AllocResult,
        error::{range_error, type_error},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            array_buffer_constructor::throw_if_detached, array_buffer_object::ArrayBufferObject,
            data_view_object::DataViewObject, intrinsics::Intrinsic, rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::to_index,
    },
    runtime_fn,
};

pub struct DataViewConstructor;

impl DataViewConstructor {
    /// Properties of the DataView Constructor (https://tc39.es/ecma262/#sec-properties-of-the-dataview-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::DataViewConstructor_construct,
            1,
            cx.names.data_view(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::DataViewPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// DataView (https://tc39.es/ecma262/#sec-dataview-buffer-byteoffset-bytelength)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "DataView constructor must be called with new");
        };

        let buffer_argument = arguments.get(cx, 0);
        let Some(buffer_object) = buffer_argument.as_opt::<ArrayBufferObject>() else {
            return type_error(cx, "DataView constructor first argument must be an array buffer");
        };

        let offset_arg = arguments.get(cx, 1);
        let offset = to_index(cx, offset_arg)?;

        throw_if_detached(cx, *buffer_object)?;

        let buffer_byte_length = buffer_object.byte_length();
        if offset > buffer_byte_length {
            return range_error(
                cx,
                &format!(
                    "DataView constructor offset {offset} is out of bounds for buffer with byte length {buffer_byte_length}"
                ),
            );
        }

        let byte_length_argument = arguments.get(cx, 2);
        let view_byte_length = if byte_length_argument.is_undefined() {
            if buffer_object.is_fixed_length() {
                Some(buffer_byte_length - offset)
            } else {
                None
            }
        } else {
            let view_byte_length = to_index(cx, byte_length_argument)?;

            if offset + view_byte_length > buffer_byte_length {
                return range_error(
                    cx,
                    "DataView constructor byte length is too large for this buffer",
                );
            }

            Some(view_byte_length)
        };

        let data_view = DataViewObject::new_from_constructor(
            cx,
            new_target,
            buffer_object,
            view_byte_length,
            offset,
        )?;

        // Be sure to check for array buffer detachment since constructor may invoke user code
        throw_if_detached(cx, *buffer_object)?;

        // Also check if underlying buffer was resized during construction and redo bounds checks
        let buffer_byte_length = buffer_object.byte_length();
        if offset > buffer_byte_length {
            return range_error(cx, "DataView constructor offset is out of bounds for buffer");
        }

        if !byte_length_argument.is_undefined() {
            if offset + view_byte_length.unwrap() > buffer_byte_length {
                return range_error(
                    cx,
                    "DataView constructor byte length is too large for this buffer",
                );
            }
        }

        Ok(data_view.as_value())
    }}
}
