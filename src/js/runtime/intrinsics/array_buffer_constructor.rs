use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, HeapPtr, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            array_buffer_object::ArrayBufferObject, data_view_object::DataViewObject,
            intrinsics::Intrinsic, rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::to_index,
    },
    runtime_fn,
};

pub struct ArrayBufferConstructor;

impl ArrayBufferConstructor {
    /// Properties of the ArrayBuffer Constructor (https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::ArrayBufferConstructor_construct,
            1,
            cx.names.array_buffer(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::ArrayBufferPrototype)?;

        intrinsic_methods!(cx, builder, {
            is_view ArrayBufferConstructor_is_view (1),
        });

        // get ArrayBuffer [ @@species ] (https://tc39.es/ecma262/#sec-get-arraybuffer-%symbol.species%)
        builder.getter(cx.symbols.species(), RuntimeFunction::ReturnThis)?;

        builder.build()
    }

    runtime_fn! {
    /// ArrayBuffer (https://tc39.es/ecma262/#sec-arraybuffer-length)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "ArrayBuffer constructor must be called with new");
        };

        let byte_length_arg = arguments.get(cx, 0);
        let byte_length = to_index(cx, byte_length_arg)?;

        let options_arg = arguments.get(cx, 1);
        let max_byte_length = get_array_buffer_max_byte_length_option(cx, options_arg)?;

        Ok(ArrayBufferObject::new(
            cx,
            new_target,
            byte_length,
            max_byte_length,
            /* data */ None,
        )?
        .as_value())
    }}

    runtime_fn! {
    /// ArrayBuffer.isView (https://tc39.es/ecma262/#sec-arraybuffer.isview)
    fn is_view(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        if !value.is_object() {
            return Ok(cx.bool(false));
        }

        let object = value.as_object();
        let is_view = object.is::<DataViewObject>() || object.is_typed_array();

        Ok(cx.bool(is_view))
    }}
}

/// ArrayBufferCopyAndDetach (https://tc39.es/ecma262/#sec-arraybuffercopyanddetach)
pub fn array_buffer_copy_and_detach(
    cx: Context,
    mut array_buffer: Handle<ArrayBufferObject>,
    new_length: Handle<Value>,
    to_fixed: bool,
) -> EvalResult<Handle<ArrayBufferObject>> {
    let new_byte_length = if new_length.is_undefined() {
        array_buffer.byte_length()
    } else {
        to_index(cx, new_length)?
    };

    throw_if_detached(cx, *array_buffer)?;

    // Can only remain auto-resizable if ArrayBuffer was already resizable and we are not forced
    // to fix the length.
    let new_max_byte_length = if !to_fixed && !array_buffer.is_fixed_length() {
        array_buffer.max_byte_length()
    } else {
        None
    };

    // Create a new ArrayBuffer with the new length and a direct reference to the old buffer's data
    let array_buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
    let new_buffer = ArrayBufferObject::new(
        cx,
        array_buffer_constructor,
        new_byte_length,
        new_max_byte_length,
        array_buffer.data_opt(),
    )?;

    // Finally detach the original buffer
    array_buffer.detach();

    Ok(new_buffer)
}

/// CloneArrayBuffer (https://tc39.es/ecma262/#sec-clonearraybuffer)
pub fn clone_array_buffer(
    cx: Context,
    source_buffer: Handle<ArrayBufferObject>,
    source_byte_offset: usize,
    source_length: usize,
) -> EvalResult<Handle<ArrayBufferObject>> {
    let array_buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
    let mut target_buffer = ArrayBufferObject::new(
        cx,
        array_buffer_constructor,
        source_length,
        /* max_byte_length */ None,
        /* data */ None,
    )?;

    // Copy a portion of the source buffer after the given offset to the target buffer
    let source_buffer_view =
        &source_buffer.data()[source_byte_offset..(source_byte_offset + source_length)];

    target_buffer.data_mut().copy_from_slice(source_buffer_view);

    Ok(target_buffer)
}

/// GetArrayBufferMaxByteLengthOption (https://tc39.es/ecma262/#sec-getarraybuffermaxbytelengthoption)
fn get_array_buffer_max_byte_length_option(
    cx: Context,
    options: Handle<Value>,
) -> EvalResult<Option<usize>> {
    if !options.is_object() {
        return Ok(None);
    }

    let options = options.as_object();
    let max_byte_length = get(cx, options, cx.names.max_byte_length())?;

    if max_byte_length.is_undefined() {
        return Ok(None);
    }

    Ok(Some(to_index(cx, max_byte_length)?))
}

#[inline]
pub fn throw_if_detached(cx: Context, array_buffer: HeapPtr<ArrayBufferObject>) -> EvalResult<()> {
    if array_buffer.is_detached() {
        type_error(cx, "array buffer is detached")
    } else {
        Ok(())
    }
}
