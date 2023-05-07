use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        gc::Gc,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::to_index,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 4GB max array buffer size
const MAX_ARRAY_BUFFER_SIZE: usize = 1 << 32;

// 25.1 ArrayBuffer Objects
extend_object! {
    pub struct ArrayBufferObject {
        data: Vec<u8>,
        is_detached: bool,
    }
}

impl ArrayBufferObject {
    // 25.1.2.1 AllocateArrayBuffer
    pub fn new(
        cx: &mut Context,
        constructor: Gc<ObjectValue>,
        byte_length: usize,
    ) -> EvalResult<Gc<ArrayBufferObject>> {
        let mut object = maybe!(object_create_from_constructor::<ArrayBufferObject>(
            cx,
            constructor,
            ObjectKind::ArrayBufferObject,
            Intrinsic::ArrayBufferPrototype
        ));

        object.is_detached = false;

        // Temporarily fill default values so object is fully initialized before GC may be triggered
        object.data = vec![];

        if byte_length > MAX_ARRAY_BUFFER_SIZE {
            return range_error_(
                cx,
                &format!("cannot allocate array buffer of size {}", byte_length),
            );
        }

        object.data = vec![0; byte_length];

        object.into()
    }

    pub fn data(&mut self) -> &mut [u8] {
        &mut self.data
    }

    pub fn is_detached(&self) -> bool {
        self.is_detached
    }

    // 25.1.2.3 DetachArrayBuffer
    pub fn detach(&mut self) {
        self.data = Vec::new();
        self.is_detached = true;
    }
}

pub struct ArrayBufferConstructor;

impl ArrayBufferConstructor {
    // 25.1.4 Properties of the ArrayBuffer Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.array_buffer(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::ArrayBufferPrototype).into(),
                false,
                false,
                false,
            ),
        );

        let species_key = PropertyKey::symbol(cx.well_known_symbols.species);
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 25.1.3.1 ArrayBuffer
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error_(cx, "ArrayBuffer constructor must be called with new");
        };

        let byte_length = maybe!(to_index(cx, get_argument(arguments, 0)));

        maybe!(ArrayBufferObject::new(cx, new_target, byte_length)).into()
    }

    // 25.1.4.3 get ArrayBuffer [ @@species ]
    fn get_species(
        _: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_value.into()
    }
}

// 25.1.2.4 CloneArrayBuffer
pub fn clone_array_buffer(
    cx: &mut Context,
    mut source_buffer: Gc<ArrayBufferObject>,
    source_byte_offset: usize,
    source_length: usize,
) -> EvalResult<Gc<ArrayBufferObject>> {
    let array_buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
    let mut target_buffer =
        maybe!(ArrayBufferObject::new(cx, array_buffer_constructor, source_length));

    if source_buffer.is_detached() {
        return type_error_(cx, "detached array buffer cannot be cloned");
    }

    // Copy a portion of the source buffer after the given offset to the target buffer
    let source_buffer_view =
        &source_buffer.data()[source_byte_offset..(source_byte_offset + source_length)];

    target_buffer.data().copy_from_slice(source_buffer_view);

    target_buffer.into()
}
