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

use super::intrinsics::Intrinsic;

// 4GB max array buffer size
const MAX_ARRAY_BUFFER_SIZE: usize = 1 << 32;

// 25.1 ArrayBuffer Objects
#[repr(C)]
pub struct ArrayBufferObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    data: Vec<u8>,
    is_detached: bool,
}

impl GcDeref for ArrayBufferObject {}

impl_gc_into!(ArrayBufferObject, ObjectValue);

impl ArrayBufferObject {
    const VTABLE: *const () = extract_object_vtable::<ArrayBufferObject>();

    // 25.1.2.1 AllocateArrayBuffer
    pub fn new(
        cx: &mut Context,
        constructor: Gc<ObjectValue>,
        byte_length: usize,
    ) -> EvalResult<Gc<ArrayBufferObject>> {
        let object = maybe!(ordinary_create_from_constructor(
            cx,
            constructor,
            Intrinsic::ArrayBufferPrototype
        ));

        if byte_length > MAX_ARRAY_BUFFER_SIZE {
            return range_error_(
                cx,
                &format!("cannot allocate array buffer of size {}", byte_length),
            );
        }

        let mut data = Vec::with_capacity(byte_length);
        unsafe { data.set_len(byte_length) };

        let array_buffer =
            ArrayBufferObject { _vtable: Self::VTABLE, object, data, is_detached: false };

        cx.heap.alloc(array_buffer).into()
    }

    pub fn data(&mut self) -> &mut [u8] {
        &mut self.data
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
impl Object for ArrayBufferObject {
    fn is_array_buffer(&self) -> bool {
        true
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
            &cx.names.array_buffer(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::ErrorPrototype).into(),
                false,
                false,
                false,
            ),
        );

        let species_key = PropertyKey::symbol(cx.well_known_symbols.species);
        func.intrinsic_getter(cx, &species_key, Self::get_species, realm);

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
