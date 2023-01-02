use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        abstract_operations::get,
        completion::AbstractResult,
        gc::{Gc, GcDeref},
        object_value::{
            extract_object_vtable, set_immutable_prototype, Object, ObjectValue, ObjectValueVtable,
        },
        ordinary_object::OrdinaryObject,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::to_object,
        value::Value,
        Context,
    },
    maybe_,
};

#[repr(C)]
pub struct ObjectPrototype {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
}

impl GcDeref for ObjectPrototype {}

impl ObjectPrototype {
    const VTABLE: *const () = extract_object_vtable::<ObjectPrototype>();

    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: &mut Context) -> Gc<ObjectPrototype> {
        let object_prototype = ObjectPrototype {
            _vtable: Self::VTABLE,
            object: OrdinaryObject::new_uninit(),
        };
        cx.heap.alloc(object_prototype)
    }

    pub fn initialize(&mut self, cx: &mut Context, realm: Gc<Realm>) {
        self.object = OrdinaryObject::new(None, true);
        self.object
            .intrinsic_func(cx, "toString", Self::to_string, 0, realm);
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }

    // 20.1.3.6 Object.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> AbstractResult<Value> {
        if this_value.is_undefined() {
            return cx.heap.alloc_string("[object Undefined]".to_owned()).into();
        } else if this_value.is_null() {
            return cx.heap.alloc_string("[object Null]".to_owned()).into();
        }

        let object = maybe_!(to_object(cx, this_value));

        // TODO: Change to symbol once symbols are implemented
        let tag = maybe_!(get(cx, object, "@@toStringTag"));

        let tag_string = if tag.is_string() {
            return cx
                .heap
                .alloc_string(format!("[object {}]", tag.as_string().str()))
                .into();
        } else if object.is_array() {
            "Array"
        } else if object.is_callable() {
            "Function"
        } else if object.is_error() {
            "Error"
        } else if object.is_bool_object() {
            "Boolean"
        } else if object.is_number_object() {
            "Number"
        } else if object.is_string_object() {
            "String"
        } else if object.is_date_object() {
            "Date"
        } else if object.is_regexp_object() {
            "RegExp"
        } else {
            "Object"
        };

        cx.heap
            .alloc_string(format!("[object {}]", tag_string))
            .into()
    }
}

#[wrap_ordinary_object]
impl Object for ObjectPrototype {
    fn set_prototype_of(&mut self, proto: Option<Gc<ObjectValue>>) -> AbstractResult<bool> {
        set_immutable_prototype(self.into(), proto)
    }
}

impl_gc_into!(ObjectPrototype, ObjectValue);
