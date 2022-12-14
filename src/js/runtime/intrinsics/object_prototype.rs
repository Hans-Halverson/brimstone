use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        abstract_operations::{get, has_own_property},
        completion::EvalResult,
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{
            extract_object_vtable, set_immutable_prototype, Object, ObjectValue, ObjectValueVtable,
        },
        ordinary_object::OrdinaryObject,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::{same_object_value, to_object, to_property_key},
        value::Value,
        Context,
    },
    maybe,
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

    // 20.1.3 Properties of the Object Prototype Object
    pub fn initialize(&mut self, cx: &mut Context, realm: Gc<Realm>) {
        self.object = OrdinaryObject::new(None, true);

        // Constructor property is added once ObjectConstructor has been created
        self.object
            .intrinsic_func(cx, "hasOwnProperty", Self::has_own_property, 1, realm);
        self.object
            .intrinsic_func(cx, "isPrototypeOf", Self::is_prototype_of, 1, realm);
        self.object.intrinsic_func(
            cx,
            "propertyIsEnumerable",
            Self::property_is_enumerable,
            1,
            realm,
        );
        self.object
            .intrinsic_func(cx, "valueOf", Self::value_of, 0, realm);
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

    // 20.1.3.2 Object.prototype.hasOwnProperty
    fn has_own_property(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let property_key = to_property_key(get_argument(arguments, 0));
        let this_object = maybe!(to_object(cx, this_value));

        maybe!(has_own_property(this_object, property_key.str())).into()
    }

    // 20.1.3.3 Object.prototype.isPrototypeOf
    fn is_prototype_of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_object() {
            return false.into();
        }

        let this_object = maybe!(to_object(cx, this_value));

        // Walk prototype chain, seeing if this_object is in the prototype chain
        let mut current_value = value.as_object();
        loop {
            match maybe!(current_value.get_prototype_of()) {
                None => return false.into(),
                Some(prototype) => {
                    if same_object_value(this_object, prototype) {
                        return true.into();
                    }

                    current_value = prototype;
                }
            }
        }
    }

    // 20.1.3.4 Object.prototype.propertyIsEnumerable
    fn property_is_enumerable(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let property_key = to_property_key(get_argument(arguments, 0));
        let this_object = maybe!(to_object(cx, this_value));

        match maybe!(this_object.get_own_property(property_key.str())) {
            None => false.into(),
            Some(desc) => desc.is_enumerable().into(),
        }
    }

    // 20.1.3.7 Object.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        maybe!(to_object(cx, this_value)).into()
    }

    // 20.1.3.6 Object.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if this_value.is_undefined() {
            return cx.heap.alloc_string("[object Undefined]".to_owned()).into();
        } else if this_value.is_null() {
            return cx.heap.alloc_string("[object Null]".to_owned()).into();
        }

        let object = maybe!(to_object(cx, this_value));

        // TODO: Change to symbol once symbols are implemented
        let tag = maybe!(get(cx, object, "@@toStringTag"));

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
    fn set_prototype_of(&mut self, proto: Option<Gc<ObjectValue>>) -> EvalResult<bool> {
        set_immutable_prototype(self.into(), proto)
    }
}

impl_gc_into!(ObjectPrototype, ObjectValue);
