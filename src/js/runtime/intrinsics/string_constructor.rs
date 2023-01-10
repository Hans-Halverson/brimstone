use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{
            ordinary_create_from_constructor, ordinary_object_create, OrdinaryObject,
        },
        property::Property,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::to_string,
        value::{StringValue, Value},
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 10.4.3 String Exotic Objects
#[repr(C)]
pub struct StringObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    // The string value wrapped by this object
    string_data: Gc<StringValue>,
}

impl GcDeref for StringObject {}

impl_gc_into!(StringObject, ObjectValue);

impl StringObject {
    const VTABLE: *const () = extract_object_vtable::<StringObject>();

    pub fn new(mut object: OrdinaryObject, string_data: Gc<StringValue>) -> StringObject {
        // String objects have an immutable length property
        let length = string_data.str().len();

        object.set_property(
            String::from("length"),
            Property::data((length as f64).into(), false, false, false),
        );

        StringObject {
            _vtable: Self::VTABLE,
            object,
            string_data,
        }
    }

    pub fn new_from_value(cx: &mut Context, string_data: Gc<StringValue>) -> Gc<StringObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::StringPrototype);
        let object = ordinary_object_create(proto);

        cx.heap.alloc(StringObject::new(object, string_data))
    }

    pub fn string_data(&self) -> Gc<StringValue> {
        self.string_data
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
impl Object for StringObject {
    fn is_string_object(&self) -> bool {
        true
    }

    // TODO: Implement string exotic methods
}

pub struct StringConstructor;

impl StringConstructor {
    // 22.1.2 Properties of the String Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func =
            BuiltinFunction::create(cx, Self::construct, 1, "String", Some(realm), None, None);

        func.set_is_constructor();
        func.set_property(
            "prototype".to_owned(),
            Property::data(
                realm.get_intrinsic(Intrinsic::StringPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 22.1.1.1 String
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let string_value = if arguments.is_empty() {
            cx.heap.alloc_string(String::new()).into()
        } else {
            let value = get_argument(arguments, 0);
            if new_target.is_none() && value.is_symbol() {
                unimplemented!("Symbols")
            }

            maybe!(to_string(cx, value))
        };

        match new_target {
            None => string_value.into(),
            Some(new_target) => {
                let object = maybe!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::StringPrototype
                ));

                cx.heap
                    .alloc(StringObject::new(object, string_value))
                    .into()
            }
        }
    }
}
