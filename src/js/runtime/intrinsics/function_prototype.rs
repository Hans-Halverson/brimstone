use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        abstract_operations::call_object,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        error::type_error_,
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::OrdinaryObject,
        property::PrivateProperty,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::is_callable,
        value::Value,
        Context,
    },
};

use super::intrinsics::Intrinsic;

#[repr(C)]
pub struct FunctionPrototype {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
}

impl GcDeref for FunctionPrototype {}

impl_gc_into!(FunctionPrototype, ObjectValue);

impl FunctionPrototype {
    const VTABLE: *const () = extract_object_vtable::<FunctionPrototype>();

    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: &mut Context) -> Gc<FunctionPrototype> {
        let func_prototype = FunctionPrototype {
            _vtable: Self::VTABLE,
            object: OrdinaryObject::new_uninit(),
        };

        cx.heap.alloc(func_prototype)
    }

    // 20.2.3 Properties of the Function Prototype Object
    pub fn initialize(&mut self, cx: &mut Context, realm: Gc<Realm>) {
        self.object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        self.object.intrinsic_name_prop(cx, "");
        self.object.instrinsic_length_prop(0.0);
        self.object
            .intrinsic_func(cx, "call", Self::call_intrinsic, 1, realm);
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }

    // 20.2.3.3 Function.prototype.call
    fn call_intrinsic(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if !is_callable(this_value) {
            return type_error_(cx, "value is not a function");
        }

        call_object(
            cx,
            this_value.as_object(),
            get_argument(arguments, 0),
            &arguments[1..],
        )
    }
}

#[wrap_ordinary_object]
impl Object for FunctionPrototype {
    fn call(
        &self,
        _: &mut Context,
        _this_argument: Value,
        _arguments: &[Value],
    ) -> EvalResult<Value> {
        // 20.2.3 Properties of the Function Prototype Object
        // Accepts any arguments and returns undefined when invoked
        Value::undefined().into()
    }

    fn is_callable(&self) -> bool {
        true
    }
}
