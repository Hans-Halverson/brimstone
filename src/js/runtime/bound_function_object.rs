use wrap_ordinary_object::wrap_ordinary_object;

use crate::{extend_object, maybe};

use super::{
    abstract_operations::{call_object, construct},
    completion::EvalResult,
    environment::private_environment::PrivateNameId,
    gc::Gc,
    object_value::{extract_object_vtable, HasObject, Object, ObjectValue},
    ordinary_object::object_ordinary_init_optional_proto,
    property::{PrivateProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::same_object_value,
    value::Value,
    Context, Realm,
};

// 10.4.1 Bound Function Exotic Objects
extend_object! {
    pub struct BoundFunctionObject {
        pub bound_target_function: Gc<ObjectValue>,
        bound_this: Value,
        bound_arguments: Vec<Value>,
    }
}

impl BoundFunctionObject {
    const VTABLE: *const () = extract_object_vtable::<BoundFunctionObject>();

    // 10.4.1.3 BoundFunctionCreate
    pub fn new(
        cx: &mut Context,
        target_function: Gc<ObjectValue>,
        bound_this: Value,
        bound_arguments: Vec<Value>,
    ) -> EvalResult<Gc<BoundFunctionObject>> {
        let proto = maybe!(target_function.get_prototype_of(cx));

        let mut object = cx.heap.alloc_uninit::<BoundFunctionObject>();
        object._vtable = Self::VTABLE;

        object_ordinary_init_optional_proto(object.object_mut(), proto);

        object.bound_target_function = target_function;
        object.bound_this = bound_this;
        object.bound_arguments = bound_arguments;

        object.into()
    }
}

#[wrap_ordinary_object]
impl Object for BoundFunctionObject {
    // 10.4.1.1 [[Call]]
    fn call(&self, cx: &mut Context, _: Value, arguments: &[Value]) -> EvalResult<Value> {
        if self.bound_arguments.is_empty() {
            call_object(cx, self.bound_target_function, self.bound_this, arguments)
        } else if arguments.is_empty() {
            call_object(cx, self.bound_target_function, self.bound_this, &self.bound_arguments)
        } else {
            let all_arguments = [&self.bound_arguments, arguments].concat();
            call_object(cx, self.bound_target_function, self.bound_this, &all_arguments)
        }
    }

    // 10.4.1.2 [[Construct]]
    fn construct(
        &self,
        cx: &mut Context,
        arguments: &[Value],
        new_target: Gc<ObjectValue>,
    ) -> EvalResult<Gc<ObjectValue>> {
        let new_target = if same_object_value(self.into(), new_target) {
            self.bound_target_function
        } else {
            new_target
        };

        if self.bound_arguments.is_empty() {
            construct(cx, self.bound_target_function, arguments, Some(new_target))
        } else if arguments.is_empty() {
            construct(cx, self.bound_target_function, &self.bound_arguments, Some(new_target))
        } else {
            let all_arguments = [&self.bound_arguments, arguments].concat();
            construct(cx, self.bound_target_function, &all_arguments, Some(new_target))
        }
    }

    fn is_callable(&self) -> bool {
        true
    }

    fn is_constructor(&self) -> bool {
        self.bound_target_function.is_constructor()
    }

    fn is_bound_function(&self) -> bool {
        true
    }

    fn get_realm(&self, cx: &mut Context) -> EvalResult<Gc<Realm>> {
        self.bound_target_function.get_realm(cx)
    }
}
