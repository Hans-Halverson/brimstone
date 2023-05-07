use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object, js::runtime::ordinary_object::object_create_with_optional_proto, maybe,
    set_uninit,
};

use super::{
    abstract_operations::{call_object, construct},
    completion::EvalResult,
    gc::{Gc, HandleValue, HeapPtr},
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::same_object_value,
    value::Value,
    Context, Realm,
};

// 10.4.1 Bound Function Exotic Objects
extend_object! {
    pub struct BoundFunctionObject {
        bound_target_function: Gc<ObjectValue>,
        bound_this: Value,
        bound_arguments: Vec<Value>,
    }
}

impl BoundFunctionObject {
    // 10.4.1.3 BoundFunctionCreate
    pub fn new(
        cx: &mut Context,
        target_function: Gc<ObjectValue>,
        bound_this: Value,
        bound_arguments: Vec<Value>,
    ) -> EvalResult<Gc<BoundFunctionObject>> {
        // May allocate, so call before allocating bound function object
        let proto = maybe!(target_function.get_prototype_of(cx));

        let mut object = object_create_with_optional_proto::<BoundFunctionObject>(
            cx,
            ObjectKind::BoundFunctionObject,
            proto,
        );

        set_uninit!(object.bound_target_function, target_function);
        set_uninit!(object.bound_this, bound_this);
        set_uninit!(object.bound_arguments, bound_arguments);

        object.into()
    }

    #[inline]
    pub fn bound_target_function(&self) -> Gc<ObjectValue> {
        self.bound_target_function
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Gc<BoundFunctionObject> {
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
        let new_target = if same_object_value(self.object(), new_target) {
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

    fn get_realm(&self, cx: &mut Context) -> EvalResult<HeapPtr<Realm>> {
        self.bound_target_function.get_realm(cx)
    }
}
