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
    Context, Handle, Realm,
};

// 10.4.1 Bound Function Exotic Objects
extend_object! {
    pub struct BoundFunctionObject {
        bound_target_function: HeapPtr<ObjectValue>,
        bound_this: Value,
        bound_arguments: Vec<Value>,
    }
}

impl BoundFunctionObject {
    // 10.4.1.3 BoundFunctionCreate
    pub fn new(
        cx: &mut Context,
        target_function: Handle<ObjectValue>,
        bound_this: HandleValue,
        bound_arguments: Vec<HandleValue>,
    ) -> EvalResult<Handle<BoundFunctionObject>> {
        // May allocate, so call before allocating bound function object
        let proto = maybe!(target_function.get_prototype_of(cx));

        let mut object = object_create_with_optional_proto::<BoundFunctionObject>(
            cx,
            ObjectKind::BoundFunctionObject,
            proto,
        );

        set_uninit!(object.bound_target_function, target_function.get_());
        set_uninit!(object.bound_this, bound_this.get());
        set_uninit!(object.bound_arguments, bound_arguments.iter().map(HandleValue::get).collect());

        Handle::from_heap(object).into()
    }

    #[inline]
    pub fn bound_target_function(&self) -> Handle<ObjectValue> {
        Handle::from_heap(self.bound_target_function)
    }

    #[inline]
    fn bound_this(&self, cx: &mut Context) -> HandleValue {
        HandleValue::from_value(cx, self.bound_this)
    }

    #[inline]
    fn bound_arguments(&self, cx: &mut Context) -> Vec<HandleValue> {
        self.bound_arguments
            .iter()
            .map(|arg| HandleValue::from_value(cx, *arg))
            .collect()
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<BoundFunctionObject> {
    // 10.4.1.1 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        _: HandleValue,
        arguments: &[HandleValue],
    ) -> EvalResult<HandleValue> {
        let bound_this = self.bound_this(cx);
        if self.bound_arguments.is_empty() {
            call_object(cx, self.bound_target_function(), bound_this, arguments)
        } else if arguments.is_empty() {
            let bound_arguments = self.bound_arguments(cx);
            call_object(cx, self.bound_target_function(), bound_this, &bound_arguments)
        } else {
            let mut all_arguments = self.bound_arguments(cx);
            all_arguments.extend(arguments.iter());
            call_object(cx, self.bound_target_function(), bound_this, &all_arguments)
        }
    }

    // 10.4.1.2 [[Construct]]
    fn construct(
        &self,
        cx: &mut Context,
        arguments: &[HandleValue],
        new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        let new_target = if same_object_value(self.object(), new_target) {
            self.bound_target_function()
        } else {
            new_target
        };

        if self.bound_arguments.is_empty() {
            construct(cx, self.bound_target_function(), arguments, Some(new_target))
        } else if arguments.is_empty() {
            let bound_arguments = self.bound_arguments(cx);
            construct(cx, self.bound_target_function(), &bound_arguments, Some(new_target))
        } else {
            let mut all_arguments = self.bound_arguments(cx);
            all_arguments.extend(arguments.iter());
            construct(cx, self.bound_target_function(), &all_arguments, Some(new_target))
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
