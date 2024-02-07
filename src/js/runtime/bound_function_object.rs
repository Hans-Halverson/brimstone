use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object, field_offset, js::runtime::ordinary_object::object_ordinary_init, maybe,
    set_uninit,
};

use super::{
    abstract_operations::{call_object, construct},
    collections::InlineArray,
    completion::EvalResult,
    gc::{HeapObject, HeapPtr, HeapVisitor},
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::same_object_value_handles,
    value::Value,
    Context, Handle, Realm,
};

// 10.4.1 Bound Function Exotic Objects
extend_object! {
    pub struct BoundFunctionObject {
        bound_target_function: HeapPtr<ObjectValue>,
        bound_this: Value,
        // Bound arguments stored inline within object
        bound_arguments: InlineArray<Value>,
    }
}

const BOUND_ARGUMENTS_BYTE_OFFSET: usize = field_offset!(BoundFunctionObject, bound_arguments);

impl BoundFunctionObject {
    // 10.4.1.3 BoundFunctionCreate
    pub fn new(
        cx: Context,
        target_function: Handle<ObjectValue>,
        bound_this: Handle<Value>,
        bound_arguments: Vec<Handle<Value>>,
    ) -> EvalResult<Handle<BoundFunctionObject>> {
        // May allocate, so call before allocating bound function object
        let proto = maybe!(target_function.get_prototype_of(cx));

        // Create with variable size since bound arguments are stored inline
        let byte_size = Self::calculate_size_in_bytes(bound_arguments.len());
        let mut object = cx.alloc_uninit_with_size::<BoundFunctionObject>(byte_size);

        let descriptor = cx.base_descriptors.get(ObjectKind::BoundFunctionObject);
        object_ordinary_init(cx, object.into(), descriptor, proto.map(|p| p.get_()));

        set_uninit!(object.bound_target_function, target_function.get_());
        set_uninit!(object.bound_this, bound_this.get());

        // Copy bound arguments into inline bound arguments array
        object
            .bound_arguments
            .init_with_uninit(bound_arguments.len());
        for (i, bound_arg) in bound_arguments.iter().enumerate() {
            set_uninit!(object.bound_arguments.as_mut_slice()[i], bound_arg.get());
        }

        object.to_handle().into()
    }

    #[inline]
    pub fn calculate_size_in_bytes(num_arguments: usize) -> usize {
        BOUND_ARGUMENTS_BYTE_OFFSET + InlineArray::<Value>::calculate_size_in_bytes(num_arguments)
    }

    #[inline]
    pub fn bound_target_function(&self) -> Handle<ObjectValue> {
        self.bound_target_function.to_handle()
    }

    #[inline]
    fn bound_this(&self, cx: Context) -> Handle<Value> {
        self.bound_this.to_handle(cx)
    }

    #[inline]
    fn bound_arguments(&self, cx: Context) -> Vec<Handle<Value>> {
        self.bound_arguments
            .as_slice()
            .iter()
            .map(|arg| arg.to_handle(cx))
            .collect()
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<BoundFunctionObject> {
    // 10.4.1.1 [[Call]]
    fn call(
        &self,
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let bound_this = self.bound_this(cx);
        if self.bound_arguments.as_slice().is_empty() {
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
        cx: Context,
        arguments: &[Handle<Value>],
        new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        let new_target = if same_object_value_handles(self.object(), new_target) {
            self.bound_target_function()
        } else {
            new_target
        };

        if self.bound_arguments.as_slice().is_empty() {
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
        self.bound_target_function().is_constructor()
    }

    fn get_realm(&self, cx: Context) -> EvalResult<HeapPtr<Realm>> {
        self.bound_target_function().get_realm(cx)
    }
}

impl HeapObject for HeapPtr<BoundFunctionObject> {
    fn byte_size(&self) -> usize {
        BoundFunctionObject::calculate_size_in_bytes(self.bound_arguments.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.bound_target_function);
        visitor.visit_value(&mut self.bound_this);

        for argument in self.bound_arguments.as_mut_slice() {
            visitor.visit_value(argument);
        }
    }
}
