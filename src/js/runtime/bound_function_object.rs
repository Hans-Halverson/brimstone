use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object, field_offset, js::runtime::ordinary_object::object_ordinary_init, maybe,
    set_uninit,
};

use super::{
    abstract_operations::{call_object, construct, length_of_array_like},
    array_object::{create_array_from_list, ArrayObject},
    builtin_function::BuiltinFunction,
    bytecode::function::Closure,
    collections::InlineArray,
    completion::EvalResult,
    gc::{HeapObject, HeapPtr, HeapVisitor},
    get,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::same_object_value_handles,
    value::Value,
    Context, Handle, Realm,
};

pub struct BoundFunctionObject;

impl BoundFunctionObject {
    pub fn new(
        cx: Context,
        target_function: Handle<ObjectValue>,
        bound_this: Handle<Value>,
        bound_arguments: Vec<Handle<Value>>,
    ) -> EvalResult<Handle<ObjectValue>> {
        let is_constructor;
        let realm;
        if target_function.is_closure() {
            let function = target_function.cast::<Closure>().function_ptr();
            is_constructor = target_function
                .cast::<Closure>()
                .function_ptr()
                .is_constructor();
            realm = function.realm();
        } else {
            is_constructor = false;
            realm = cx.current_realm();
        }

        let prototype = maybe!(target_function.get_prototype_of(cx));
        let bound_func = BuiltinFunction::create_builtin_function_without_properties(
            cx,
            BoundFunctionObject::call,
            realm,
            prototype,
            is_constructor,
        );

        // Attach private fields, adding bound arguments to array object
        Self::set_target_function(cx, bound_func, target_function);
        Self::set_bound_this(cx, bound_func, bound_this);

        let bound_args_array = create_array_from_list(cx, &bound_arguments);
        Self::set_bound_arguments(cx, bound_func, bound_args_array);

        bound_func.into()
    }

    fn get_target_function(
        cx: Context,
        bound_function: Handle<ObjectValue>,
    ) -> Handle<ObjectValue> {
        bound_function
            .private_element_find(cx, cx.well_known_symbols.bound_target().cast())
            .unwrap()
            .value()
            .cast::<ObjectValue>()
    }

    fn set_target_function(
        cx: Context,
        mut bound_function: Handle<ObjectValue>,
        target: Handle<ObjectValue>,
    ) {
        bound_function.private_element_set(
            cx,
            cx.well_known_symbols.bound_target().cast(),
            target.into(),
        );
    }

    /// If this function is a bound function, return the target function. Otherwise, return None.
    pub fn get_target_if_bound_function(
        cx: Context,
        function: Handle<ObjectValue>,
    ) -> Option<Handle<ObjectValue>> {
        function
            .private_element_find(cx, cx.well_known_symbols.bound_target().cast())
            .map(|p| p.value().cast::<ObjectValue>())
    }

    fn get_bound_this(cx: Context, bound_function: Handle<ObjectValue>) -> Handle<Value> {
        bound_function
            .private_element_find(cx, cx.well_known_symbols.bound_this().cast())
            .unwrap()
            .value()
    }

    fn set_bound_this(
        cx: Context,
        mut bound_function: Handle<ObjectValue>,
        bound_this: Handle<Value>,
    ) {
        bound_function.private_element_set(
            cx,
            cx.well_known_symbols.bound_this().cast(),
            bound_this,
        );
    }

    fn get_bound_arguments(
        cx: Context,
        bound_function: Handle<ObjectValue>,
    ) -> Handle<ArrayObject> {
        bound_function
            .private_element_find(cx, cx.well_known_symbols.bound_arguments().cast())
            .unwrap()
            .value()
            .cast::<ArrayObject>()
    }

    fn set_bound_arguments(
        cx: Context,
        mut bound_function: Handle<ObjectValue>,
        arguments: Handle<ArrayObject>,
    ) {
        bound_function.private_element_set(
            cx,
            cx.well_known_symbols.bound_arguments().cast(),
            arguments.into(),
        );
    }

    /// Call the bound function from the rust runtime. This is called when the bound function has
    /// been called (either normally or as a constructor).
    ///
    /// Combination of 10.4.1.1 [[Call]] and 10.4.1.2 [[Construct]]
    pub fn call(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let bound_function = cx.current_function();

        let bound_target_function = Self::get_target_function(cx, bound_function);
        let bound_this = Self::get_bound_this(cx, bound_function);
        let bound_arguments = Self::get_bound_arguments(cx, bound_function);

        // Gather all arguments into a single array
        let mut all_arguments = vec![];

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let num_bound_arguments = maybe!(length_of_array_like(cx, bound_arguments.into()));

        // OPTIMIZATION: Much room for optimization of bound arguments instead of using array and
        // standard array accessors.
        for i in 0..num_bound_arguments {
            index_key.replace(PropertyKey::from_u64(cx, i));
            let arg = maybe!(get(cx, bound_arguments.into(), index_key));
            all_arguments.push(arg)
        }

        all_arguments.extend(arguments.iter());

        // If there is a new_target this was called as a constructor
        if let Some(new_target) = new_target {
            let new_target = if same_object_value_handles(bound_function, new_target) {
                bound_target_function
            } else {
                new_target
            };

            maybe!(construct(cx, bound_target_function, &all_arguments, Some(new_target))).into()
        } else {
            // Otherwise call bound target normally
            call_object(cx, bound_target_function, bound_this, &all_arguments)
        }
    }
}

// 10.4.1 Bound Function Exotic Objects
//
// These are legacy bound function object used in the old interpreter.
extend_object! {
    pub struct LegacyBoundFunctionObject {
        bound_target_function: HeapPtr<ObjectValue>,
        bound_this: Value,
        // Bound arguments stored inline within object
        bound_arguments: InlineArray<Value>,
    }
}

const BOUND_ARGUMENTS_BYTE_OFFSET: usize =
    field_offset!(LegacyBoundFunctionObject, bound_arguments);

impl LegacyBoundFunctionObject {
    // 10.4.1.3 BoundFunctionCreate
    pub fn new(
        cx: Context,
        target_function: Handle<ObjectValue>,
        bound_this: Handle<Value>,
        bound_arguments: Vec<Handle<Value>>,
    ) -> EvalResult<Handle<LegacyBoundFunctionObject>> {
        // May allocate, so call before allocating bound function object
        let proto = maybe!(target_function.get_prototype_of(cx));

        // Create with variable size since bound arguments are stored inline
        let byte_size = Self::calculate_size_in_bytes(bound_arguments.len());
        let mut object = cx.alloc_uninit_with_size::<LegacyBoundFunctionObject>(byte_size);

        let descriptor = cx
            .base_descriptors
            .get(ObjectKind::LegacyBoundFunctionObject);
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
impl VirtualObject for Handle<LegacyBoundFunctionObject> {
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

impl HeapObject for HeapPtr<LegacyBoundFunctionObject> {
    fn byte_size(&self) -> usize {
        LegacyBoundFunctionObject::calculate_size_in_bytes(self.bound_arguments.len())
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
