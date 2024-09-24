use crate::maybe;

use super::{
    abstract_operations::{call_object, construct, length_of_array_like},
    array_object::{create_array_from_list, ArrayObject},
    builtin_function::BuiltinFunction,
    bytecode::function::Closure,
    completion::EvalResult,
    gc::HeapPtr,
    get,
    object_value::ObjectValue,
    property_key::PropertyKey,
    proxy_object::ProxyObject,
    type_utilities::{is_constructor_object_value, same_object_value_handles},
    value::Value,
    Context, Handle,
};

pub struct BoundFunctionObject;

impl BoundFunctionObject {
    pub fn new(
        cx: Context,
        target_function: Handle<ObjectValue>,
        bound_this: Handle<Value>,
        bound_arguments: Vec<Handle<Value>>,
    ) -> EvalResult<Handle<ObjectValue>> {
        let prototype = maybe!(target_function.get_prototype_of(cx));

        let is_constructor = if target_function.is_closure() {
            target_function
                .cast::<Closure>()
                .function_ptr()
                .is_constructor()
        } else if target_function.is_proxy() {
            target_function.cast::<ProxyObject>().is_constructor()
        } else if let Some(target_function) =
            BoundFunctionObject::get_target_if_bound_function(cx, target_function)
        {
            is_constructor_object_value(target_function)
        } else {
            false
        };

        let bound_func = BuiltinFunction::create_builtin_function_without_properties(
            cx,
            BoundFunctionObject::call,
            /* name */ None,
            // Use realm of calling function. GetFunctionRealm ignores this function and instead
            // uses realm of bound target function.
            cx.current_realm(),
            prototype,
            is_constructor,
        )
        .into();

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

    /// If this object is a bound function, return the target function. Otherwise, return None.
    pub fn get_target_if_bound_function(
        cx: Context,
        object: Handle<ObjectValue>,
    ) -> Option<Handle<ObjectValue>> {
        object
            .private_element_find(cx, cx.well_known_symbols.bound_target().cast())
            .map(|p| p.value().cast::<ObjectValue>())
    }

    pub fn is_bound_function(cx: Context, object: HeapPtr<ObjectValue>) -> bool {
        object.has_private_element(cx.well_known_symbols.bound_target().cast())
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
    /// Combination of [[Call]] (https://tc39.es/ecma262/#sec-bound-function-exotic-objects-call-thisargument-argumentslist)
    /// and [[Construct]] (https://tc39.es/ecma262/#sec-bound-function-exotic-objects-construct-argumentslist-newtarget)
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
