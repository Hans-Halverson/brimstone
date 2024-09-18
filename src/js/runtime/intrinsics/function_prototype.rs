use crate::{
    js::runtime::{
        abstract_operations::{
            call_object, create_list_from_array_like, has_own_property, ordinary_has_instance,
        },
        bound_function_object::BoundFunctionObject,
        builtin_function::BuiltinFunction,
        bytecode::function::{BytecodeFunction, Closure},
        completion::EvalResult,
        error::type_error,
        function::{get_argument, set_function_length_maybe_infinity, set_function_name},
        get,
        interned_strings::InternedStrings,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_create_with_optional_proto, object_ordinary_init},
        property_key::PropertyKey,
        realm::Realm,
        string_value::{FlatString, StringValue},
        type_utilities::{is_callable, is_callable_object, to_integer_or_infinity},
        Context, Handle, HeapPtr, Value,
    },
    maybe, must,
};

use super::{intrinsics::Intrinsic, rust_runtime::return_undefined};

pub struct FunctionPrototype {}

impl FunctionPrototype {
    /// Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: Context) -> Handle<ObjectValue> {
        // Initialized with correct values in initialize method, but set to default value
        // at first to be GC safe until initialize method is called.
        let mut object =
            object_create_with_optional_proto::<Closure>(cx, ObjectKind::Closure, None);
        object.init_extra_fields(HeapPtr::uninit(), HeapPtr::uninit());

        object.to_handle().into()
    }

    /// Properties of the Function Prototype Object, https://tc39.es/ecma262/#sec-properties-of-the-function-prototype-object
    pub fn initialize(cx: Context, function_prototype: Handle<ObjectValue>, realm: Handle<Realm>) {
        let object_proto_ptr = realm.get_intrinsic_ptr(Intrinsic::ObjectPrototype);

        let mut object = function_prototype.object();

        // Initialize all fields of the prototype objec
        let descriptor_ptr = cx.base_descriptors.get(ObjectKind::Closure);
        object_ordinary_init(cx, object.get_(), descriptor_ptr, Some(object_proto_ptr));

        // The prototype object is a function which accepts any arguments and returns undefined
        // when invoked.
        let function = BytecodeFunction::new_rust_runtime_function(
            cx,
            return_undefined,
            realm,
            /* is_constructor */ false,
            /* name */ None,
        );
        let scope = realm.default_global_scope();

        object
            .cast::<Closure>()
            .init_extra_fields(function.get_(), scope.get_());

        object.instrinsic_length_prop(cx, 0);
        object.intrinsic_name_prop(cx, "");

        object.intrinsic_func(cx, cx.names.apply(), Self::apply, 2, realm);
        object.intrinsic_func(cx, cx.names.bind(), Self::bind, 1, realm);
        object.intrinsic_func(cx, cx.names.call(), Self::call_intrinsic, 1, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);

        // [Function.hasInstance] property
        let has_instance_func = BuiltinFunction::create(
            cx,
            Self::has_instance,
            1,
            cx.well_known_symbols.has_instance(),
            realm,
            None,
            None,
        )
        .into();
        object.intrinsic_frozen_property(
            cx,
            cx.well_known_symbols.has_instance(),
            has_instance_func,
        );
    }

    /// Function.prototype.apply, https://tc39.es/ecma262/#sec-function.prototype.apply
    pub fn apply(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !is_callable(this_value) {
            return type_error(cx, "value is not a function");
        }

        let this_arg = get_argument(cx, arguments, 0);
        let arg_array = get_argument(cx, arguments, 1);

        if arg_array.is_nullish() {
            call_object(cx, this_value.as_object(), this_arg, &[])
        } else {
            let arg_list = maybe!(create_list_from_array_like(cx, arg_array));
            call_object(cx, this_value.as_object(), this_arg, &arg_list)
        }
    }

    /// Function.prototype.bind, https://tc39.es/ecma262/#sec-function.prototype.bind
    pub fn bind(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !is_callable(this_value) {
            return type_error(cx, "value is not a function");
        }

        let target = this_value.as_object();

        let this_arg = get_argument(cx, arguments, 0);
        let bound_args = if arguments.is_empty() {
            Vec::new()
        } else {
            arguments[1..].to_vec()
        };
        let num_bound_args = bound_args.len();

        let bound_func: Handle<ObjectValue> =
            maybe!(BoundFunctionObject::new(cx, target, this_arg, bound_args));

        let mut length = Some(0);

        // Set function length to an integer or infinity based on the inner function's length
        if maybe!(has_own_property(cx, target, cx.names.length())) {
            let target_length_value = maybe!(get(cx, target, cx.names.length()));
            if target_length_value.is_number() {
                let target_length = target_length_value.as_number();
                if target_length == f64::INFINITY {
                    length = None;
                } else if target_length == f64::NEG_INFINITY {
                    length = Some(0);
                } else {
                    let target_len_as_int =
                        must!(to_integer_or_infinity(cx, target_length_value)) as usize;
                    length = Some(target_len_as_int.saturating_sub(num_bound_args));
                }
            }
        }

        set_function_length_maybe_infinity(cx, bound_func, length);

        let target_name = maybe!(get(cx, target, cx.names.name()));
        let target_name = if target_name.is_string() {
            target_name.as_string()
        } else {
            cx.names.empty_string().as_string()
        };

        let name_key = PropertyKey::string(cx, target_name).to_handle(cx);
        set_function_name(cx, bound_func, name_key, Some("bound"));

        bound_func.into()
    }

    /// Function.prototype.call, https://tc39.es/ecma262/#sec-function.prototype.call
    pub fn call_intrinsic(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !is_callable(this_value) {
            return type_error(cx, "value is not a function");
        }

        if arguments.is_empty() {
            call_object(cx, this_value.as_object(), cx.undefined(), &[])
        } else {
            let argument = get_argument(cx, arguments, 0);
            call_object(cx, this_value.as_object(), argument, &arguments[1..])
        }
    }

    /// Function.prototype.toString, https://tc39.es/ecma262/#sec-function.prototype.tostring
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Function.prototype.toString expected a function");
        }

        let this_object = this_value.as_object();
        if this_object.is_closure() {
            let function = this_object.cast::<Closure>().function();

            // First check for if the closure is a bound function
            if BoundFunctionObject::is_bound_function(cx, this_object.get_()) {
                return cx.alloc_string("function () { [native code] }").into();
            }

            // Builtin functions have special formatting using the function name
            if function.rust_runtime_function_id().is_some() {
                let mut string_parts = vec![InternedStrings::get_str(cx, "function ")];
                if let Some(name) = function.name() {
                    string_parts.push(name);
                }
                string_parts.push(InternedStrings::get_str(cx, "() { [native code] }"));

                return StringValue::concat_all(cx, &string_parts).into();
            }

            // Non-builtin functions return their original slice of the source code
            let source_range = function.source_range();
            let source_file = function.source_file_ptr().unwrap();
            let source_contents = source_file.contents_as_slice();

            // Copy slice of source contents to string. Must first copy source contents out to
            // vec since source file may be moved when allocating result string.
            let source_slice = source_contents[source_range].to_vec();

            let func_string = FlatString::from_wtf8(cx, &source_slice)
                .as_string()
                .to_handle();

            return func_string.into();
        }

        if is_callable_object(this_object) {
            return cx.alloc_string("function () { [native code] }").into();
        }

        type_error(cx, "Function.prototype.toString expected a function")
    }

    /// Function.prototype [ @@hasInstance ], https://tc39.es/ecma262/#sec-function.prototype-%symbol.hasinstance%
    pub fn has_instance(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let has_instance = maybe!(ordinary_has_instance(cx, this_value, argument));
        cx.bool(has_instance).into()
    }
}
