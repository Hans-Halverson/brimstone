use std::{mem::size_of, rc::Weak};

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::{
        common::wtf_8::Wtf8String,
        runtime::{
            abstract_operations::{
                call_object, create_list_from_array_like, has_own_property, ordinary_has_instance,
            },
            bound_function_object::{BoundFunctionObject, LegacyBoundFunctionObject},
            builtin_function::BuiltinFunction,
            completion::EvalResult,
            error::type_error_,
            function::{
                get_argument, set_function_length_maybe_infinity, set_function_name, Function,
            },
            gc::{HeapObject, HeapVisitor},
            get,
            object_descriptor::ObjectKind,
            object_value::{ObjectValue, VirtualObject},
            ordinary_object::object_ordinary_init,
            property_descriptor::PropertyDescriptor,
            property_key::PropertyKey,
            realm::Realm,
            string_value::FlatString,
            type_utilities::{is_callable, is_callable_object, to_integer_or_infinity},
            Context, Handle, HeapPtr, Value,
        },
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

extend_object! {
    pub struct FunctionPrototype {}
}

impl FunctionPrototype {
    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: Context) -> Handle<FunctionPrototype> {
        let object = cx.alloc_uninit::<FunctionPrototype>();

        // Initialized with correct values in initialize method, but set to default value
        // at first to be GC safe until initialize method is called.
        let descriptor = cx.base_descriptors.get(ObjectKind::FunctionPrototype);
        object_ordinary_init(cx, object.into(), descriptor, None);

        object.to_handle()
    }
}

impl Handle<FunctionPrototype> {
    // 20.2.3 Properties of the Function Prototype Object
    pub fn initialize(&mut self, cx: Context, realm: Handle<Realm>) {
        let object_proto_ptr = realm.get_intrinsic_ptr(Intrinsic::ObjectPrototype);
        let descriptor_ptr = cx.base_descriptors.get(ObjectKind::FunctionPrototype);
        object_ordinary_init(cx, self.object().get_(), descriptor_ptr, Some(object_proto_ptr));

        self.object().instrinsic_length_prop(cx, 0);
        self.object().intrinsic_name_prop(cx, "");

        self.object()
            .intrinsic_func(cx, cx.names.apply(), FunctionPrototype::apply, 2, realm);
        self.object()
            .intrinsic_func(cx, cx.names.bind(), FunctionPrototype::bind, 1, realm);
        self.object().intrinsic_func(
            cx,
            cx.names.call(),
            FunctionPrototype::call_intrinsic,
            1,
            realm,
        );
        self.object().intrinsic_func(
            cx,
            cx.names.to_string(),
            FunctionPrototype::to_string,
            0,
            realm,
        );

        // [Function.hasInstance] property
        let has_instance_func = BuiltinFunction::create(
            cx,
            FunctionPrototype::has_instance,
            1,
            cx.well_known_symbols.has_instance(),
            realm,
            None,
            None,
        )
        .into();
        self.object().intrinsic_frozen_property(
            cx,
            cx.well_known_symbols.has_instance(),
            has_instance_func,
        );
    }
}

impl FunctionPrototype {
    // 20.2.3.1 Function.prototype.apply
    pub fn apply(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !is_callable(this_value) {
            return type_error_(cx, "value is not a function");
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

    // 20.2.3.2 Function.prototype.bind
    pub fn bind(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !is_callable(this_value) {
            return type_error_(cx, "value is not a function");
        }

        let target = this_value.as_object();

        let this_arg = get_argument(cx, arguments, 0);
        let bound_args = if arguments.is_empty() {
            Vec::new()
        } else {
            arguments[1..].to_vec()
        };
        let num_bound_args = bound_args.len();

        let bound_func: Handle<ObjectValue> = if cx.options.bytecode {
            maybe!(BoundFunctionObject::new(cx, target, this_arg, bound_args))
        } else {
            maybe!(LegacyBoundFunctionObject::new(cx, target, this_arg, bound_args)).into()
        };

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
                    length = Some(target_len_as_int.saturating_sub(num_bound_args) as usize);
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

    // 20.2.3.3 Function.prototype.call
    pub fn call_intrinsic(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !is_callable(this_value) {
            return type_error_(cx, "value is not a function");
        }

        if arguments.is_empty() {
            call_object(cx, this_value.as_object(), cx.undefined(), &[])
        } else {
            let argument = get_argument(cx, arguments, 0);
            call_object(cx, this_value.as_object(), argument, &arguments[1..])
        }
    }

    // 20.2.3.5 Function.prototype.toString
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "Function.prototype.toString expected a function");
        }

        let this_object = this_value.as_object();
        if this_object.is_function_object() {
            // If this is a function, return the original source code if it is available
            let this_function = this_object.cast::<Function>();
            if let Some(source_loc) = this_function.source_loc() {
                if let Some(source) = this_function.source().as_ref().and_then(Weak::upgrade) {
                    let func_bytes = &source.contents.as_bytes()[source_loc.start..source_loc.end];
                    let func_string = FlatString::from_wtf8(cx, func_bytes)
                        .as_string()
                        .to_handle();

                    return func_string.into();
                }
            }
        } else if this_object.is_builtin_function_object() {
            // All builtin functions are formatted specially
            let this_func = this_object.cast::<BuiltinFunction>();
            let func_name = if let Some(initial_name) = this_func.initial_name() {
                initial_name.to_wtf8_string()
            } else {
                Wtf8String::new()
            };

            let mut func_string = Wtf8String::from_str("function ");
            func_string.push_wtf8_str(&func_name);
            func_string.push_str("() { [native code] }");

            return cx.alloc_wtf8_string(&func_string).into();
        } else if is_callable_object(this_object) {
            return cx.alloc_string("function () { [native code] }").into();
        }

        type_error_(cx, "Function.prototype.toString expected a function")
    }

    // 20.2.3.6 Function.prototype [ @@hasInstance ]
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

#[wrap_ordinary_object]
impl VirtualObject for Handle<FunctionPrototype> {
    fn call(
        &self,
        cx: Context,
        _this_argument: Handle<Value>,
        _arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // 20.2.3 Properties of the Function Prototype Object
        // Accepts any arguments and returns undefined when invoked
        cx.undefined().into()
    }

    fn is_callable(&self) -> bool {
        true
    }
}

impl HeapObject for HeapPtr<FunctionPrototype> {
    fn byte_size(&self) -> usize {
        size_of::<FunctionPrototype>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}
