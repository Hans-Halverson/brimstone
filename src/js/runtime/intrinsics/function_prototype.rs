use std::cmp::max;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::{
            call_object, create_list_from_array_like, has_own_property, ordinary_has_instance,
        },
        bound_function_object::BoundFunctionObject,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::{get_argument, set_function_length_maybe_infinity, set_function_name},
        gc::Gc,
        get,
        object_descriptor::ObjectKind,
        object_value::{ObjectValue, VirtualObject},
        ordinary_object::{object_ordinary_init, object_ordinary_init_optional_proto},
        property::Property,
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::{is_callable, to_integer_or_infinity},
        value::Value,
        Context,
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

extend_object! {
    pub struct FunctionPrototype {}
}

impl FunctionPrototype {
    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: &mut Context) -> Gc<FunctionPrototype> {
        let object = cx.heap.alloc_uninit::<FunctionPrototype>();

        // Initialized with correct values in initialize method, but set to default value
        // at first to be GC safe until initialize method is called.
        let descriptor = cx.base_descriptors.get(ObjectKind::FunctionPrototype);
        object_ordinary_init_optional_proto(cx, object.object(), descriptor, None);

        object
    }
}

impl Gc<FunctionPrototype> {
    // 20.2.3 Properties of the Function Prototype Object
    pub fn initialize(&mut self, cx: &mut Context, realm: Gc<Realm>) {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        object_ordinary_init(cx, self.object(), ObjectKind::FunctionPrototype, object_proto);

        self.object().intrinsic_name_prop(cx, "");
        self.object().instrinsic_length_prop(cx, 0);

        self.object()
            .intrinsic_func(cx, &cx.names.apply(), FunctionPrototype::apply, 2, realm);
        self.object()
            .intrinsic_func(cx, &cx.names.bind(), FunctionPrototype::bind, 1, realm);
        self.object().intrinsic_func(
            cx,
            &cx.names.call(),
            FunctionPrototype::call_intrinsic,
            1,
            realm,
        );

        // [Function.hasInstance] property
        let has_instance_key = PropertyKey::symbol(cx.well_known_symbols.has_instance);
        let has_instance_name = cx.alloc_string(String::from("[Function.hasInstance]"));
        let has_instance_func = BuiltinFunction::create(
            cx,
            FunctionPrototype::has_instance,
            1,
            &PropertyKey::string_not_number(has_instance_name),
            Some(realm),
            None,
            None,
        )
        .into();
        self.object().set_property(
            cx,
            &has_instance_key,
            Property::data(has_instance_func, false, false, false),
        );
    }
}

impl FunctionPrototype {
    // 20.2.3.1 Function.prototype.apply
    fn apply(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if !is_callable(this_value) {
            return type_error_(cx, "value is not a function");
        }

        let this_arg = get_argument(arguments, 0);
        let arg_array = get_argument(arguments, 1);

        if arg_array.is_nullish() {
            call_object(cx, this_value.as_object(), this_arg, &[])
        } else {
            let arg_list = maybe!(create_list_from_array_like(cx, arg_array));
            call_object(cx, this_value.as_object(), this_arg, &arg_list)
        }
    }

    // 20.2.3.2 Function.prototype.bind
    fn bind(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if !is_callable(this_value) {
            return type_error_(cx, "value is not a function");
        }

        let target = this_value.as_object();

        let this_arg = get_argument(arguments, 0);
        let bound_args = if arguments.is_empty() {
            Vec::new()
        } else {
            arguments[1..].to_vec()
        };
        let num_bound_args = bound_args.len();

        let bound_func: Gc<ObjectValue> =
            maybe!(BoundFunctionObject::new(cx, target, this_arg, bound_args)).into();

        let mut length = Some(0);

        // Set function length to an integer or infinity based on the inner function's length
        if maybe!(has_own_property(cx, bound_func, &cx.names.length())) {
            let target_length_value = maybe!(get(cx, target, &cx.names.length()));
            if target_length_value.is_number() {
                let target_length = target_length_value.as_number();
                if target_length == f64::INFINITY {
                    length = None;
                } else if target_length == f64::NEG_INFINITY {
                    length = Some(0);
                } else {
                    let target_len_as_int =
                        must!(to_integer_or_infinity(cx, target_length_value)) as usize;
                    length = Some(max(target_len_as_int - num_bound_args, 0) as i32);
                }
            }
        }

        set_function_length_maybe_infinity(cx, bound_func, length);

        let target_name = maybe!(get(cx, target, &cx.names.name()));
        let name_key = maybe!(PropertyKey::from_value(cx, target_name));
        set_function_name(cx, bound_func, &name_key, Some("bound"));

        bound_func.into()
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

        if arguments.is_empty() {
            call_object(cx, this_value.as_object(), Value::undefined(), &[])
        } else {
            call_object(cx, this_value.as_object(), get_argument(arguments, 0), &arguments[1..])
        }
    }

    // 20.2.3.6 Function.prototype [ @@hasInstance ]
    fn has_instance(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        maybe!(ordinary_has_instance(cx, this_value, get_argument(arguments, 0))).into()
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Gc<FunctionPrototype> {
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
