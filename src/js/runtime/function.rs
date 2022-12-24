use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::{
        parser::ast::{self, AstPtr},
        runtime::intrinsics::intrinsics::Intrinsic,
    },
    maybe_, must_,
};

use super::{
    abstract_operations::define_property_or_throw,
    completion::{AbstractResult, Completion},
    environment::{
        environment::{to_trait_object, Environment},
        function_environment::FunctionEnvironment,
        private_environment::PrivateEnvironment,
    },
    error::type_error_,
    eval::function::instantiate_ordinary_function_object,
    execution_context::{ExecutionContext, ScriptOrModule},
    gc::{Gc, GcDeref},
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    ordinary_object::{ordinary_create_from_constructor, ordinary_object_create, OrdinaryObject},
    property_descriptor::PropertyDescriptor,
    realm::Realm,
    type_utilities::to_object,
    value::Value,
    Context,
};

#[derive(PartialEq)]
pub enum ThisMode {
    Lexical,
    Strict,
    Global,
}

#[derive(PartialEq)]
pub enum ConstructorKind {
    Base,
    Derived,
}

// 10.2 ECMAScript Function Object
#[repr(C)]
pub struct Function {
    _vtable: ObjectValueVtable,
    is_strict: bool,
    is_class_constructor: bool,
    // Whether this function has a [[Construct]] internal slot
    has_construct: bool,
    constructor_kind: ConstructorKind,
    pub this_mode: ThisMode,
    // Object properties of this function
    object: OrdinaryObject,
    pub home_object: Option<Gc<ObjectValue>>,
    realm: Gc<Realm>,
    script_or_module: Option<ScriptOrModule>,
    func_node: AstPtr<ast::Function>,
    pub environment: Gc<dyn Environment>,
    pub private_environment: Option<Gc<PrivateEnvironment>>,
}

impl GcDeref for Function {}

impl_gc_into!(Function, ObjectValue);

const VTABLE: *const () = extract_object_vtable::<Function>();

impl Function {
    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }
}

#[wrap_ordinary_object]
impl Object for Function {
    // 10.2.1 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        this_argument: Value,
        arguments: Vec<Value>,
    ) -> AbstractResult<Value> {
        let callee_context = self.prepare_for_ordinary_call(cx, None);

        if self.is_class_constructor {
            // Ensure that error is created in callee's execution context
            let error = type_error_(cx, &format!("Cannot call class constructor"));
            cx.pop_execution_context();

            return error;
        }

        maybe_!(self.ordinary_call_bind_this(cx, callee_context, this_argument));
        let result = self.ordinary_call_evaluate_body(arguments);

        cx.pop_execution_context();

        match result {
            Completion::Return(value) => value.into(),
            Completion::Normal(_) => Value::undefined().into(),
            Completion::Throw(value) => AbstractResult::Throw(value),
            Completion::Break | Completion::Continue => {
                panic!("Call completion cannot be Break or Continue")
            }
        }
    }

    // 9.2.2 [[Construct]]
    fn construct(
        &self,
        cx: &mut Context,
        arguments: Vec<Value>,
        new_target: Gc<ObjectValue>,
    ) -> AbstractResult<Gc<ObjectValue>> {
        let this_argument: Option<Gc<ObjectValue>> =
            if self.constructor_kind == ConstructorKind::Base {
                Some(
                    maybe_!(ordinary_create_from_constructor(
                        cx,
                        new_target,
                        Intrinsic::ObjectPrototype
                    ))
                    .into(),
                )
            } else {
                None
            };

        let callee_context = self.prepare_for_ordinary_call(cx, Some(new_target));
        match this_argument {
            Some(this_argument) => {
                maybe_!(self.ordinary_call_bind_this(cx, callee_context, this_argument.into()))
            }
            None => {}
        }

        let constructor_env = callee_context.lexical_env;
        let result = self.ordinary_call_evaluate_body(arguments);

        cx.pop_execution_context();

        match result {
            Completion::Return(value) => {
                if value.is_object() {
                    return value.as_object().into();
                }

                match this_argument {
                    Some(this_argument) => return this_argument.into(),
                    None => {}
                }

                if !value.is_undefined() {
                    return type_error_(
                        cx,
                        &format!("Constructor must return object or undefined"),
                    );
                }
            }
            Completion::Normal(_) => {}
            Completion::Throw(value) => return AbstractResult::Throw(value),
            Completion::Break | Completion::Continue => {
                panic!("Construct completion cannot be Break or Continue")
            }
        }

        let this_binding = maybe_!(constructor_env.get_this_binding(cx));
        this_binding.as_object().into()
    }
}

impl Function {
    // 10.2.1.1 PrepareForOrdinaryCall
    fn prepare_for_ordinary_call(
        &self,
        cx: &mut Context,
        new_target: Option<Gc<ObjectValue>>,
    ) -> Gc<ExecutionContext> {
        let func_env = to_trait_object(FunctionEnvironment::new(cx, self.into(), new_target));
        let callee_context = cx.heap.alloc(ExecutionContext {
            function: Some(self.into()),
            realm: self.realm,
            script_or_module: self.script_or_module,
            lexical_env: func_env,
            variable_env: func_env,
            private_env: self.private_environment,
        });

        cx.push_execution_context(callee_context);

        callee_context
    }

    // 10.2.1.2 OrdinaryCallBindThis
    fn ordinary_call_bind_this(
        &self,
        cx: &mut Context,
        mut callee_context: Gc<ExecutionContext>,
        this_argument: Value,
    ) -> AbstractResult<()> {
        let this_value = match self.this_mode {
            ThisMode::Lexical => return ().into(),
            ThisMode::Strict => this_argument,
            ThisMode::Global => {
                let object_value = if this_argument.is_nullish() {
                    let global_env = self.realm.global_env;
                    global_env.global_this_value
                } else {
                    maybe_!(to_object(this_argument))
                };

                object_value.into()
            }
        };

        let local_func_env = callee_context
            .lexical_env
            .as_function_environment()
            .unwrap();
        maybe_!(local_func_env.bind_this_value(cx, this_value));

        ().into()
    }

    // 10.2.1.4 OrdinaryCallEvaluateBody
    fn ordinary_call_evaluate_body(&self, arguments: Vec<Value>) -> Completion {
        unimplemented!()
    }
}

// 10.2.3 OrdinaryFunctionCreate
pub fn ordinary_function_create(
    cx: &mut Context,
    function_prototype: Gc<ObjectValue>,
    func_node: &ast::Function,
    is_lexical_this: bool,
    environment: Gc<dyn Environment>,
    private_environment: Option<Gc<PrivateEnvironment>>,
) -> Gc<Function> {
    // TODO: Check if function is in strict mode
    let is_strict = false;
    let this_mode = if is_lexical_this {
        ThisMode::Lexical
    } else if is_strict {
        ThisMode::Strict
    } else {
        ThisMode::Global
    };
    let object = ordinary_object_create(function_prototype);
    let argument_count = expected_argument_count(func_node);

    let func = Function {
        _vtable: VTABLE,
        is_strict,
        is_class_constructor: false,
        has_construct: false,
        constructor_kind: ConstructorKind::Base,
        this_mode,
        object,
        home_object: None,
        realm: cx.current_realm(),
        script_or_module: cx.get_active_script_or_module(),
        environment,
        private_environment,
        func_node: AstPtr::from_ref(func_node),
    };

    let func = cx.heap.alloc(func);
    set_function_length(cx, func.into(), argument_count);

    func
}

// 10.2.5 MakeConstructor
pub fn make_constructor(
    cx: &mut Context,
    mut func: Gc<Function>,
    writable_prototype: Option<bool>,
    prototype: Option<Gc<ObjectValue>>,
) {
    // TODO: func may be a BuiltinFunction

    func.has_construct = true;
    func.constructor_kind = ConstructorKind::Base;

    let writable_prototype = writable_prototype.unwrap_or(true);
    let prototype = match prototype {
        Some(prototype) => prototype,
        None => {
            let object_prototype = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
            let ordinary_object = ordinary_object_create(object_prototype);
            let prototype = cx.heap.alloc(ordinary_object).into();

            let desc = PropertyDescriptor::data(func.into(), writable_prototype, false, true);
            must_!(define_property_or_throw(cx, prototype, "constructor", desc));

            prototype
        }
    };

    let desc = PropertyDescriptor::data(prototype.into(), writable_prototype, false, false);
    must_!(define_property_or_throw(cx, func.into(), "prototype", desc));
}

// 10.2.6 MakeClassConstructor
fn make_class_constructor(mut func: Gc<Function>) {
    func.is_class_constructor = true;
}

// 10.2.7 MakeMethod
fn make_method(mut func: Gc<Function>, home_object: Gc<ObjectValue>) {
    func.home_object = Some(home_object);
}

// 10.2.9 SetFunctionName
pub fn set_function_name(
    cx: &mut Context,
    func: Gc<ObjectValue>,
    name: &str,
    prefix: Option<&str>,
) {
    // TODO: Handle symbol and private names

    if let Some(mut builtin_func) = func.as_builtin_function_opt() {
        // Choose to not add prefix, as this is optional in spec
        builtin_func.initial_name = Some(name.to_string());
    }

    let name = if let Some(prefix) = prefix {
        cx.heap.alloc_string(format!("{} {}", prefix, name))
    } else {
        cx.heap.alloc_string(name.into())
    };

    let desc = PropertyDescriptor::data(name.into(), false, false, true);
    must_!(define_property_or_throw(cx, func, "name", desc))
}

// 10.2.10 SetFunctionLength
pub fn set_function_length(cx: &mut Context, func: Gc<ObjectValue>, length: u32) {
    let float_length: f64 = length.into();
    let desc = PropertyDescriptor::data(float_length.into(), false, false, true);
    must_!(define_property_or_throw(cx, func, "length", desc))
}

// 8.5.1 InstantiateFunctionObject
pub fn instantiate_function_object(
    cx: &mut Context,
    func_node: &ast::Function,
    env: Gc<dyn Environment>,
    private_env: Option<Gc<PrivateEnvironment>>,
) -> Gc<Function> {
    if func_node.is_async || func_node.is_generator {
        unimplemented!("async and generator functions not yet implemented")
    }

    instantiate_ordinary_function_object(cx, func_node, env, private_env)
}

// 15.1.5 ExpectedArgumentCount
// Count is the number of parameters to the left of the first initializer or rest parameter.
fn expected_argument_count(func_node: &ast::Function) -> u32 {
    let mut count = 0;
    for param in &func_node.params {
        match param {
            ast::Pattern::Assign(_) => return count,
            _ => count += 1,
        }
    }

    count
}

impl Into<Gc<Function>> for &Function {
    fn into(self) -> Gc<Function> {
        Gc::from_ptr(self as *const _ as *mut Function)
    }
}
