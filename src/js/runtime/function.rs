use std::collections::HashMap;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::parser::ast::{self, AstPtr},
    maybe, maybe_, maybe__, must,
};

use super::{
    abstract_operations::{construct, define_property_or_throw, initialize_instance_elements},
    completion::{Completion, CompletionKind, EvalResult},
    environment::{
        environment::{DynEnvironment, HeapDynEnvironment},
        function_environment::FunctionEnvironment,
        private_environment::{HeapPrivateName, PrivateEnvironment, PrivateName},
    },
    error::type_error_,
    eval::{
        class::{ClassFieldDefinition, HeapClassFieldDefinition},
        expression::eval_expression,
        function::{function_declaration_instantiation, instantiate_ordinary_function_object},
        statement::{eval_named_anonymous_function_or_expression, eval_statement_list},
    },
    execution_context::{ExecutionContext, HeapScriptOrModule, ScriptOrModule},
    gc::Gc,
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::{
        object_ordinary_init, ordinary_create_from_constructor, ordinary_object_create,
    },
    property::{HeapProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::{HeapPropertyKey, PropertyKey},
    realm::Realm,
    string_value::StringValue,
    type_utilities::to_object,
    value::Value,
    Context,
};

#[derive(PartialEq)]
enum ThisMode {
    Lexical,
    Strict,
    Global,
}

#[derive(Clone, Copy, PartialEq)]
pub enum ConstructorKind {
    Base,
    Derived,
}

// 10.2 ECMAScript Function Object
extend_object! {
    pub struct Function {
        is_strict: bool,
        is_class_constructor: bool,
        // Whether this function has a [[Construct]] internal slot
        has_construct: bool,
        constructor_kind: ConstructorKind,
        this_mode: ThisMode,
        // Object properties of this function
        home_object: Option<Gc<ObjectValue>>,
        realm: Gc<Realm>,
        script_or_module: Option<HeapScriptOrModule>,
        func_node: HeapFuncKind,
        environment: HeapDynEnvironment,
        private_environment: Option<Gc<PrivateEnvironment>>,
        fields: Vec<HeapClassFieldDefinition>,
        private_methods: Vec<(HeapPrivateName, HeapProperty)>,
    }
}

// Function objects may have special kinds, such as executing a class property node instead of a
// function node, or executing a builtin constructor. Stored on stack.
pub enum FuncKind {
    Function(AstPtr<ast::Function>),
    ClassProperty(AstPtr<ast::ClassProperty>, PropertyKey),
    DefaultConstructor,
}

/// A FuncKind that is stored on the heap.
enum HeapFuncKind {
    Function(AstPtr<ast::Function>),
    ClassProperty(AstPtr<ast::ClassProperty>, HeapPropertyKey),
    DefaultConstructor,
}

impl Function {
    fn new(
        cx: &mut Context,
        prototype: Gc<ObjectValue>,
        func_node: FuncKind,
        is_lexical_this: bool,
        is_strict: bool,
        environment: DynEnvironment,
        private_environment: Option<Gc<PrivateEnvironment>>,
    ) -> Gc<Function> {
        let this_mode = if is_lexical_this {
            ThisMode::Lexical
        } else if is_strict {
            ThisMode::Strict
        } else {
            ThisMode::Global
        };

        let mut object = cx.heap.alloc_uninit::<Function>();
        object_ordinary_init(cx, object.object(), ObjectKind::Function, prototype);

        object.is_strict = is_strict;
        object.is_class_constructor = false;
        object.has_construct = false;
        object.constructor_kind = ConstructorKind::Base;
        object.this_mode = this_mode;
        object.home_object = None;
        object.realm = cx.current_realm();
        object.script_or_module = cx
            .get_active_script_or_module()
            .as_ref()
            .map(ScriptOrModule::to_heap);
        object.environment = environment.to_heap();
        object.private_environment = private_environment;
        object.func_node = func_node.to_heap();
        object.fields = vec![];
        object.private_methods = vec![];

        object
    }

    #[inline]
    pub fn environment(&self) -> DynEnvironment {
        DynEnvironment::from_heap(&self.environment)
    }

    #[inline]
    pub fn home_object(&self) -> Option<Gc<ObjectValue>> {
        self.home_object
    }

    pub fn has_home_object(&self) -> bool {
        self.home_object.is_some()
    }

    pub fn is_class_property(&self) -> bool {
        match self.func_node {
            HeapFuncKind::ClassProperty(..) => true,
            _ => false,
        }
    }

    fn is_default_constructor(&self) -> bool {
        match self.func_node {
            HeapFuncKind::DefaultConstructor => true,
            _ => false,
        }
    }

    #[inline]
    pub fn func_ast_node(&self) -> Option<AstPtr<ast::Function>> {
        match &self.func_node {
            HeapFuncKind::Function(func_ast_node) => Some(func_ast_node.clone()),
            _ => None,
        }
    }

    pub fn is_lexical_this_mode(&self) -> bool {
        self.this_mode == ThisMode::Lexical
    }

    pub fn constructor_kind(&self) -> ConstructorKind {
        self.constructor_kind
    }

    pub fn set_constructor_kind(&mut self, kind: ConstructorKind) {
        self.constructor_kind = kind
    }

    pub fn add_fields(&mut self, fields: Vec<ClassFieldDefinition>) {
        self.fields.reserve_exact(fields.len());
        for field in fields {
            self.fields.push(field.to_heap());
        }
    }

    pub fn add_private_methods(&mut self, private_methods: HashMap<PrivateName, Property>) {
        self.private_methods.reserve_exact(private_methods.len());
        for (private_name, method_property) in private_methods {
            self.private_methods
                .push((private_name.to_heap(), method_property.to_heap()));
        }
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Gc<Function> {
    // 10.2.1 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        this_argument: Value,
        arguments: &[Value],
    ) -> EvalResult<Value> {
        let callee_context = self.prepare_for_ordinary_call(cx, None);

        if self.is_class_constructor {
            // Ensure that error is created in callee's execution context
            let error = type_error_(cx, &format!("Cannot call class constructor"));
            cx.pop_execution_context();

            return error;
        }

        self.ordinary_call_bind_this(cx, callee_context, this_argument);
        let result = self.ordinary_call_evaluate_body(cx, &arguments);

        cx.pop_execution_context();

        match result.kind() {
            CompletionKind::Return => result.value().into(),
            CompletionKind::Normal => Value::undefined().into(),
            CompletionKind::Throw => EvalResult::Throw(result.value().into()),
            CompletionKind::Break | CompletionKind::Continue => {
                panic!("Call completion cannot be Break or Continue")
            }
        }
    }

    // 10.2.2 [[Construct]]
    fn construct(
        &self,
        cx: &mut Context,
        arguments: &[Value],
        new_target: Gc<ObjectValue>,
    ) -> EvalResult<Gc<ObjectValue>> {
        // Default constructor is implemented as a special function. Steps follow the default
        // constructor abstract closure in 15.7.14 ClassDefinitionEvaluation.
        if self.is_default_constructor() {
            let new_object = if self.constructor_kind == ConstructorKind::Derived {
                let func = must!(self.object().get_prototype_of(cx));
                match func {
                    Some(func) if func.is_constructor() => {
                        maybe!(construct(cx, func, arguments, Some(new_target)))
                    }
                    _ => return type_error_(cx, "super class must be a constructor"),
                }
            } else {
                maybe!(ordinary_create_from_constructor(cx, new_target, Intrinsic::ObjectPrototype))
                    .into()
            };

            maybe!(initialize_instance_elements(cx, new_object, *self));

            return new_object.into();
        }

        let this_argument: Option<Gc<ObjectValue>> =
            if self.constructor_kind == ConstructorKind::Base {
                let object = maybe!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::ObjectPrototype
                ))
                .into();

                if self.is_default_constructor() {
                    maybe!(initialize_instance_elements(cx, object, *self));
                    None
                } else {
                    Some(object)
                }
            } else {
                if self.is_default_constructor() {
                    let func = must!(self.object().get_prototype_of(cx));
                    let object = match func {
                        Some(func) if func.is_constructor() => {
                            maybe!(construct(cx, func, arguments, Some(new_target)))
                        }
                        _ => return type_error_(cx, "super class must be a constructor"),
                    };

                    maybe!(initialize_instance_elements(cx, object, *self));
                }

                None
            };

        let callee_context = self.prepare_for_ordinary_call(cx, Some(new_target));
        match this_argument {
            Some(this_argument) => {
                self.ordinary_call_bind_this(cx, callee_context, this_argument.into());
                let initialize_result = initialize_instance_elements(cx, this_argument, *self);

                if let EvalResult::Throw(thrown_value) = initialize_result {
                    cx.pop_execution_context();
                    return EvalResult::Throw(thrown_value);
                }
            }
            None => {}
        }

        let constructor_env = callee_context.lexical_env();
        let result = self.ordinary_call_evaluate_body(cx, &arguments);

        cx.pop_execution_context();

        match result.kind() {
            CompletionKind::Return => {
                let value = result.value();
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
            CompletionKind::Normal => {}
            CompletionKind::Throw => return EvalResult::Throw(result.value()),
            CompletionKind::Break | CompletionKind::Continue => {
                panic!("Construct completion cannot be Break or Continue")
            }
        }

        let this_binding = maybe!(constructor_env.get_this_binding(cx));
        this_binding.as_object().into()
    }

    fn is_callable(&self) -> bool {
        true
    }

    fn is_constructor(&self) -> bool {
        self.has_construct
    }

    fn get_realm(&self, _: &mut Context) -> EvalResult<Gc<Realm>> {
        self.realm.into()
    }
}

impl Gc<Function> {
    // 10.2.1.1 PrepareForOrdinaryCall
    fn prepare_for_ordinary_call(
        &self,
        cx: &mut Context,
        new_target: Option<Gc<ObjectValue>>,
    ) -> Gc<ExecutionContext> {
        let func_env = FunctionEnvironment::new(cx, *self, new_target).into_dyn();
        let callee_context = ExecutionContext::new(
            cx,
            /* function */ Some(self.object()),
            self.realm,
            self.script_or_module
                .as_ref()
                .map(ScriptOrModule::from_heap),
            /* lexical_env */ func_env,
            /* variable_env */ func_env,
            self.private_environment,
            self.is_strict,
        );

        cx.push_execution_context(callee_context);

        callee_context
    }

    // 10.2.1.2 OrdinaryCallBindThis
    fn ordinary_call_bind_this(
        &self,
        cx: &mut Context,
        callee_context: Gc<ExecutionContext>,
        this_argument: Value,
    ) {
        let this_value = match self.this_mode {
            ThisMode::Lexical => return ().into(),
            ThisMode::Strict => this_argument,
            ThisMode::Global => {
                let object_value = if this_argument.is_nullish() {
                    self.realm.global_this_value()
                } else {
                    must!(to_object(cx, this_argument))
                };

                object_value.into()
            }
        };

        let mut local_func_env = callee_context
            .lexical_env()
            .as_function_environment()
            .unwrap();
        must!(local_func_env.bind_this_value(cx, this_value));
    }

    // 10.2.1.4 OrdinaryCallEvaluateBody
    // 10.2.1.3 EvaluateBody
    fn ordinary_call_evaluate_body(&self, cx: &mut Context, arguments: &[Value]) -> Completion {
        let other_self = *self;
        match &self.func_node {
            HeapFuncKind::Function(func_node) => {
                let func_node = func_node.as_ref();
                if func_node.is_async || func_node.is_generator {
                    unimplemented!("async and generator functions")
                }

                // 15.2.3 EvaluateFunctionBody
                // 15.3.3 EvaluateConciseBody
                maybe_!(function_declaration_instantiation(cx, other_self, arguments));
                match func_node.body.as_ref() {
                    ast::FunctionBody::Block(block) => eval_statement_list(cx, &block.body),
                    ast::FunctionBody::Expression(expr) => {
                        let value = maybe__!(eval_expression(cx, expr));
                        Completion::return_(value)
                    }
                }
            }
            // Initializer evaluation in EvaluateBody
            HeapFuncKind::ClassProperty(prop, name) => {
                let name = PropertyKey::from_heap(name);
                let expr = prop.as_ref().value.as_ref().unwrap();
                let value = maybe__!(eval_named_anonymous_function_or_expression(cx, expr, name));

                Completion::return_(value)
            }
            HeapFuncKind::DefaultConstructor => {
                unreachable!("default constructor body is never evaluated")
            }
        }
    }

    #[inline]
    pub fn iter_fields<F: FnMut(ClassFieldDefinition) -> EvalResult<()>>(
        &self,
        mut f: F,
    ) -> EvalResult<()> {
        // GC safe iteration over class fields
        for i in 0..self.fields.len() {
            let field = &self.fields[i];
            maybe!(f(ClassFieldDefinition::from_heap(field)));
        }

        ().into()
    }

    #[inline]
    pub fn iter_private_methods<F: FnMut(PrivateName, Property) -> EvalResult<()>>(
        &self,
        mut f: F,
    ) -> EvalResult<()> {
        // GC safe iteration over private methods
        for i in 0..self.private_methods.len() {
            let (heap_private_name, heap_private_method) = &self.private_methods[i];
            maybe!(f(
                PrivateName::from_heap(heap_private_name),
                Property::from_heap(heap_private_method)
            ));
        }

        ().into()
    }
}

impl FuncKind {
    fn to_heap(&self) -> HeapFuncKind {
        match self {
            FuncKind::Function(func_node) => HeapFuncKind::Function(func_node.clone()),
            FuncKind::ClassProperty(class_property_node, property_key) => {
                HeapFuncKind::ClassProperty(class_property_node.clone(), property_key.to_heap())
            }
            FuncKind::DefaultConstructor => HeapFuncKind::DefaultConstructor,
        }
    }
}

// 10.2.3 OrdinaryFunctionCreate
pub fn ordinary_function_create(
    cx: &mut Context,
    function_prototype: Gc<ObjectValue>,
    func_node: &ast::Function,
    is_lexical_this: bool,
    environment: DynEnvironment,
    private_environment: Option<Gc<PrivateEnvironment>>,
) -> Gc<Function> {
    let is_strict = func_node.is_strict_mode;
    let argument_count = expected_argument_count(func_node);
    let func_node = FuncKind::Function(AstPtr::from_ref(func_node));

    let func = Function::new(
        cx,
        function_prototype,
        func_node,
        is_lexical_this,
        is_strict,
        environment,
        private_environment,
    );

    set_function_length(cx, func.into(), argument_count);

    func
}

// A copy of OrdinaryObjectCreate, but for creating function objects with special non-function kinds
// such as class properties and static initializers.
pub fn ordinary_function_create_special_kind(
    cx: &mut Context,
    function_prototype: Gc<ObjectValue>,
    func_node: FuncKind,
    is_lexical_this: bool,
    is_strict: bool,
    argument_count: i32,
    environment: DynEnvironment,
    private_environment: Option<Gc<PrivateEnvironment>>,
) -> Gc<Function> {
    let func = Function::new(
        cx,
        function_prototype,
        func_node,
        is_lexical_this,
        is_strict,
        environment,
        private_environment,
    );

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
            let object_prototype = cx.get_intrinsic(Intrinsic::ObjectPrototype);
            let prototype = ordinary_object_create(cx, object_prototype).into();

            let desc = PropertyDescriptor::data(func.into(), writable_prototype, false, true);
            must!(define_property_or_throw(cx, prototype, cx.names.constructor(), desc));

            prototype
        }
    };

    let desc = PropertyDescriptor::data(prototype.into(), writable_prototype, false, false);
    must!(define_property_or_throw(cx, func.into(), cx.names.prototype(), desc));
}

// 10.2.6 MakeClassConstructor
pub fn make_class_constructor(mut func: Gc<Function>) {
    func.is_class_constructor = true;
}

// 10.2.7 MakeMethod
pub fn make_method(mut func: Gc<Function>, home_object: Gc<ObjectValue>) {
    func.home_object = Some(home_object);
}

// 10.2.8 DefineMethodProperty
pub fn define_method_property(
    cx: &mut Context,
    home_object: Gc<ObjectValue>,
    key: PropertyKey,
    closure: Gc<Function>,
    is_enumerable: bool,
) -> EvalResult<()> {
    let desc = PropertyDescriptor::data(closure.into(), true, is_enumerable, true);

    // Spec says this cannot throw, but this is not true since the home_object may already have a
    // non-configurable property that cannot be redefined (e.g. "prototype").
    maybe!(define_property_or_throw(cx, home_object, key, desc));

    ().into()
}

// 10.2.9 SetFunctionName
pub fn set_function_name(
    cx: &mut Context,
    func: Gc<ObjectValue>,
    name: PropertyKey,
    prefix: Option<&str>,
) {
    // Convert name to string value, property formatting symbol name
    let name_string = match name.as_symbol_opt() {
        Some(sym) => {
            if let Some(description) = sym.description() {
                let left_paren = cx.alloc_string(String::from("["));
                let right_paren = cx.alloc_string(String::from("]"));

                StringValue::concat_all(cx, &[left_paren, description, right_paren])
            } else {
                cx.names.empty_string().as_string()
            }
        }
        None => name.to_value(cx).as_string(),
    };

    // Add prefix to name
    let name_string = if let Some(prefix) = prefix {
        let prefix_string = cx.alloc_string(format!("{} ", prefix));
        StringValue::concat(cx, prefix_string, name_string)
    } else {
        name_string
    };

    if let Some(mut builtin_func) = func.as_builtin_function_opt() {
        // Choose to not add prefix, as this is optional in spec
        builtin_func.set_initial_name(Some(name_string));
    }

    let desc = PropertyDescriptor::data(name_string.into(), false, false, true);
    must!(define_property_or_throw(cx, func, cx.names.name(), desc))
}

// 10.2.10 SetFunctionLength
pub fn set_function_length(cx: &mut Context, func: Gc<ObjectValue>, length: i32) {
    let desc = PropertyDescriptor::data(Value::smi(length), false, false, true);
    must!(define_property_or_throw(cx, func, cx.names.length(), desc))
}

// Identical to SetFunctionLength, but a None value represents a length of positive infinity
pub fn set_function_length_maybe_infinity(
    cx: &mut Context,
    func: Gc<ObjectValue>,
    length: Option<i32>,
) {
    let length = if let Some(length) = length {
        Value::smi(length)
    } else {
        Value::number(f64::INFINITY)
    };

    let desc = PropertyDescriptor::data(length, false, false, true);
    must!(define_property_or_throw(cx, func, cx.names.length(), desc))
}

// 8.5.1 InstantiateFunctionObject
pub fn instantiate_function_object(
    cx: &mut Context,
    func_node: &ast::Function,
    env: DynEnvironment,
    private_env: Option<Gc<PrivateEnvironment>>,
) -> Gc<Function> {
    if func_node.is_async || func_node.is_generator {
        unimplemented!("async and generator functions")
    }

    instantiate_ordinary_function_object(cx, func_node, env, private_env)
}

// 15.1.5 ExpectedArgumentCount
// Count is the number of parameters to the left of the first initializer or rest parameter.
fn expected_argument_count(func_node: &ast::Function) -> i32 {
    let mut count = 0;
    for param in &func_node.params {
        match param {
            ast::FunctionParam::Pattern(ast::Pattern::Assign(_)) | ast::FunctionParam::Rest(_) => {
                return count
            }
            _ => count += 1,
        }
    }

    count
}

pub fn get_argument(arguments: &[Value], i: usize) -> Value {
    if i < arguments.len() {
        arguments[i]
    } else {
        Value::undefined()
    }
}
