use std::{cell::RefCell, rc::Rc};

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{js::parser::ast, maybe_, must_};

use super::{
    abstract_operations::define_property_or_throw,
    completion::{AbstractResult, Completion},
    environment::{
        environment::{to_trait_object, Environment},
        function_environment::FunctionEnvironment,
    },
    error::type_error_,
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

// 9.2 ECMAScript Function Object
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
    realm: Rc<RefCell<Realm>>,
    script_or_module: Option<ScriptOrModule>,
    func_node: ast::Function,
    pub environment: Gc<dyn Environment>,
}

impl GcDeref for Function {}

const FUNCTION_VTABLE: *const () = extract_object_vtable::<Function>();

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
    // 9.2.1 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        this_argument: Value,
        arguments: Vec<Value>,
    ) -> AbstractResult<Value> {
        if self.is_class_constructor {
            return type_error_(cx, &format!("Cannot call class constructor"));
        }

        let callee_context = self.prepare_for_ordinary_call(cx, None);
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
                Some(ordinary_create_from_constructor(new_target, "%Object.prototype%").into())
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
    // 9.2.1.1 PrepareForOrdinaryCall
    fn prepare_for_ordinary_call(
        &self,
        cx: &mut Context,
        new_target: Option<Gc<ObjectValue>>,
    ) -> Gc<ExecutionContext> {
        let func_env = to_trait_object(FunctionEnvironment::new(cx, self.into(), new_target));
        let exec_ctx = cx.heap.alloc(ExecutionContext {
            function: Some(self.into()),
            realm: self.realm.clone(),
            script_or_module: self.script_or_module,
            lexical_env: func_env,
            variable_env: func_env,
        });

        cx.push_execution_context(exec_ctx);

        exec_ctx
    }

    // 9.2.1.2 OrdinaryCallBindThis
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
                    let global_env = self.realm.borrow().global_env;
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

    // 9.2.1.3 OrdinaryCallEvaluateBody
    fn ordinary_call_evaluate_body(&self, arguments: Vec<Value>) -> Completion {
        unimplemented!()
    }
}

// 9.2.3 OrdinaryFunctionCreate
pub fn ordinary_function_create(
    cx: &mut Context,
    function_prototype: Gc<ObjectValue>,
    func_node: ast::Function,
    scope: Gc<dyn Environment>,
) -> Gc<Function> {
    // TODO: Check if function is in strict mode
    let is_strict = false;
    let this_mode = if is_strict {
        ThisMode::Strict
    } else {
        ThisMode::Global
    };
    let object = ordinary_object_create(cx, function_prototype);
    let argument_count = expected_argument_count(&func_node);

    let func = Function {
        _vtable: FUNCTION_VTABLE,
        is_strict,
        is_class_constructor: false,
        has_construct: false,
        constructor_kind: ConstructorKind::Base,
        this_mode,
        object,
        home_object: None,
        realm: cx.current_realm(),
        script_or_module: cx.get_active_script_or_module(),
        environment: scope,
        func_node,
    };

    let func = cx.heap.alloc(func);
    set_function_length(cx, func, argument_count);

    func
}

// 9.2.5 MakeConstructor
fn make_constructor(
    cx: &mut Context,
    mut func: Gc<Function>,
    writable_prototype: Option<bool>,
    prototype: Option<Gc<ObjectValue>>,
) {
    func.has_construct = true;
    func.constructor_kind = ConstructorKind::Base;

    let writable_prototype = writable_prototype.unwrap_or(true);
    let prototype = match prototype {
        Some(prototype) => prototype,
        None => {
            let object_prototype = cx
                .current_realm()
                .borrow()
                .get_instrinsic("%Object.prototype%");
            let ordinary_object = ordinary_object_create(cx, object_prototype);
            let prototype = cx.heap.alloc(ordinary_object).into();

            let desc = PropertyDescriptor::data(func.into(), writable_prototype, false, true);
            must_!(define_property_or_throw(cx, prototype, "constructor", desc));

            prototype
        }
    };

    let desc = PropertyDescriptor::data(prototype.into(), writable_prototype, false, false);
    must_!(define_property_or_throw(cx, func.into(), "prototype", desc));
}

// 9.2.6 MakeClassConstructor
fn make_class_constructor(mut func: Gc<Function>) {
    func.is_class_constructor = true;
}

// 9.2.7 MakeMethod
fn make_method(mut func: Gc<Function>, home_object: Gc<ObjectValue>) {
    func.home_object = Some(home_object);
}

// 9.2.8 SetFunctionName
fn set_function_name(cx: &mut Context, func: Gc<Function>, name: &str, prefix: Option<&str>) {
    // TODO: Handle symbol names
    let name = if let Some(prefix) = prefix {
        cx.heap.alloc_string(format!("{} {}", prefix, name))
    } else {
        cx.heap.alloc_string(name.into())
    };

    let desc = PropertyDescriptor::data(name.into(), false, false, true);
    must_!(define_property_or_throw(cx, func.into(), "name", desc))
}

// 9.2.9 SetFunctionLength
fn set_function_length(cx: &mut Context, func: Gc<Function>, length: u32) {
    let float_length: f64 = length.into();
    let desc = PropertyDescriptor::data(float_length.into(), false, false, true);
    must_!(define_property_or_throw(cx, func.into(), "length", desc))
}

fn expected_argument_count(func_node: &ast::Function) -> u32 {
    unimplemented!()
}

impl<'a> Into<&'a ObjectValue> for &'a Function {
    fn into(self) -> &'a ObjectValue {
        unsafe { &*((self as *const _) as *const ObjectValue) }
    }
}

impl Into<Gc<ObjectValue>> for Gc<Function> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self.as_ref() as *const _ as *mut ObjectValue)
    }
}

impl Into<Gc<Function>> for &Function {
    fn into(self) -> Gc<Function> {
        Gc::from_ptr(self as *const _ as *mut Function)
    }
}

impl Into<Gc<ObjectValue>> for &Function {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self as *const _ as *mut ObjectValue)
    }
}
