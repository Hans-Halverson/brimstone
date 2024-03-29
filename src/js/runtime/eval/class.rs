use std::collections::HashMap;

use crate::{
    js::{
        parser::ast::{self, AstPtr, ClassElement, ClassMethodKind},
        runtime::{
            abstract_operations::{call_object, create_method_property, define_field},
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::Environment,
                private_environment::{HeapPrivateName, PrivateEnvironment, PrivateName},
            },
            error::type_error_,
            eval::pattern::initialize_bound_name,
            function::{
                make_class_constructor, make_constructor, make_method, ordinary_function_create,
                ordinary_function_create_special_kind, set_function_name, ConstructorKind,
                FuncKind, Function,
            },
            gc::HeapVisitor,
            get,
            intrinsics::intrinsics::Intrinsic,
            object_descriptor::ObjectKind,
            object_value::ObjectValue,
            ordinary_object::object_create_with_optional_proto,
            property_key::PropertyKey,
            string_value::StringValue,
            Completion, Context, EvalResult, Handle, HeapPtr, Value,
        },
    },
    maybe, maybe__, must,
};

use super::{
    expression::{eval_outer_expression, eval_property_name},
    function::{define_method, method_definition_evaluation, private_method_definition_evaluation},
    pattern::{id_string_value, private_id_property_key},
};

// 6.2.10 ClassFieldDefinition Record. Stored on the stack.
pub struct ClassFieldDefinition {
    pub name: ClassFieldDefinitionName,
    pub initializer: Option<Handle<Function>>,
}

// A ClassFieldDefinition that is stored on the heap.
#[derive(Clone)]
pub struct HeapClassFieldDefinition {
    name: HeapClassFieldDefinitionName,
    initializer: Option<HeapPtr<Function>>,
}

// Stored on the stack.
pub enum ClassFieldDefinitionName {
    Normal(Handle<PropertyKey>),
    Private(PrivateName),
}

// Stored on the heap.
#[derive(Clone)]
pub enum HeapClassFieldDefinitionName {
    Normal(PropertyKey),
    Private(HeapPrivateName),
}

impl ClassFieldDefinition {
    pub fn to_heap(&self) -> HeapClassFieldDefinition {
        HeapClassFieldDefinition {
            name: self.name.to_heap(),
            initializer: self.initializer.map(|i| i.get_()),
        }
    }

    pub fn from_heap(
        cx: Context,
        heap_field_def: &HeapClassFieldDefinition,
    ) -> ClassFieldDefinition {
        ClassFieldDefinition {
            name: ClassFieldDefinitionName::from_heap(cx, &heap_field_def.name),
            initializer: heap_field_def.initializer.map(|i| i.to_handle()),
        }
    }
}

impl ClassFieldDefinitionName {
    pub fn to_heap(&self) -> HeapClassFieldDefinitionName {
        match self {
            ClassFieldDefinitionName::Normal(property_key) => {
                HeapClassFieldDefinitionName::Normal(property_key.get())
            }
            ClassFieldDefinitionName::Private(private_name) => {
                HeapClassFieldDefinitionName::Private(private_name.get_())
            }
        }
    }

    pub fn from_heap(
        cx: Context,
        heap_name_def: &HeapClassFieldDefinitionName,
    ) -> ClassFieldDefinitionName {
        match heap_name_def {
            HeapClassFieldDefinitionName::Normal(property_key) => {
                ClassFieldDefinitionName::Normal(property_key.to_handle(cx))
            }
            HeapClassFieldDefinitionName::Private(private_name) => {
                ClassFieldDefinitionName::Private(private_name.to_handle())
            }
        }
    }
}

// 15.7.10 ClassFieldDefinitionEvaluation
fn class_field_definition_evaluation(
    cx: Context,
    prop: &ast::ClassProperty,
    property_key: Handle<PropertyKey>,
    home_object: Handle<ObjectValue>,
) -> EvalResult<Handle<Function>> {
    let current_execution_context_ptr = cx.current_execution_context_ptr();
    let env = current_execution_context_ptr.lexical_env();
    let private_env = current_execution_context_ptr.private_env();

    let prototype = current_execution_context_ptr.get_intrinsic(Intrinsic::FunctionPrototype);

    let func_node = FuncKind::ClassProperty(AstPtr::from_ref(prop), property_key);

    let initializer = ordinary_function_create_special_kind(
        cx,
        prototype,
        func_node,
        /* is_lexical_this */ false,
        /* is_strict */ true,
        /* argument_count */ 0,
        env,
        private_env,
    );
    make_method(initializer, home_object);

    initializer.into()
}

// 15.7.11 ClassStaticBlockDefinitionEvaluation
fn class_static_block_definition_evaluation(
    cx: Context,
    block: &ast::ClassMethod,
    home_object: Handle<ObjectValue>,
) -> Handle<Function> {
    let current_execution_context_ptr = cx.current_execution_context_ptr();
    let env = current_execution_context_ptr.lexical_env();
    let private_env = current_execution_context_ptr.private_env();

    let prototype = current_execution_context_ptr.get_intrinsic(Intrinsic::FunctionPrototype);
    let body_function =
        ordinary_function_create(cx, prototype, &block.value, false, env, private_env);
    make_method(body_function, home_object);

    body_function
}

enum StaticElement {
    Field(ClassFieldDefinition),
    Initializer(Handle<Function>),
}

// 15.7.14 ClassDefinitionEvaluation
pub fn class_definition_evaluation(
    cx: Context,
    class: &ast::Class,
    class_binding: Option<Handle<StringValue>>,
    class_name: Handle<PropertyKey>,
) -> EvalResult<Handle<Function>> {
    let mut current_execution_context = cx.current_execution_context();
    let realm = current_execution_context.realm();
    let env = current_execution_context.lexical_env();
    let mut class_env = DeclarativeEnvironment::new(cx, Some(env));

    if let Some(class_binding) = class_binding {
        must!(class_env.create_immutable_binding(cx, class_binding, true));
    }

    let outer_private_env = current_execution_context.private_env();
    let mut class_private_env = PrivateEnvironment::new(cx, outer_private_env);

    // Add private fields to class's private environment
    for element in &class.body {
        match element {
            ast::ClassElement::Method(ast::ClassMethod { key, is_private, .. })
            | ast::ClassElement::Property(ast::ClassProperty { key, is_private, .. })
                if *is_private =>
            {
                let key_string = id_string_value(cx, key.expr.to_id());
                if !class_private_env.has_private_name(key_string) {
                    class_private_env.add_private_name(cx, key_string)
                }
            }
            _ => {}
        }
    }

    // Evaluate super class in the class environment
    let (proto_parent, constructor_parent) = if let Some(super_class) = class.super_class.as_deref()
    {
        current_execution_context.set_lexical_env(class_env.into_dyn_env());

        let super_class_result = eval_outer_expression(cx, super_class);

        current_execution_context.set_lexical_env(env);

        let super_class = match super_class_result {
            EvalResult::Ok(super_class) => super_class,
            EvalResult::Throw(thrown_value) => return EvalResult::Throw(thrown_value),
        };

        if super_class.is_null() {
            let constructor_parent = realm.get_intrinsic(Intrinsic::FunctionPrototype);
            (None, constructor_parent)
        } else if !super_class.is_object() || !super_class.as_object().is_constructor() {
            return type_error_(cx, "super class must be a constructor");
        } else {
            let super_class = super_class.as_object();
            let proto_parent = maybe!(get(cx, super_class, cx.names.prototype()));

            if proto_parent.is_object() {
                (Some(proto_parent.as_object()), super_class)
            } else if proto_parent.is_null() {
                (None, super_class)
            } else {
                return type_error_(cx, "super class prototype must be an object or null");
            }
        }
    } else {
        let proto_parent = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let constructor_parent = realm.get_intrinsic(Intrinsic::FunctionPrototype);
        (Some(proto_parent), constructor_parent)
    };

    // Set up prototype and constructor
    let proto = object_create_with_optional_proto::<ObjectValue>(
        cx,
        ObjectKind::OrdinaryObject,
        proto_parent,
    )
    .to_handle();

    current_execution_context.set_lexical_env(class_env.into_dyn_env());
    current_execution_context.set_private_env(Some(class_private_env));

    let mut func = if let Some(constructor) = class.constructor.as_ref() {
        let constructor = constructor.as_ref();
        let mut func =
            define_method(cx, proto.into(), &constructor.value, Some(constructor_parent));

        make_class_constructor(func);
        set_function_name(cx, func.into(), class_name, None);

        // Despite using the constructor's AST node, we consider the source location to contain the
        // entire class.
        func.set_source_loc(class.loc);

        func
    } else {
        // Default constructor implemented as a special function object
        let func = ordinary_function_create_special_kind(
            cx,
            constructor_parent,
            FuncKind::DefaultConstructor(class.loc),
            /* is_lexical_this */ false,
            /* is_strict */ true,
            /* argument_count */ 0,
            current_execution_context.lexical_env(),
            current_execution_context.private_env(),
        );

        // Mark class constructor so that body is never evaluated
        make_class_constructor(func);
        set_function_name(cx, func.into(), class_name, None);

        func
    };

    make_constructor(cx, func, Some(false), Some(proto.into()));

    if class.super_class.is_some() {
        func.set_constructor_kind(ConstructorKind::Derived);
    }

    create_method_property(cx, proto.into(), cx.names.constructor(), func.into());

    let mut instance_fields = vec![];
    let mut static_elements = vec![];
    let mut instance_private_methods = HashMap::new();
    let mut static_private_methods = HashMap::new();

    // Evaluate class element definitions
    for element in &class.body {
        match element {
            ClassElement::Property(prop) => {
                let home_object: Handle<ObjectValue> = if prop.is_static {
                    proto.into()
                } else {
                    func.into()
                };

                let result = (|| {
                    let (property_key, field_def_name) = if prop.is_private {
                        let id = prop.key.expr.to_id();
                        let property_name_string = id_string_value(cx, id);
                        let private_name = class_private_env.get_private_name(property_name_string);
                        let property_key = private_id_property_key(cx, id);
                        let field_def_name = ClassFieldDefinitionName::Private(private_name);
                        (property_key, field_def_name)
                    } else {
                        let property_key =
                            maybe!(eval_property_name(cx, &prop.key.expr, prop.is_computed));
                        let field_def_name = ClassFieldDefinitionName::Normal(property_key.clone());
                        (property_key, field_def_name)
                    };

                    let initializer = if prop.value.is_none() {
                        None
                    } else {
                        Some(maybe!(class_field_definition_evaluation(
                            cx,
                            prop,
                            property_key,
                            home_object
                        )))
                    };

                    EvalResult::Ok(ClassFieldDefinition { name: field_def_name, initializer })
                })();

                let field_def = match result {
                    EvalResult::Ok(field_def) => field_def,
                    EvalResult::Throw(thrown_value) => {
                        current_execution_context.set_lexical_env(env);
                        current_execution_context.set_private_env(outer_private_env);

                        return EvalResult::Throw(thrown_value);
                    }
                };

                if prop.is_static {
                    static_elements.push(StaticElement::Field(field_def));
                } else {
                    instance_fields.push(field_def);
                }
            }
            ClassElement::Method(method) => {
                if method.kind == ClassMethodKind::Constructor {
                    continue;
                }

                let home_object: Handle<ObjectValue> = if method.is_static {
                    func.into()
                } else {
                    proto.into()
                };

                if method.kind == ClassMethodKind::StaticInitializer {
                    let body_function =
                        class_static_block_definition_evaluation(cx, method, func.into());

                    static_elements.push(StaticElement::Initializer(body_function));

                    continue;
                }

                if method.is_private {
                    // Resolve private name to id
                    let id = method.key.expr.to_id();
                    let property_name_string = id_string_value(cx, id);
                    let private_name = class_private_env.get_private_name(property_name_string);
                    let property_key = private_id_property_key(cx, id);

                    let private_property = private_method_definition_evaluation(
                        cx,
                        home_object,
                        &method.value,
                        property_key,
                        method.kind,
                    );

                    let container = if method.is_static {
                        &mut static_private_methods
                    } else {
                        &mut instance_private_methods
                    };

                    // Add property to appropriate container, combining accessors if necessary
                    if private_property.is_private_method() {
                        container.insert(private_name, private_property);
                    } else {
                        match container.get_mut(&private_name) {
                            None => {
                                container.insert(private_name, private_property);
                            }
                            Some(existing_property) => {
                                existing_property.add_accessor(private_property);
                            }
                        }
                    }

                    continue;
                }

                let result = (|| {
                    let property_key =
                        maybe!(eval_property_name(cx, &method.key.expr, method.is_computed));
                    let property_kind = match method.kind {
                        ClassMethodKind::Get => ast::PropertyKind::Get,
                        ClassMethodKind::Set => ast::PropertyKind::Set,
                        ClassMethodKind::Method => ast::PropertyKind::Init,
                        _ => unreachable!(),
                    };

                    method_definition_evaluation(
                        cx,
                        home_object,
                        &method.value,
                        property_key,
                        &property_kind,
                        false,
                    )
                })();

                match result {
                    EvalResult::Ok(_) => {}
                    EvalResult::Throw(thrown_value) => {
                        current_execution_context.set_lexical_env(env);
                        current_execution_context.set_private_env(outer_private_env);

                        return EvalResult::Throw(thrown_value);
                    }
                }
            }
        }
    }

    current_execution_context.set_lexical_env(env);

    if let Some(class_binding) = class_binding {
        must!(class_env.initialize_binding(cx, class_binding, func.into()));
    }

    func.add_fields(cx, instance_fields);

    // Store templates for private methods on constructor function
    func.add_private_methods(cx, instance_private_methods);

    // Define static private methods as private properties of the constructor function
    for (private_name, static_private_method) in static_private_methods {
        let mut func_object: Handle<ObjectValue> = func.into();
        must!(func_object.private_method_or_accessor_add(cx, private_name, static_private_method));
    }

    // Initialize static fields
    for static_element in static_elements {
        match static_element {
            StaticElement::Field(field_def) => {
                let result = define_field(cx, func.into(), field_def);

                if let EvalResult::Throw(thrown_value) = result {
                    current_execution_context.set_private_env(outer_private_env);
                    return EvalResult::Throw(thrown_value);
                }
            }
            StaticElement::Initializer(body_function) => {
                let result = call_object(cx, body_function.into(), func.into(), &[]);

                if let EvalResult::Throw(thrown_value) = result {
                    current_execution_context.set_private_env(outer_private_env);
                    return EvalResult::Throw(thrown_value);
                }
            }
        }
    }

    current_execution_context.set_private_env(outer_private_env);

    func.into()
}

// 15.7.15 BindingClassDeclarationEvaluation
fn binding_class_declaration_evaluation(
    cx: Context,
    class: &ast::Class,
) -> EvalResult<Handle<Function>> {
    if let Some(id) = class.id.as_deref() {
        let name_value = id_string_value(cx, id);
        let name_key = PropertyKey::string(cx, name_value).to_handle(cx);
        let value = maybe!(class_definition_evaluation(cx, class, Some(name_value), name_key));

        let lexical_env = cx.current_execution_context_ptr().lexical_env();
        maybe!(initialize_bound_name(cx, name_value, value.into(), Some(lexical_env)));

        value.into()
    } else {
        class_definition_evaluation(cx, class, None, cx.names.default())
    }
}

// 15.7.16 Class Declaration and Expression Evaluation
pub fn eval_class_declaration(cx: Context, class: &ast::Class) -> Completion {
    maybe__!(binding_class_declaration_evaluation(cx, class));
    Completion::empty(cx)
}

pub fn eval_class_expression(cx: Context, class: &ast::Class) -> EvalResult<Handle<Value>> {
    if let Some(id) = class.id.as_deref() {
        let name_value = id_string_value(cx, id);
        let name_key = PropertyKey::string(cx, name_value).to_handle(cx);
        let value = maybe!(class_definition_evaluation(cx, class, Some(name_value), name_key));
        value.into()
    } else {
        let value = maybe!(class_definition_evaluation(cx, class, None, cx.names.empty_string()));
        value.into()
    }
}

impl HeapClassFieldDefinition {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.name.visit_pointers(visitor);
        visitor.visit_pointer_opt(&mut self.initializer);
    }
}

impl HeapClassFieldDefinitionName {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self {
            HeapClassFieldDefinitionName::Normal(property_key) => {
                visitor.visit_property_key(property_key)
            }
            HeapClassFieldDefinitionName::Private(private_name) => {
                visitor.visit_pointer(private_name)
            }
        }
    }
}
