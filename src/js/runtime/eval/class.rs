use crate::{
    js::{
        parser::ast::{self, ClassElement, ClassMethodKind},
        runtime::{
            abstract_operations::create_method_property,
            environment::{
                declarative_environment::DeclarativeEnvironment,
                environment::{to_trait_object, Environment},
                private_environment::PrivateEnvironment,
            },
            error::type_error_,
            eval::pattern::initialize_bound_name,
            function::{
                make_class_constructor, make_constructor, make_method, ordinary_function_create,
                set_function_name, ConstructorKind, Function,
            },
            get,
            intrinsics::intrinsics::Intrinsic,
            object_value::ObjectValue,
            ordinary_object::ordinary_object_create_optional_proto,
            value::{NULL_TAG, OBJECT_TAG},
            Completion, Context, EvalResult, Gc, Value,
        },
    },
    maybe, maybe__, must,
};

use super::{
    expression::{eval_expression, eval_property_name},
    function::{define_method, method_definition_evaluation},
};

// 15.7.10 ClassFieldDefinitionEvaluation
fn class_field_definition_evaluation(
    cx: &mut Context,
    prop: &ast::ClassProperty,
    home_object: Gc<ObjectValue>,
) -> EvalResult<Option<Gc<Function>>> {
    let current_execution_context = cx.current_execution_context();
    let env = current_execution_context.lexical_env;
    let private_env = current_execution_context.private_env;

    // let prototype = current_execution_context
    //     .realm
    //     .get_intrinsic(Intrinsic::FunctionPrototype);
    // let initializer = ordinary_function_create(cx, prototype, func_node, false, env, private_env);
    // make_method(initializer, home_object);

    unimplemented!("class fields")
}

// 15.7.14 ClassDefinitionEvaluation
pub fn class_definition_evaluation(
    cx: &mut Context,
    class: &ast::Class,
    class_binding: Option<&str>,
    class_name: &str,
) -> EvalResult<Gc<Function>> {
    let mut current_execution_context = cx.current_execution_context();
    let realm = current_execution_context.realm;
    let env = current_execution_context.lexical_env;
    let mut class_env = cx.heap.alloc(DeclarativeEnvironment::new(Some(env)));

    if let Some(class_binding) = class_binding {
        must!(class_env.create_immutable_binding(cx, String::from(class_binding), true));
    }

    let outer_private_env = current_execution_context.private_env;
    let class_private_env = PrivateEnvironment::new(cx, outer_private_env);

    // TODO: Implement private fields

    // Evaluate super class in the class environment
    let (proto_parent, constructor_parent) = if let Some(super_class) = class.super_class.as_deref()
    {
        current_execution_context.lexical_env = to_trait_object(class_env);

        let super_class_result = eval_expression(cx, super_class);

        current_execution_context.lexical_env = env;

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
            let proto_parent = maybe!(get(cx, super_class, "prototype"));

            match proto_parent.get_tag() {
                OBJECT_TAG => (Some(proto_parent.as_object()), super_class),
                NULL_TAG => (None, super_class),
                _ => return type_error_(cx, "super class prototype must be an object or null"),
            }
        }
    } else {
        let proto_parent = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let constructor_parent = realm.get_intrinsic(Intrinsic::FunctionPrototype);
        (Some(proto_parent), constructor_parent)
    };

    // Set up prototype and constructor
    let proto = cx
        .heap
        .alloc(ordinary_object_create_optional_proto(proto_parent));

    current_execution_context.lexical_env = to_trait_object(class_env);
    current_execution_context.private_env = Some(class_private_env);

    let mut func = if let Some(constructor) = class.constructor.as_ref() {
        let constructor = constructor.as_ref();
        let func = define_method(
            cx,
            proto.into(),
            &constructor.value,
            Some(constructor_parent),
        );

        make_class_constructor(func);
        set_function_name(cx, func.into(), class_name, None);

        func
    } else {
        unimplemented!("default constructor")
    };

    make_constructor(cx, func, Some(false), Some(proto.into()));

    if class.super_class.is_some() {
        func.constructor_kind = ConstructorKind::Derived;
    }

    create_method_property(cx, proto.into(), "constructor", func.into());

    // Evaluate class element definitions
    for element in &class.body {
        match element {
            ClassElement::Property(prop) => {
                let home_object: Gc<ObjectValue> = if prop.is_static {
                    proto.into()
                } else {
                    func.into()
                };
                let completion = class_field_definition_evaluation(cx, prop, home_object);
                unimplemented!("class fields")
            }
            ClassElement::Method(method) => {
                if method.kind == ClassMethodKind::Constructor {
                    continue;
                }

                if method.kind == ClassMethodKind::StaticInitializer {
                    unimplemented!("class static initializers")
                }

                let home_object: Gc<ObjectValue> = if method.is_static {
                    func.into()
                } else {
                    proto.into()
                };

                let result = (|| {
                    let property_key =
                        maybe!(eval_property_name(cx, &method.key, method.is_computed));
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
                        property_kind,
                        false,
                    )
                })();

                // TODO: Handle private fields

                match result {
                    EvalResult::Ok(_) => {}
                    EvalResult::Throw(thrown_value) => {
                        current_execution_context.lexical_env = env;
                        current_execution_context.private_env = outer_private_env;

                        return EvalResult::Throw(thrown_value);
                    }
                }
            }
        }
    }

    current_execution_context.lexical_env = env;

    if let Some(class_binding) = class_binding {
        must!(class_env.initialize_binding(cx, class_binding, func.into()));
    }

    // TODO: Handle private fields

    // TODO: Handle static elements

    current_execution_context.private_env = outer_private_env;

    func.into()
}

// 15.7.15 BindingClassDeclarationEvaluation
fn binding_class_declaration_evaluation(
    cx: &mut Context,
    class: &ast::Class,
) -> EvalResult<Gc<Function>> {
    if let Some(id) = class.id.as_deref() {
        let name = id.name.as_str();
        let value = maybe!(class_definition_evaluation(cx, class, Some(name), name));

        let lexical_env = cx.current_execution_context().lexical_env;
        maybe!(initialize_bound_name(
            cx,
            name,
            value.into(),
            Some(lexical_env)
        ));

        value.into()
    } else {
        class_definition_evaluation(cx, class, None, "default")
    }
}

// 15.7.16 Class Declaration and Expression Evaluation
pub fn eval_class_declaration(cx: &mut Context, class: &ast::Class) -> Completion {
    maybe__!(binding_class_declaration_evaluation(cx, class));
    Completion::empty()
}

pub fn eval_class_expression(cx: &mut Context, class: &ast::Class) -> EvalResult<Value> {
    if let Some(id) = class.id.as_deref() {
        let name = id.name.as_str();
        let value = maybe!(class_definition_evaluation(cx, class, Some(name), name));
        value.into()
    } else {
        let value = maybe!(class_definition_evaluation(cx, class, None, ""));
        value.into()
    }
}
