use std::rc::Rc;

use brimstone::{
    js::{
        parser::{analyze::analyze, parse_script, source::Source},
        runtime::{
            abstract_operations::define_property_or_throw,
            completion::CompletionKind,
            error::{syntax_error_, type_error_},
            eval::script::eval_script,
            function::get_argument,
            intrinsics::{
                array_buffer_constructor::ArrayBufferObject,
                global_object::set_default_global_bindings, intrinsics::Intrinsic,
            },
            object_value::ObjectValue,
            ordinary_object::OrdinaryObject,
            Context, EvalResult, Gc, PropertyDescriptor, PropertyKey, Realm, Value,
        },
    },
    must,
};

pub struct Test262Object;

impl Test262Object {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        let create_realm_key =
            PropertyKey::string(cx.heap.alloc_string(String::from("createRealm")));
        object.intrinsic_func(cx, &create_realm_key, Self::create_realm, 0, realm);

        let eval_script_key = PropertyKey::string(cx.heap.alloc_string(String::from("evalScript")));
        object.intrinsic_func(cx, &eval_script_key, Self::eval_script, 1, realm);

        let global_key = PropertyKey::string(cx.heap.alloc_string(String::from("global")));
        object.intrinsic_data_prop(&global_key, realm.global_object.into());

        let detach_array_buffer_key =
            PropertyKey::string(cx.heap.alloc_string(String::from("detachArrayBuffer")));
        object.intrinsic_func(cx, &detach_array_buffer_key, Self::detach_array_buffer, 1, realm);

        object.into()
    }

    pub fn install(cx: &mut Context, realm: Gc<Realm>, test_262_object: Gc<ObjectValue>) {
        let test_262_key = PropertyKey::string(cx.heap.alloc_string(String::from("$262")));
        let desc = PropertyDescriptor::data(test_262_object.into(), true, false, true);
        must!(define_property_or_throw(cx, realm.global_object, &test_262_key, desc));
    }

    fn create_realm(
        cx: &mut Context,
        _: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        // Create a new realm
        let mut realm = Realm::new(cx);
        realm.set_global_object(cx, None, None);
        set_default_global_bindings(cx, realm);

        // Add $262 object to new global object
        let test_262_object = Test262Object::new(cx, realm);
        Test262Object::install(cx, realm, test_262_object);

        test_262_object.into()
    }

    fn eval_script(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let script_text = get_argument(arguments, 0);
        if !script_text.is_string() {
            return type_error_(cx, "expected string");
        }

        let source = Rc::new(Source::new_from_string(
            "<eval>",
            String::from(script_text.as_string().to_string()),
        ));
        let parse_result = parse_script(&source);
        let mut ast = match parse_result {
            Ok(ast) => ast,
            Err(error) => return syntax_error_(cx, &error.to_string()),
        };

        let analyze_result = analyze(&mut ast, source);
        if let Err(errors) = analyze_result {
            // Choose an arbitrary syntax error to return
            let error = &errors.errors[0];
            return syntax_error_(cx, &error.to_string());
        }

        let realm = cx.current_realm();
        let completion = eval_script(cx, Rc::new(ast), realm);

        match completion.kind() {
            CompletionKind::Normal => EvalResult::Ok(completion.value()),
            CompletionKind::Throw => EvalResult::Throw(completion.value()),
            CompletionKind::Return | CompletionKind::Break | CompletionKind::Continue => {
                panic!("unexpected abnormal completion")
            }
        }
    }

    fn detach_array_buffer(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_object() {
            return Value::undefined().into();
        }

        let object = value.as_object();
        if !object.is_array_buffer() {
            return Value::undefined().into();
        }

        let mut array_buffer = object.cast::<ArrayBufferObject>();
        array_buffer.detach();

        Value::undefined().into()
    }
}
