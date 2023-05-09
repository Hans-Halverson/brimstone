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
            gc::HandleValue,
            intrinsics::{
                array_buffer_constructor::ArrayBufferObject,
                global_object::set_default_global_bindings, intrinsics::Intrinsic,
            },
            object_value::ObjectValue,
            Context, EvalResult, Handle, PropertyDescriptor, PropertyKey, Realm,
        },
    },
    must,
};

pub struct Test262Object;

impl Test262Object {
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        let create_realm_string = cx.alloc_string(String::from("createRealm"));
        let create_realm_key = PropertyKey::string(cx, create_realm_string);
        object.intrinsic_func(cx, create_realm_key, Self::create_realm, 0, realm);

        let eval_script_string = cx.alloc_string(String::from("evalScript"));
        let eval_script_key = PropertyKey::string(cx, eval_script_string);
        object.intrinsic_func(cx, eval_script_key, Self::eval_script, 1, realm);

        let global_string = cx.alloc_string(String::from("global"));
        let global_key = PropertyKey::string(cx, global_string);
        object.intrinsic_data_prop(cx, global_key, realm.global_object().into());

        let detach_array_buffer_string = cx.alloc_string(String::from("detachArrayBuffer"));
        let detach_array_buffer_key = PropertyKey::string(cx, detach_array_buffer_string);
        object.intrinsic_func(cx, detach_array_buffer_key, Self::detach_array_buffer, 1, realm);

        object.to_handle()
    }

    pub fn install(cx: &mut Context, realm: Handle<Realm>, test_262_object: Handle<ObjectValue>) {
        let test_262_string = cx.alloc_string(String::from("$262"));
        let test_262_key = PropertyKey::string(cx, test_262_string);
        let desc = PropertyDescriptor::data(test_262_object.into(), true, false, true);
        must!(define_property_or_throw(cx, realm.global_object(), test_262_key, desc));
    }

    fn create_realm(
        cx: &mut Context,
        _: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
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
        _: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let script_text = get_argument(cx, arguments, 0);
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
        _: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return cx.undefined().into();
        }

        let object = value.as_object();
        if !object.is_array_buffer() {
            return cx.undefined().into();
        }

        let mut array_buffer = object.cast::<ArrayBufferObject>();
        array_buffer.detach();

        cx.undefined().into()
    }
}
