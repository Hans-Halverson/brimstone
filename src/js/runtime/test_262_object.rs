use std::rc::Rc;

use crate::{
    js::{
        parser::{analyze::analyze, parse_script, source::Source},
        runtime::bytecode::generator::BytecodeProgramGenerator,
    },
    maybe,
};

use super::{
    error::{syntax_error, type_error},
    function::get_argument,
    intrinsics::{
        array_buffer_constructor::ArrayBufferObject, global_object::set_default_global_bindings,
        intrinsics::Intrinsic,
    },
    object_value::ObjectValue,
    to_console_string, Context, EvalResult, Handle, PropertyKey, Realm, Value,
};

/// Utility functions used in test262 tests. Must be included in the main library for now so that
/// builtin functions can be registered.
pub struct Test262Object;

impl Test262Object {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        let create_realm_string = cx.alloc_string("createRealm");
        let create_realm_key = PropertyKey::string(cx, create_realm_string).to_handle(cx);
        object.intrinsic_func(cx, create_realm_key, Self::create_realm, 0, realm);

        let eval_script_string = cx.alloc_string("evalScript");
        let eval_script_key = PropertyKey::string(cx, eval_script_string).to_handle(cx);
        object.intrinsic_func(cx, eval_script_key, Self::eval_script, 1, realm);

        let global_string = cx.alloc_string("global");
        let global_key = PropertyKey::string(cx, global_string).to_handle(cx);
        object.intrinsic_data_prop(cx, global_key, realm.global_object().into());

        let detach_array_buffer_string = cx.alloc_string("detachArrayBuffer");
        let detach_array_buffer_key =
            PropertyKey::string(cx, detach_array_buffer_string).to_handle(cx);
        object.intrinsic_func(cx, detach_array_buffer_key, Self::detach_array_buffer, 1, realm);

        object.to_handle()
    }

    pub fn install(mut cx: Context, realm: Handle<Realm>, test_262_object: Handle<ObjectValue>) {
        // Install the the "$262" property on the global object
        let test_262_string = cx.alloc_string("$262");
        let test_262_key = PropertyKey::string(cx, test_262_string).to_handle(cx);
        realm
            .global_object()
            .intrinsic_data_prop(cx, test_262_key, test_262_object.into());

        // Also install a global print function needed in tests
        let print_string = cx.alloc_string("print");
        let print_key = PropertyKey::string(cx, print_string).to_handle(cx);
        realm
            .global_object()
            .intrinsic_func(cx, print_key, Self::print, 1, realm);
    }

    pub fn print(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        println!("{}", to_console_string(cx, value));

        cx.undefined().into()
    }

    pub fn create_realm(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        // Create a new realm
        let realm = Realm::new_uninit(cx);
        maybe!(set_default_global_bindings(
            cx, realm, /* expose_gc */ false, /* expose_test262 */ false
        ));

        // Add $262 object to new global object
        let test_262_object = Test262Object::new(cx, realm);
        Test262Object::install(cx, realm, test_262_object);

        test_262_object.into()
    }

    pub fn eval_script(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let script_text = get_argument(cx, arguments, 0);
        if !script_text.is_string() {
            return type_error(cx, "expected string");
        }

        let source = Rc::new(Source::new_from_wtf8_string(
            "<eval>",
            script_text.as_string().to_wtf8_string(),
        ));
        let parse_result = parse_script(&source);
        let mut parse_result = match parse_result {
            Ok(parse_result) => parse_result,
            Err(error) => return syntax_error(cx, &error.to_string()),
        };

        let analyze_result = analyze(&mut parse_result, source);
        if let Err(errors) = analyze_result {
            // Choose an arbitrary syntax error to return
            let error = &errors.errors[0];
            return syntax_error(cx, &error.to_string());
        }

        let realm = cx.current_realm();
        let gen_result =
            BytecodeProgramGenerator::generate_from_program_parse_result(cx, &parse_result, realm);
        let bytecode_program = match gen_result {
            Ok(bytecode_program) => bytecode_program,
            Err(error) => return syntax_error(cx, &error.to_string()),
        };

        match cx.execute_program(bytecode_program) {
            Ok(value) => EvalResult::Ok(value),
            Err(error) => EvalResult::Throw(error),
        }
    }

    pub fn detach_array_buffer(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
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
