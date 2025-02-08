use std::rc::Rc;

use crate::{
    js::{
        parser::{analyze::analyze, parse_script, source::Source},
        runtime::{bytecode::generator::BytecodeProgramGenerator, get},
    },
    must,
};

use super::{
    abstract_operations::set,
    error::{syntax_error, syntax_parse_error, type_error},
    function::get_argument,
    gc::HandleScope,
    gc_object::GcObject,
    intrinsics::{array_buffer_constructor::ArrayBufferObject, intrinsics::Intrinsic},
    object_value::ObjectValue,
    string_value::StringValue,
    Context, EvalResult, Handle, PropertyKey, Realm, Value,
};

/// Utility functions used in test262 tests. Must be included in the main library for now so that
/// builtin functions can be registered.
pub struct Test262Object;

impl Test262Object {
    fn new(mut cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        let create_realm_string = cx.alloc_string("createRealm").as_string();
        let create_realm_key = PropertyKey::string(cx, create_realm_string).to_handle(cx);
        object.intrinsic_func(cx, create_realm_key, Self::create_realm, 0, realm);

        let eval_script_string = cx.alloc_string("evalScript").as_string();
        let eval_script_key = PropertyKey::string(cx, eval_script_string).to_handle(cx);
        object.intrinsic_func(cx, eval_script_key, Self::eval_script, 1, realm);

        let global_string = cx.alloc_string("global").as_string();
        let global_key = PropertyKey::string(cx, global_string).to_handle(cx);
        object.intrinsic_data_prop(cx, global_key, realm.global_object().into());

        let detach_array_buffer_string = cx.alloc_string("detachArrayBuffer").as_string();
        let detach_array_buffer_key =
            PropertyKey::string(cx, detach_array_buffer_string).to_handle(cx);
        object.intrinsic_func(cx, detach_array_buffer_key, Self::detach_array_buffer, 1, realm);

        object.intrinsic_func(cx, cx.names.gc(), GcObject::run, 0, realm);

        object.to_handle()
    }

    pub fn install(cx: Context, realm: Handle<Realm>) {
        HandleScope::new(cx, |mut cx| {
            // Create the test262 object
            let test_262_object = Test262Object::new(cx, realm);

            // Install the the "$262" property on the global object
            realm
                .global_object()
                .intrinsic_data_prop(cx, test_262_key(cx), test_262_object.into());

            // Also install a global print function needed in tests
            let print_string = cx.alloc_string("print").as_string();
            let print_key = PropertyKey::string(cx, print_string).to_handle(cx);
            realm
                .global_object()
                .intrinsic_func(cx, print_key, Self::print, 1, realm);

            // Install the global print log property
            must!(Self::set_print_log(
                cx,
                realm.global_object(),
                cx.names.empty_string().as_string()
            ));
        });
    }

    fn print_log_key(mut cx: Context) -> Handle<PropertyKey> {
        let print_log_string = cx.alloc_string("$$printLog").as_string();
        PropertyKey::string(cx, print_log_string).to_handle(cx)
    }

    pub fn get_print_log(
        cx: Context,
        global_object: Handle<ObjectValue>,
    ) -> EvalResult<Handle<StringValue>> {
        let print_log = get(cx, global_object, Self::print_log_key(cx))?;
        if !print_log.is_string() {
            return type_error(cx, "printLog must be a string");
        }

        Ok(print_log.as_string())
    }

    fn set_print_log(
        cx: Context,
        global_object: Handle<ObjectValue>,
        print_log: Handle<StringValue>,
    ) -> EvalResult<()> {
        set(cx, global_object, Self::print_log_key(cx), print_log.into(), true)
    }

    /// Adds strings to a running print log stored on the global object.
    pub fn print(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        if !argument.is_string() {
            return type_error(cx, "print expects a string");
        }

        let global_object = cx.current_realm_ptr().global_object();

        let old_print_log = Self::get_print_log(cx, global_object)?;
        let new_print_log = StringValue::concat(cx, old_print_log, argument.as_string());
        Self::set_print_log(cx, global_object, new_print_log)?;

        Ok(cx.undefined())
    }

    pub fn create_realm(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        // Create a new realm that also has the test262 object installed
        let realm = Realm::new(cx);
        Test262Object::install(cx, realm);

        get(cx, realm.global_object(), test_262_key(cx))
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

        // Use the file path of the active source file
        let file_path = cx.vm().current_source_file().path().to_string();

        let source = match Source::new_for_eval(file_path, script_text.as_string().to_wtf8_string())
        {
            Ok(source) => Rc::new(source),
            Err(error) => return syntax_parse_error(cx, &error),
        };

        let parse_result = parse_script(&source, cx.options.as_ref());
        let mut parse_result = match parse_result {
            Ok(parse_result) => parse_result,
            Err(error) => return syntax_parse_error(cx, &error),
        };

        let analyze_result = analyze(&mut parse_result);
        if let Err(errors) = analyze_result {
            // Choose an arbitrary syntax error to return
            let error = &errors.errors[0];
            return syntax_parse_error(cx, error);
        }

        let realm = cx.current_realm();
        let gen_result =
            BytecodeProgramGenerator::generate_from_parse_script_result(cx, &parse_result, realm);
        let bytecode_script = match gen_result {
            Ok(bytecode_script) => bytecode_script,
            Err(error) => return syntax_error(cx, &error.to_string()),
        };

        cx.vm().execute_script(bytecode_script)
    }

    pub fn detach_array_buffer(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return Ok(cx.undefined());
        }

        let object = value.as_object();
        if !object.is_array_buffer() {
            return Ok(cx.undefined());
        }

        let mut array_buffer = object.cast::<ArrayBufferObject>();
        array_buffer.detach();

        Ok(cx.undefined())
    }
}

fn test_262_key(mut cx: Context) -> Handle<PropertyKey> {
    let test_262_string = cx.alloc_string("$262").as_string();
    PropertyKey::string(cx, test_262_string).to_handle(cx)
}
