use std::rc::Rc;

use crate::{
    handle_scope, must_a,
    parser::source::Source,
    runtime::{
        Context, EvalResult, Handle, PropertyKey, Realm,
        abstract_operations::set,
        alloc_error::AllocResult,
        error::{syntax_parse_error, type_error},
        eval::eval::evaluate_script,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            array_buffer_constructor::ArrayBufferObject, intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        string_value::StringValue,
    },
    runtime_fn,
};

/// Utility functions used in test262 tests. Must be included in the main library for now so that
/// builtin functions can be registered.
pub struct Test262Object;

impl Test262Object {
    fn new(mut cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        let create_realm_string = cx.alloc_static_string("createRealm")?;
        let create_realm_key = PropertyKey::string_handle(cx, create_realm_string)?;
        builder.method(create_realm_key, RuntimeFunction::Test262Object_create_realm, 0)?;

        let eval_script_string = cx.alloc_static_string("evalScript")?;
        let eval_script_key = PropertyKey::string_handle(cx, eval_script_string)?;
        builder.method(eval_script_key, RuntimeFunction::Test262Object_eval_script, 1)?;

        let global_string = cx.alloc_static_string("global")?;
        let global_key = PropertyKey::string_handle(cx, global_string)?;
        builder.data(global_key, realm.global_object().into())?;

        let detach_array_buffer_string = cx.alloc_static_string("detachArrayBuffer")?;
        let detach_array_buffer_key = PropertyKey::string_handle(cx, detach_array_buffer_string)?;
        builder.method(
            detach_array_buffer_key,
            RuntimeFunction::Test262Object_detach_array_buffer,
            1,
        )?;

        builder.method(cx.names.gc(), RuntimeFunction::GcObject_run, 0)?;

        builder.build()
    }

    pub fn install(mut cx: Context, realm: Handle<Realm>) -> AllocResult<()> {
        handle_scope!(cx, {
            // Create the test262 object
            let test_262_object = Test262Object::new(cx, realm)?;

            let mut builder = IntrinsicBuilder::new(cx, realm, realm.global_object());

            // Install the "$262" property on the global object
            builder.data(test_262_key(cx)?, test_262_object.into())?;

            // Also install a global print function needed in tests
            let print_string = cx.alloc_static_string("print")?;
            let print_key = PropertyKey::string_handle(cx, print_string)?;
            builder.method(print_key, RuntimeFunction::Test262Object_print, 1)?;

            builder.build()?;

            // Install the global print log property
            must_a!(Self::set_print_log(
                cx,
                realm.global_object(),
                cx.names.empty_string().as_string()
            ));

            Ok(())
        })
    }

    fn print_log_key(mut cx: Context) -> AllocResult<Handle<PropertyKey>> {
        let print_log_string = cx.alloc_static_string("$$printLog")?;
        PropertyKey::string_handle(cx, print_log_string)
    }

    pub fn get_print_log(
        cx: Context,
        global_object: Handle<ObjectValue>,
    ) -> EvalResult<Handle<StringValue>> {
        let print_log = get(cx, global_object, Self::print_log_key(cx)?)?;
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
        set(cx, global_object, Self::print_log_key(cx)?, print_log.into(), true)
    }

    runtime_fn! {
    /// Adds strings to a running print log stored on the global object.
    fn print(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        if !argument.is_string() {
            return type_error(cx, "print expects a string");
        }

        let global_object = cx.current_realm_ptr().global_object();

        let old_print_log = Self::get_print_log(cx, global_object)?;
        let new_print_log = StringValue::concat(cx, old_print_log, argument.as_string())?;
        Self::set_print_log(cx, global_object, new_print_log)?;

        Ok(cx.undefined())
    }}

    runtime_fn! {
    fn create_realm(cx, _, _) {
        // Create a new realm that also has the test262 object installed
        let mut realm = Realm::new(cx)?;
        realm.install_optional_globals(cx)?;

        get(cx, realm.global_object(), test_262_key(cx)?)
    }}

    runtime_fn! {
    fn eval_script(cx, _, arguments) {
        let script_text = arguments.get(cx, 0);
        if !script_text.is_string() {
            return type_error(cx, "expected a string");
        }

        // Use the file path of the active source file
        let file_path = cx
            .vm()
            .current_source_file()
            .map_or_else(|| "<anonymous>".to_string(), |source| source.path().to_string());

        let source =
            match Source::new_for_eval(file_path, script_text.as_string().to_wtf8_string()?) {
                Ok(source) => Rc::new(source),
                Err(error) => return syntax_parse_error(cx, &error),
            };

        evaluate_script(cx, cx.current_realm(), source)
    }}

    runtime_fn! {
    fn detach_array_buffer(cx, _, arguments) {
        let value = arguments.get(cx, 0);
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
    }}
}

fn test_262_key(mut cx: Context) -> AllocResult<Handle<PropertyKey>> {
    let test_262_string = cx.alloc_static_string("$262")?;
    PropertyKey::string_handle(cx, test_262_string)
}
