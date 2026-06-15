use std::{
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    common::wtf_8::Wtf8String,
    must_a,
    parser::source::Source,
    runtime::{
        Context, EvalResult, Handle, PropertyDescriptor, PropertyKey, Realm, Value,
        abstract_operations::{create_data_property_or_throw, define_property_or_throw},
        alloc_error::AllocResult,
        array_object::array_create_in_realm,
        builtin_function::BuiltinFunction,
        console::ConsoleObject,
        error::syntax_parse_error,
        eval::eval::evaluate_script,
        function::get_argument,
        gc_object::GcObject,
        intrinsics::{
            array_buffer_constructor::ArrayBufferObject, error_constructor::ErrorObject,
            intrinsics::Intrinsic, rust_runtime::RuntimeFunctionPtr,
        },
        object_value::ObjectValue,
        to_string,
    },
};

/// Global methods used in the shell for other engines, used for compatibility with some third-party
/// test suites.
pub struct TestShell;

impl TestShell {
    pub fn install(cx: Context, realm: Handle<Realm>) -> AllocResult<()> {
        // Install the global functions used in the compatibility test shell
        Self::install_custom_function(cx, realm, "gc", GcObject::run, 0)?;
        Self::install_custom_function(cx, realm, "load", Self::load, 1)?;
        Self::install_custom_function(cx, realm, "loadString", Self::load_string, 1)?;
        Self::install_custom_function(cx, realm, "print", ConsoleObject::log, 0)?;
        Self::install_custom_function(cx, realm, "printErr", ConsoleObject::log, 0)?;
        Self::install_custom_function(cx, realm, "read", Self::read, 1)?;
        Self::install_custom_function(cx, realm, "readFile", Self::read, 1)?;
        Self::install_custom_function(cx, realm, "runString", Self::run_string, 1)?;

        // Install the `performance.now` method
        let performance_object = Self::install_performance_object(cx, realm)?;
        Self::install_custom_function_on_object(
            cx,
            realm,
            performance_object,
            "now",
            Self::performance_now,
            0,
        )?;

        // Install the global `arguments` array
        Self::install_script_arguments(cx, realm)?;

        Ok(())
    }

    fn install_custom_function(
        cx: Context,
        realm: Handle<Realm>,
        name: &'static str,
        function_ptr: RuntimeFunctionPtr,
        length: u32,
    ) -> AllocResult<()> {
        Self::install_custom_function_on_object(
            cx,
            realm,
            realm.global_object(),
            name,
            function_ptr,
            length,
        )
    }

    fn install_custom_function_on_object(
        mut cx: Context,
        realm: Handle<Realm>,
        object: Handle<ObjectValue>,
        name: &'static str,
        function_ptr: RuntimeFunctionPtr,
        length: u32,
    ) -> AllocResult<()> {
        let function_id = cx.rust_runtime_functions.register(function_ptr).unwrap();

        let name_string = cx.alloc_static_string(name)?;
        let name_key = PropertyKey::string_handle(cx, name_string)?;
        let fuzzilli_function =
            BuiltinFunction::create_custom(cx, function_id, length, name_key, realm, None)?;

        let desc = PropertyDescriptor::data(fuzzilli_function.as_value(), true, false, true);
        must_a!(define_property_or_throw(cx, object, name_key, desc));

        Ok(())
    }

    fn install_performance_object(
        mut cx: Context,
        realm: Handle<Realm>,
    ) -> AllocResult<Handle<ObjectValue>> {
        let performance_object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        let name_key = PropertyKey::string_handle(cx, cx.alloc_static_string("performance")?)?;
        let desc = PropertyDescriptor::data(performance_object.as_value(), true, false, true);
        must_a!(define_property_or_throw(cx, realm.global_object(), name_key, desc));

        Ok(performance_object)
    }

    /// Install the script arguments passed after `--` as a global `arguments` array.
    fn install_script_arguments(mut cx: Context, realm: Handle<Realm>) -> AllocResult<()> {
        let array = must_a!(array_create_in_realm(cx, realm, 0, None));

        for (index, arg) in cx.options.script_args.clone().iter().enumerate() {
            let arg_value = must_a!(cx.alloc_string(arg)).as_value();
            let index_key = PropertyKey::from_u64(cx, index as u64)?.to_handle(cx);
            must_a!(create_data_property_or_throw(cx, array.as_object(), index_key, arg_value));
        }

        let arguments_key = PropertyKey::string_handle(cx, cx.alloc_static_string("arguments")?)?;
        realm
            .global_object()
            .intrinsic_data_prop(cx, arguments_key, array.as_value())?;

        Ok(())
    }

    /// Read a file at the given path and evaluate it as a script in the current realm.
    fn load(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let path_arg = get_argument(cx, arguments, 0);
        let path = resolve_path_arg(cx, path_arg)?;

        let source = match Source::new_from_file(&path.to_string_lossy()) {
            Ok(source) => Rc::new(source),
            Err(error) => return syntax_parse_error(cx, &error),
        };

        evaluate_script(cx, cx.current_realm(), source)
    }

    /// Evaluate a script in the current realm.
    fn load_string(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let script_arg = get_argument(cx, arguments, 0);
        let script_string = to_string(cx, script_arg)?.to_wtf8_string()?;
        let source = source_from_string(cx, script_string)?;

        evaluate_script(cx, cx.current_realm(), source)
    }

    /// Read a file to either a string or an ArrayBuffer depending on the value of the second
    /// argument.
    fn read(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let path_arg = get_argument(cx, arguments, 0);
        let path = resolve_path_arg(cx, path_arg)?;

        let mode_arg = get_argument(cx, arguments, 1);
        let is_binary = mode_arg.is_string() && mode_arg.as_string().flatten()?.eq_str("binary");

        if is_binary {
            let bytes = match std::fs::read(&path) {
                Ok(bytes) => bytes,
                Err(_) => return read_file_error(cx, &path),
            };

            let buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
            let mut array_buffer = ArrayBufferObject::new(
                cx,
                buffer_constructor,
                bytes.len(),
                /* max_byte_length */ None,
                /* data */ None,
            )?;
            array_buffer.data_mut().copy_from_slice(&bytes);

            Ok(array_buffer.as_value())
        } else {
            let string = match std::fs::read_to_string(&path) {
                Ok(string) => string,
                Err(_) => return read_file_error(cx, &path),
            };

            Ok(cx.alloc_string(&string)?.as_value())
        }
    }

    fn performance_now(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let time_origin = cx.current_realm().time_origin();
        let duration_millis = time_origin.elapsed().as_secs_f64() * 1000.0;

        Ok(cx.number(duration_millis))
    }

    /// Create a new realm, evaluate the script in it, and return the new realm's global object.
    fn run_string(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let script_arg = get_argument(cx, arguments, 0);
        let script_string = to_string(cx, script_arg)?.to_wtf8_string()?;
        let source = source_from_string(cx, script_string)?;

        let mut new_realm = Realm::new(cx)?;
        new_realm.install_optional_globals(cx)?;

        evaluate_script(cx, new_realm, source)?;

        Ok(new_realm.global_object().as_value())
    }
}

/// Resolve a path argument relative to the directory of the currently executing source
/// file. Handle both absolute and relative paths.
fn resolve_path_arg(mut cx: Context, path_arg: Handle<Value>) -> EvalResult<PathBuf> {
    let path_string = to_string(cx, path_arg)?.flatten()?.to_string();
    let path = Path::new(&path_string);

    if path.is_absolute() {
        return Ok(path.to_path_buf());
    }

    let Some(current_source_file) = cx.vm().current_source_file() else {
        return Ok(path.to_path_buf());
    };

    let current_source_file_path = current_source_file.path().to_string();
    let Some(current_source_file_dir) = Path::new(&current_source_file_path).parent() else {
        return Ok(path.to_path_buf());
    };

    Ok(current_source_file_dir.join(path))
}

fn source_from_string(mut cx: Context, string: Wtf8String) -> EvalResult<Rc<Source>> {
    // Use the file path of the active source file
    let file_path = cx
        .vm()
        .current_source_file()
        .map_or_else(|| "<anonymous>".to_string(), |source| source.path().to_string());

    match Source::new_for_string(&file_path, string) {
        Ok(source) => Ok(Rc::new(source)),
        Err(err) => syntax_parse_error(cx, &err),
    }
}

fn read_file_error(cx: Context, path: &Path) -> EvalResult<Handle<Value>> {
    let error_object =
        ErrorObject::new_with_message(cx, format!("failed to read {}", path.to_string_lossy()))?;
    Ok(error_object.as_value())
}
