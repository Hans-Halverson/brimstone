use std::rc::Rc;

use crate::js::{
    common::wtf_8::Wtf8String,
    parser::{
        analyze::analyze_function_for_function_constructor,
        parser::{
            parse_function_body_for_function_constructor, parse_function_for_function_constructor,
            parse_function_params_for_function_constructor,
        },
        source::Source,
    },
    runtime::{
        bytecode::{function::Closure, generator::BytecodeProgramGenerator},
        error::{syntax_error, syntax_parse_error},
        eval_result::EvalResult,
        intrinsics::{
            async_generator_prototype::AsyncGeneratorPrototype,
            generator_prototype::GeneratorPrototype, intrinsics::Intrinsic,
        },
        object_value::ObjectValue,
        ordinary_object::get_prototype_from_constructor,
        to_string, Context, Handle, Value,
    },
};

/// CreateDynamicFunction (https://tc39.es/ecma262/#sec-createdynamicfunction)
pub fn create_dynamic_function(
    mut cx: Context,
    constructor: Handle<ObjectValue>,
    new_target: Option<Handle<ObjectValue>>,
    args: &[Handle<Value>],
    is_async: bool,
    is_generator: bool,
) -> EvalResult<Handle<ObjectValue>> {
    let new_target = new_target.unwrap_or(constructor);

    let prefix;
    let fallback_proto;

    if is_generator {
        if is_async {
            prefix = "async function*";
            fallback_proto = Intrinsic::AsyncGeneratorFunctionPrototype;
        } else {
            prefix = "function*";
            fallback_proto = Intrinsic::GeneratorFunctionPrototype;
        }
    } else if is_async {
        prefix = "async function";
        fallback_proto = Intrinsic::AsyncFunctionPrototype;
    } else {
        prefix = "function";
        fallback_proto = Intrinsic::FunctionPrototype;
    }

    let arg_count = args.len();

    // Construct text for params, body, and entire function
    let mut params_string = Wtf8String::new();

    let body_arg = if arg_count == 0 {
        cx.names.empty_string().as_string().into()
    } else if arg_count == 1 {
        args[0]
    } else {
        params_string.push_wtf8_str(&to_string(cx, args[0])?.to_wtf8_string());

        for arg in &args[1..(args.len() - 1)] {
            params_string.push_char(',');
            params_string.push_wtf8_str(&to_string(cx, *arg)?.to_wtf8_string());
        }

        args[args.len() - 1]
    };

    // Build the body string
    let body_string = to_string(cx, body_arg)?;
    let body_string = {
        let mut builder = Wtf8String::new();

        builder.push_char('\n');
        builder.push_wtf8_str(&body_string.to_wtf8_string());
        builder.push_char('\n');

        builder
    };

    // Build the full source string
    let source_string = {
        let mut builder = Wtf8String::new();

        builder.push_str(prefix);
        builder.push_str(" anonymous(");
        builder.push_wtf8_str(&params_string);
        builder.push_str("\n) {");
        builder.push_wtf8_str(&body_string);
        builder.push_str("}");

        builder
    };

    // Use the file path of the active source file
    let file_path = cx.vm().current_source_file().path().to_string();

    // Make sure that parameter list and body are valid by themselves. Only need to check that they
    // parse correctly, full analysis will be performed on entire function text.
    let params_source = match Source::new_for_eval(file_path.clone(), params_string) {
        Ok(source) => Rc::new(source),
        Err(err) => return syntax_parse_error(cx, &err),
    };
    if let Err(err) = parse_function_params_for_function_constructor(
        &params_source,
        cx.options.clone(),
        is_async,
        is_generator,
    ) {
        return syntax_parse_error(cx, &err);
    }

    let body_source = match Source::new_for_eval(file_path.clone(), body_string) {
        Ok(source) => Rc::new(source),
        Err(err) => return syntax_parse_error(cx, &err),
    };
    if let Err(err) = parse_function_body_for_function_constructor(
        &body_source,
        cx.options.clone(),
        is_async,
        is_generator,
    ) {
        return syntax_parse_error(cx, &err);
    }

    // Parse and analyze entire function
    let full_source = match Source::new_for_eval(file_path, source_string) {
        Ok(source) => Rc::new(source),
        Err(err) => return syntax_parse_error(cx, &err),
    };
    let mut parse_result =
        match parse_function_for_function_constructor(&full_source, cx.options.clone()) {
            Ok(parse_result) => parse_result,
            Err(err) => return syntax_parse_error(cx, &err),
        };

    if let Err(errs) =
        analyze_function_for_function_constructor(&mut parse_result, full_source.clone())
    {
        return syntax_error(cx, &format!("could not parse function: {}", errs));
    }

    // Create function object
    let proto = get_prototype_from_constructor(cx, new_target, fallback_proto)?;

    // Generate bytecode for the function
    let realm = cx.current_realm();
    let generate_result = BytecodeProgramGenerator::generate_from_function_constructor_parse_result(
        cx,
        &parse_result,
        realm,
    );
    let bytecode_function = match generate_result {
        Ok(func) => func,
        Err(error) => return syntax_error(cx, &error.to_string()),
    };

    // Dynamic functions are always in the global scope
    let global_scope = realm.default_global_scope();
    let closure = Closure::new_with_proto(cx, bytecode_function, global_scope, proto);

    if is_generator {
        if is_async {
            AsyncGeneratorPrototype::install_on_async_generator_function(cx, closure)?;
        } else {
            GeneratorPrototype::install_on_generator_function(cx, closure)?;
        }
    }

    Ok(closure.as_object())
}
