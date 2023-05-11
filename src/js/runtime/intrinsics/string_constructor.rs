use crate::{
    js::{
        common::unicode::CodePoint,
        runtime::{
            builtin_function::BuiltinFunction,
            completion::EvalResult,
            error::range_error_,
            function::get_argument,
            object_value::ObjectValue,
            property::Property,
            realm::Realm,
            string_object::StringObject,
            string_value::StringValue,
            type_utilities::{to_number, to_string, to_uint16},
            Context, Handle, Value,
        },
    },
    maybe,
};

use super::{intrinsics::Intrinsic, symbol_prototype::symbol_descriptive_string};

pub struct StringConstructor;

impl StringConstructor {
    // 22.1.2 Properties of the String Constructor
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.string(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::StringPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.intrinsic_func(cx, cx.names.from_char_code(), Self::from_char_code, 1, realm);
        func.intrinsic_func(cx, cx.names.from_code_point(), Self::from_code_point, 1, realm);

        func
    }

    // 22.1.1.1 String
    fn construct(
        cx: &mut Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let string_value = if arguments.is_empty() {
            cx.names.empty_string().as_string().into()
        } else {
            let value = get_argument(cx, arguments, 0);
            if new_target.is_none() && value.is_symbol() {
                return symbol_descriptive_string(cx, value.as_symbol()).into();
            }

            maybe!(to_string(cx, value))
        };

        match new_target {
            None => string_value.into(),
            Some(new_target) => {
                let string_object =
                    maybe!(StringObject::new_from_constructor(cx, new_target, string_value));
                string_object.into()
            }
        }
    }

    // 22.1.2.1 String.fromCharCode
    fn from_char_code(
        cx: &mut Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        // Common case, return a single code unit string
        if arguments.len() == 1 {
            let code_unit = maybe!(to_uint16(cx, arguments[0]));
            return StringValue::from_code_unit(cx, code_unit).into();
        }

        // Otherwise concatenate strings together
        let mut concat_string = cx.names.empty_string().as_string();

        for arg in arguments {
            let code_unit = maybe!(to_uint16(cx, *arg));
            let code_unit_string = StringValue::from_code_unit(cx, code_unit);
            concat_string = StringValue::concat(cx, concat_string, code_unit_string);
        }

        concat_string.into()
    }

    // 22.1.2.2 String.fromCodePoint
    fn from_code_point(
        cx: &mut Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        macro_rules! get_code_point {
            ($arg:expr) => {{
                let arg = $arg;
                let code_point = maybe!(to_number(cx, arg));

                // All valid code points are integers in the smi range
                if !code_point.is_smi() {
                    return range_error_(
                        cx,
                        &format!("invalid code point {}", code_point.as_number()),
                    );
                }

                let code_point = code_point.as_smi();

                if code_point < 0 || code_point > 0x10FFFF {
                    return range_error_(cx, &format!("invalid code point {}", code_point));
                }

                code_point as CodePoint
            }};
        }

        // Common case, return a single code unit string
        if arguments.len() == 1 {
            let code_point = get_code_point!(arguments[0]);
            return StringValue::from_code_point(cx, code_point).into();
        }

        // Otherwise concatenate strings together
        let mut concat_string = cx.names.empty_string().as_string();

        for arg in arguments {
            let code_point = get_code_point!(*arg);
            let code_point_string = StringValue::from_code_point(cx, code_point);
            concat_string = StringValue::concat(cx, concat_string, code_point_string);
        }

        concat_string.into()
    }
}
