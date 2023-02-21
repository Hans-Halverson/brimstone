use crate::{
    js::{
        common::unicode::CodePoint,
        runtime::{
            builtin_function::BuiltinFunction,
            completion::EvalResult,
            error::range_error_,
            function::get_argument,
            gc::Gc,
            object_value::ObjectValue,
            ordinary_object::ordinary_create_from_constructor,
            property::Property,
            realm::Realm,
            string_object::StringObject,
            string_value::StringValue,
            type_utilities::{to_number, to_string, to_uint16},
            value::Value,
            Context,
        },
    },
    maybe,
};

use super::{intrinsics::Intrinsic, symbol_prototype::symbol_descriptive_string};

pub struct StringConstructor;

impl StringConstructor {
    // 22.1.2 Properties of the String Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            &cx.names.string(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::StringPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.intrinsic_func(cx, &cx.names.from_char_code(), Self::from_char_code, 1, realm);
        func.intrinsic_func(cx, &cx.names.from_code_point(), Self::from_code_point, 1, realm);

        func
    }

    // 22.1.1.1 String
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let string_value = if arguments.is_empty() {
            cx.names.empty_string().as_string().into()
        } else {
            let value = get_argument(arguments, 0);
            if new_target.is_none() && value.is_symbol() {
                return symbol_descriptive_string(cx, value.as_symbol()).into();
            }

            maybe!(to_string(cx, value))
        };

        match new_target {
            None => string_value.into(),
            Some(new_target) => {
                let object = maybe!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::StringPrototype
                ));

                let string_object = StringObject::new(cx, object, string_value);
                cx.heap.alloc(string_object).into()
            }
        }
    }

    // 22.1.2.1 String.fromCharCode
    fn from_char_code(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
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
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
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
