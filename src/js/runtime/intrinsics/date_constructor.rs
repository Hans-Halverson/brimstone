use crate::{
    js::{
        common::time::get_current_unix_time,
        runtime::{
            builtin_function::BuiltinFunction,
            completion::EvalResult,
            function::get_argument,
            gc::Handle,
            intrinsics::{
                date_object::{make_date, make_day, make_time, time_clip, utc, DateObject},
                date_prototype::{this_date_value, to_date_string},
            },
            object_value::ObjectValue,
            realm::Realm,
            string_parsing::parse_string_to_date,
            to_string,
            type_utilities::{
                to_integer_or_infinity_f64, to_number, to_primitive, ToPrimitivePreferredType,
            },
            Context, Value,
        },
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct DateConstructor;

impl DateConstructor {
    // 21.4.2 The Date Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            7,
            cx.names.date(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::DatePrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.now(), Self::now, 0, realm);
        func.intrinsic_func(cx, cx.names.parse(), Self::parse, 1, realm);
        func.intrinsic_func(cx, cx.names.utc(), Self::utc, 7, realm);

        func
    }

    // 21.4.2.1 Date
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return to_date_string(cx, get_current_unix_time()).into();
        };

        let number_of_args = arguments.len();

        let date_value = if number_of_args == 0 {
            get_current_unix_time()
        } else if number_of_args == 1 {
            let date_value = if let Some(date_value_arg) =
                this_date_value(get_argument(cx, arguments, 0))
            {
                date_value_arg
            } else {
                let arg = get_argument(cx, arguments, 0);
                let primitive_value = maybe!(to_primitive(cx, arg, ToPrimitivePreferredType::None));

                if primitive_value.is_string() {
                    let primitive_string = primitive_value.as_string();
                    parse_string_to_date(primitive_string).unwrap_or(f64::NAN)
                } else {
                    maybe!(to_number(cx, primitive_value)).as_number()
                }
            };

            time_clip(date_value)
        } else {
            let year_arg = get_argument(cx, arguments, 0);
            let mut year = maybe!(to_number(cx, year_arg)).as_number();

            let month_arg = get_argument(cx, arguments, 1);
            let month = maybe!(to_number(cx, month_arg)).as_number();

            let day = if number_of_args > 2 {
                let day_arg = get_argument(cx, arguments, 2);
                maybe!(to_number(cx, day_arg)).as_number()
            } else {
                1.0
            };

            let hour = if number_of_args > 3 {
                let hour_arg = get_argument(cx, arguments, 3);
                maybe!(to_number(cx, hour_arg)).as_number()
            } else {
                0.0
            };

            let minute = if number_of_args > 4 {
                let minute_arg = get_argument(cx, arguments, 4);
                maybe!(to_number(cx, minute_arg)).as_number()
            } else {
                0.0
            };

            let second = if number_of_args > 5 {
                let second_arg = get_argument(cx, arguments, 5);
                maybe!(to_number(cx, second_arg)).as_number()
            } else {
                0.0
            };

            let millisecond = if number_of_args > 6 {
                let millisecond_arg = get_argument(cx, arguments, 6);
                maybe!(to_number(cx, millisecond_arg)).as_number()
            } else {
                0.0
            };

            // Two digit years are treated as 19XX
            if !year.is_nan() {
                let year_int = to_integer_or_infinity_f64(year);
                if (0.0..=99.0).contains(&year_int) {
                    year = 1900.0 + year_int;
                }
            }

            let final_day = make_day(year, month, day);
            let final_time = make_time(hour, minute, second, millisecond);
            let final_date = make_date(final_day, final_time);

            time_clip(utc(final_date))
        };

        maybe!(DateObject::new_from_constructor(cx, new_target, date_value))
            .to_handle()
            .into()
    }

    // 21.4.3.1 Date.now
    pub fn now(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        Value::from(get_current_unix_time()).to_handle(cx).into()
    }

    // 21.4.3.2 Date.parse
    pub fn parse(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let string_arg = get_argument(cx, arguments, 0);
        let string = maybe!(to_string(cx, string_arg));

        if let Some(date_value) = parse_string_to_date(string) {
            Value::from(date_value).to_handle(cx).into()
        } else {
            Value::nan().to_handle(cx).into()
        }
    }

    // 21.4.3.4 Date.UTC
    pub fn utc(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let number_of_args = arguments.len();

        let year_arg = get_argument(cx, arguments, 0);
        let mut year = maybe!(to_number(cx, year_arg)).as_number();

        let month = if number_of_args > 1 {
            let month_arg = get_argument(cx, arguments, 1);
            maybe!(to_number(cx, month_arg)).as_number()
        } else {
            0.0
        };

        let day = if number_of_args > 2 {
            let day_arg = get_argument(cx, arguments, 2);
            maybe!(to_number(cx, day_arg)).as_number()
        } else {
            1.0
        };

        let hour = if number_of_args > 3 {
            let hour_arg = get_argument(cx, arguments, 3);
            maybe!(to_number(cx, hour_arg)).as_number()
        } else {
            0.0
        };

        let minute = if number_of_args > 4 {
            let minute_arg = get_argument(cx, arguments, 4);
            maybe!(to_number(cx, minute_arg)).as_number()
        } else {
            0.0
        };

        let second = if number_of_args > 5 {
            let second_arg = get_argument(cx, arguments, 5);
            maybe!(to_number(cx, second_arg)).as_number()
        } else {
            0.0
        };

        let millisecond = if number_of_args > 6 {
            let millisecond_arg = get_argument(cx, arguments, 6);
            maybe!(to_number(cx, millisecond_arg)).as_number()
        } else {
            0.0
        };

        // Two digit years are treated as 19XX
        if !year.is_nan() {
            let year_int = to_integer_or_infinity_f64(year);
            if (0.0..=99.0).contains(&year_int) {
                year = 1900.0 + year_int;
            }
        }

        let final_day = make_day(year, month, day);
        let final_time = make_time(hour, minute, second, millisecond);
        let final_date = make_date(final_day, final_time);

        Value::from(time_clip(final_date)).to_handle(cx).into()
    }
}
