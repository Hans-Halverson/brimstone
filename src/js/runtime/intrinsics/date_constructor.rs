use crate::{
    runtime::{
        Context,
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        gc::Handle,
        intrinsics::{
            date_object::{
                DateObject, make_date, make_day, make_full_year, make_time, time_clip, utc,
            },
            date_prototype::{to_date_string, validate_date_value},
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        string_parsing::parse_string_to_date,
        to_string,
        type_utilities::{ToPrimitivePreferredType, to_number, to_primitive},
    },
    runtime_fn,
};

pub struct DateConstructor;

impl DateConstructor {
    /// The Date Constructor (https://tc39.es/ecma262/#sec-date-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::DateConstructor_construct,
            7,
            cx.names.date(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::DatePrototype).into(),
        )?;

        func.intrinsic_func(cx, cx.names.now_(), RuntimeFunction::DateConstructor_now, 0, realm)?;
        func.intrinsic_func(
            cx,
            cx.names.parse(),
            RuntimeFunction::DateConstructor_parse,
            1,
            realm,
        )?;
        func.intrinsic_func(cx, cx.names.utc(), RuntimeFunction::DateConstructor_utc, 7, realm)?;

        Ok(func)
    }

    runtime_fn! {
    /// Date (https://tc39.es/ecma262/#sec-date)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return Ok(to_date_string(cx, cx.current_unix_time_millis() as f64)?.as_value());
        };

        let number_of_args = arguments.len();

        let date_value = if number_of_args == 0 {
            cx.current_unix_time_millis() as f64
        } else if number_of_args == 1 {
            let date_value = if let Some(date_value_arg) = validate_date_value(arguments.get(cx, 0))
            {
                date_value_arg
            } else {
                let arg = arguments.get(cx, 0);
                let primitive_value = to_primitive(cx, arg, ToPrimitivePreferredType::None)?;

                if primitive_value.is_string() {
                    let primitive_string = primitive_value.as_string();
                    parse_string_to_date(primitive_string)?.unwrap_or(f64::NAN)
                } else {
                    to_number(cx, primitive_value)?.as_number()
                }
            };

            time_clip(date_value)
        } else {
            let year_arg = arguments.get(cx, 0);
            let mut year = to_number(cx, year_arg)?.as_number();

            let month_arg = arguments.get(cx, 1);
            let month = to_number(cx, month_arg)?.as_number();

            let day = if number_of_args > 2 {
                let day_arg = arguments.get(cx, 2);
                to_number(cx, day_arg)?.as_number()
            } else {
                1.0
            };

            let hour = if number_of_args > 3 {
                let hour_arg = arguments.get(cx, 3);
                to_number(cx, hour_arg)?.as_number()
            } else {
                0.0
            };

            let minute = if number_of_args > 4 {
                let minute_arg = arguments.get(cx, 4);
                to_number(cx, minute_arg)?.as_number()
            } else {
                0.0
            };

            let second = if number_of_args > 5 {
                let second_arg = arguments.get(cx, 5);
                to_number(cx, second_arg)?.as_number()
            } else {
                0.0
            };

            let millisecond = if number_of_args > 6 {
                let millisecond_arg = arguments.get(cx, 6);
                to_number(cx, millisecond_arg)?.as_number()
            } else {
                0.0
            };

            year = make_full_year(year);

            let final_day = make_day(year, month, day);
            let final_time = make_time(hour, minute, second, millisecond);
            let final_date = make_date(final_day, final_time);

            time_clip(utc(final_date))
        };

        Ok(DateObject::new_from_constructor(cx, new_target, date_value)?
            .to_handle()
            .into())
    }}

    runtime_fn! {
    /// Date.now (https://tc39.es/ecma262/#sec-date.now)
    fn now(cx, _, _) {
        Ok(cx.number(cx.current_unix_time_millis() as f64))
    }}

    runtime_fn! {
    /// Date.parse (https://tc39.es/ecma262/#sec-date.parse)
    fn parse(cx, _, arguments) {
        let string_arg = arguments.get(cx, 0);
        let string = to_string(cx, string_arg)?;

        if let Some(date_value) = parse_string_to_date(string)? {
            Ok(cx.number(date_value))
        } else {
            Ok(cx.nan())
        }
    }}

    runtime_fn! {
    /// Date.UTC (https://tc39.es/ecma262/#sec-date.utc)
    fn utc(cx, _, arguments) {
        let number_of_args = arguments.len();

        let year_arg = arguments.get(cx, 0);
        let mut year = to_number(cx, year_arg)?.as_number();

        let month = if number_of_args > 1 {
            let month_arg = arguments.get(cx, 1);
            to_number(cx, month_arg)?.as_number()
        } else {
            0.0
        };

        let day = if number_of_args > 2 {
            let day_arg = arguments.get(cx, 2);
            to_number(cx, day_arg)?.as_number()
        } else {
            1.0
        };

        let hour = if number_of_args > 3 {
            let hour_arg = arguments.get(cx, 3);
            to_number(cx, hour_arg)?.as_number()
        } else {
            0.0
        };

        let minute = if number_of_args > 4 {
            let minute_arg = arguments.get(cx, 4);
            to_number(cx, minute_arg)?.as_number()
        } else {
            0.0
        };

        let second = if number_of_args > 5 {
            let second_arg = arguments.get(cx, 5);
            to_number(cx, second_arg)?.as_number()
        } else {
            0.0
        };

        let millisecond = if number_of_args > 6 {
            let millisecond_arg = arguments.get(cx, 6);
            to_number(cx, millisecond_arg)?.as_number()
        } else {
            0.0
        };

        year = make_full_year(year);

        let final_day = make_day(year, month, day);
        let final_time = make_time(hour, minute, second, millisecond);
        let final_date = make_date(final_day, final_time);

        Ok(cx.number(time_clip(final_date)))
    }}
}
