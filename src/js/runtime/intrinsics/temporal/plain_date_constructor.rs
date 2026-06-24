use temporal_rs::{Calendar, PlainDate};

use crate::runtime::{
    Context, Handle, Realm, Value,
    alloc_error::AllocResult,
    builtin_function::BuiltinFunction,
    error::type_error,
    eval_result::EvalResult,
    function::get_argument,
    intrinsics::{
        intrinsics::Intrinsic,
        rust_runtime::RuntimeFunction,
        temporal::{
            plain_date_object::PlainDateObject,
            utils::{
                clamp_day_arg_for_temporal_rs, clamp_month_arg_for_temporal_rs,
                clamp_year_arg_for_temporal_rs, get_overflow_option, map_temporal_result,
                validate_options_object,
            },
        },
    },
    object_value::ObjectValue,
    type_utilities::to_integer_with_truncation,
};

pub struct PlainDateConstructor;

impl PlainDateConstructor {
    /// Temporal.PlainDate Constructor (https://tc39.es/proposal-temporal/#sec-temporal-plaindate-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::PlainDateConstructor_construct,
            3,
            cx.names.plain_date(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::PlainDatePrototype).into(),
        )?;

        func.intrinsic_func(
            cx,
            cx.names.compare(),
            RuntimeFunction::PlainDateConstructor_compare,
            2,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.from(),
            RuntimeFunction::PlainDateConstructor_from,
            1,
            realm,
        )?;

        Ok(func)
    }

    /// Temporal.PlainDate (https://tc39.es/proposal-temporal/#sec-temporal-plaindate)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Temporal.PlainDate";

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.PlainDate constructor must be called with new");
        };

        let year_arg = get_argument(cx, arguments, 0);
        let month_arg = get_argument(cx, arguments, 1);
        let day_arg = get_argument(cx, arguments, 2);

        // Convert year, month, and day arguments into truncated integers
        let year_trunc =
            to_integer_with_truncation(cx, year_arg, "Temporal.PlainDate year argument")?;
        let month_trunc =
            to_integer_with_truncation(cx, month_arg, "Temporal.PlainDate month argument")?;
        let day_trunc = to_integer_with_truncation(cx, day_arg, "Temporal.PlainDate day argument")?;

        // Validate calendar argument
        let calendar_arg = get_argument(cx, arguments, 3);
        let calendar = if calendar_arg.is_undefined() {
            Calendar::ISO
        } else if !calendar_arg.is_string() {
            return type_error(cx, "Temporal.PlainDate calendar argument must be a string");
        } else {
            let wtf8_string = calendar_arg.as_string().to_wtf8_string()?;
            let calendar_result = Calendar::try_from_utf8(wtf8_string.as_bytes());
            map_temporal_result(cx, calendar_result, NAME)?
        };

        // Clamp year, month, and day into range for `brimstone_rs`
        let year = clamp_year_arg_for_temporal_rs(cx, year_trunc, NAME)?;
        let month = clamp_month_arg_for_temporal_rs(cx, month_trunc, NAME)?;
        let day = clamp_day_arg_for_temporal_rs(cx, day_trunc, NAME)?;

        let plain_date_result = PlainDate::try_new(year, month, day, calendar);
        let plain_date = map_temporal_result(cx, plain_date_result, NAME)?;

        Ok(PlainDateObject::new_from_constructor(cx, new_target, plain_date)?.as_value())
    }

    /// Temporal.PlainDate.compare (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.compare)
    pub fn compare(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDate.compare";

        let arg_1 = get_argument(cx, arguments, 0);
        let arg_2 = get_argument(cx, arguments, 1);

        let date_1 = to_temporal_date(cx, arg_1, None, NAME)?;
        let date_2 = to_temporal_date(cx, arg_2, None, NAME)?;

        Ok(cx.smi(date_1.compare_iso(&date_2) as i32))
    }

    /// Temporal.PlainDate.from (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.from)
    pub fn from(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let item_arg = get_argument(cx, arguments, 0);
        let options_arg = get_argument(cx, arguments, 1);

        let plain_date = to_temporal_date(cx, item_arg, Some(options_arg), "PlainDate.from")?;

        Ok(PlainDateObject::new(cx, plain_date)?.as_value())
    }
}

/// ToTemporalDate (https://tc39.es/proposal-temporal/#sec-temporal-totemporaldate)
pub fn to_temporal_date(
    cx: Context,
    item: Handle<Value>,
    options: Option<Handle<Value>>,
    method_name: &str,
) -> EvalResult<PlainDate> {
    fn validate_overflow_option(
        cx: Context,
        options: Handle<Value>,
        method_name: &str,
    ) -> EvalResult<()> {
        let options = validate_options_object(cx, options, method_name)?;
        get_overflow_option(cx, options, method_name)?;

        Ok(())
    }

    let options = options.unwrap_or(cx.undefined());

    if item.is_object() {
        // Check if item is a Temporal object of some kind
        let item_object = item.as_object();
        if let Some(plain_date) = item_object.as_plain_date_object() {
            validate_overflow_option(cx, options, method_name)?;
            return Ok(plain_date.date().clone());
        }

        // TODO: Check for DateTime and ZonedDateTime objects

        // Otherwise treat as a "date-like" object
        validate_overflow_option(cx, options, method_name)?;

        unimplemented!("ToTemporalDate for date-like object")
    }

    // Otherwise parse PlainDate from string
    if !item.is_string() {
        return type_error(cx, &format!("{method_name} date must be a string"));
    }

    let item_string = item.as_string().to_wtf8_string()?;

    let parsed_date_result = PlainDate::from_utf8(item_string.as_bytes());
    let parsed_date = map_temporal_result(cx, parsed_date_result, method_name)?;

    validate_overflow_option(cx, options, method_name)?;

    Ok(parsed_date)
}
