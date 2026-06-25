use temporal_rs::{PlainMonthDay, options::Overflow};

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
            plain_month_day_object::PlainMonthDayObject,
            utils::{
                get_calendar_identifier_with_iso_default, map_temporal_result,
                parse_calendar_argument, to_integer_with_truncation,
            },
        },
    },
    object_value::ObjectValue,
};

pub struct PlainMonthDayConstructor;

impl PlainMonthDayConstructor {
    /// Temporal.PlainMonthDay Constructor (https://tc39.es/proposal-temporal/#sec-temporal-plainmonthday-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::PlainMonthDayConstructor_construct,
            2,
            cx.names.plain_month_day(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::PlainMonthDayPrototype)
                .into(),
        )?;

        func.intrinsic_func(
            cx,
            cx.names.from(),
            RuntimeFunction::PlainMonthDayConstructor_from,
            1,
            realm,
        )?;

        Ok(func)
    }

    /// Temporal.PlainMonthDay (https://tc39.es/proposal-temporal/#sec-temporal-plainmonthday)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Temporal.PlainMonthDay constructor";

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.PlainMonthDay constructor must be called with new");
        };

        let month_arg = get_argument(cx, arguments, 0);
        let day_arg = get_argument(cx, arguments, 1);

        let month = to_integer_with_truncation(cx, month_arg, NAME)?;
        let day = to_integer_with_truncation(cx, day_arg, NAME)?;

        let calendar_arg = get_argument(cx, arguments, 2);
        let calendar = parse_calendar_argument(cx, calendar_arg, NAME)?;

        let reference_iso_year_arg = get_argument(cx, arguments, 3);
        let reference_iso_year = if reference_iso_year_arg.is_undefined() {
            None
        } else {
            Some(to_integer_with_truncation(cx, reference_iso_year_arg, NAME)?)
        };

        let plain_month_day_result = PlainMonthDay::new_with_overflow(
            month,
            day,
            calendar,
            Overflow::Reject,
            reference_iso_year,
        );
        let plain_month_day = map_temporal_result(cx, plain_month_day_result, NAME)?;

        Ok(PlainMonthDayObject::new_from_constructor(cx, new_target, plain_month_day)?.as_value())
    }

    /// Temporal.PlainMonthDay.from (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.from)
    pub fn from(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let item_arg = get_argument(cx, arguments, 0);
        let plain_month_day = to_temporal_month_day(cx, item_arg, "PlainMonthDay.from")?;

        Ok(PlainMonthDayObject::new(cx, plain_month_day)?.as_value())
    }
}

/// ToTemporalMonthDay (https://tc39.es/proposal-temporal/#sec-temporal-totemporalmonthday)
pub fn to_temporal_month_day(
    cx: Context,
    item: Handle<Value>,
    method_name: &str,
) -> EvalResult<PlainMonthDay> {
    if item.is_object() {
        // Check if item is a Temporal object of some kind
        let item_object = item.as_object();
        if let Some(plain_month_day) = item_object.as_plain_month_day_object() {
            return Ok(plain_month_day.month_day().clone());
        }

        let _ = get_calendar_identifier_with_iso_default(cx, item_object, method_name)?;

        // Otherwise treat like a date-like object
        unimplemented!("ToTemporalMonthDay for date-like object")
    }

    // Otherwise parse PlainMonthDay from string
    if !item.is_string() {
        return type_error(cx, &format!("{method_name} month-day must be a string or object"));
    }

    let item_string = item.as_string().to_wtf8_string()?;

    let parsed_result = PlainMonthDay::from_utf8(item_string.as_bytes());
    let parsed = map_temporal_result(cx, parsed_result, method_name)?;

    Ok(parsed)
}
