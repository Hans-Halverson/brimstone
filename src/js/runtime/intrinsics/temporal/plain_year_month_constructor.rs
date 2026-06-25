use temporal_rs::PlainYearMonth;

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
            plain_year_month_object::PlainYearMonthObject,
            utils::{map_temporal_result, parse_calendar_argument, to_integer_with_truncation},
        },
    },
    object_value::ObjectValue,
};

pub struct PlainYearMonthConstructor;

impl PlainYearMonthConstructor {
    /// Temporal.PlainYearMonth Constructor (https://tc39.es/proposal-temporal/#sec-temporal-plainyearmonth-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::PlainYearMonthConstructor_construct,
            2,
            cx.names.plain_year_month(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::PlainYearMonthPrototype)
                .into(),
        )?;

        func.intrinsic_func(
            cx,
            cx.names.from(),
            RuntimeFunction::PlainYearMonthConstructor_from,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.compare(),
            RuntimeFunction::PlainYearMonthConstructor_compare,
            2,
            realm,
        )?;

        Ok(func)
    }

    /// Temporal.PlainYearMonth (https://tc39.es/proposal-temporal/#sec-temporal-plainyearmonth)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Temporal.PlainYearMonth constructor";

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.PlainYearMonth constructor must be called with new");
        };

        let year_arg = get_argument(cx, arguments, 0);
        let month_arg = get_argument(cx, arguments, 1);

        let year = to_integer_with_truncation(cx, year_arg, NAME)?;
        let month = to_integer_with_truncation(cx, month_arg, NAME)?;

        let calendar_arg = get_argument(cx, arguments, 2);
        let calendar = parse_calendar_argument(cx, calendar_arg, NAME)?;

        let reference_iso_day_arg = get_argument(cx, arguments, 3);
        let reference_iso_day = if reference_iso_day_arg.is_undefined() {
            None
        } else {
            Some(to_integer_with_truncation(cx, reference_iso_day_arg, NAME)?)
        };

        let plain_year_month_result =
            PlainYearMonth::try_new(year, month, reference_iso_day, calendar);
        let plain_year_month = map_temporal_result(cx, plain_year_month_result, NAME)?;

        Ok(
            PlainYearMonthObject::new_from_constructor(cx, new_target, plain_year_month)?
                .as_value(),
        )
    }

    /// Temporal.PlainYearMonth.from (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.from)
    pub fn from(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let item_arg = get_argument(cx, arguments, 0);
        let plain_year_month = to_temporal_year_month(cx, item_arg, "PlainYearMonth.from")?;

        Ok(PlainYearMonthObject::new(cx, plain_year_month)?.as_value())
    }

    /// Temporal.PlainYearMonth.compare (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.compare)
    pub fn compare(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainYearMonth.compare";

        let arg_1 = get_argument(cx, arguments, 0);
        let arg_2 = get_argument(cx, arguments, 1);

        let year_month_1 = to_temporal_year_month(cx, arg_1, NAME)?;
        let year_month_2 = to_temporal_year_month(cx, arg_2, NAME)?;

        Ok(cx.smi(year_month_1.compare_iso(&year_month_2) as i32))
    }
}

/// ToTemporalYearMonth (https://tc39.es/proposal-temporal/#sec-temporal-totemporalyearmonth)
pub fn to_temporal_year_month(
    cx: Context,
    item: Handle<Value>,
    method_name: &str,
) -> EvalResult<PlainYearMonth> {
    if item.is_object() {
        // Check if item is a Temporal object of some kind
        let item_object = item.as_object();
        if let Some(plain_year_month) = item_object.as_plain_year_month_object() {
            return Ok(plain_year_month.year_month().clone());
        }

        // Otherwise treat like a date-like object
        unimplemented!("ToTemporalYearMonth for date-like object")
    }

    // Otherwise parse PlainYearMonth from string
    if !item.is_string() {
        return type_error(cx, &format!("{method_name} year-month must be a string or object"));
    }

    let item_string = item.as_string().to_wtf8_string()?;

    let parsed_result = PlainYearMonth::from_utf8(item_string.as_bytes());
    let parsed = map_temporal_result(cx, parsed_result, method_name)?;

    Ok(parsed)
}
