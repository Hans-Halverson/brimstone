use temporal_rs::{PlainYearMonth, parsed_intermediates::ParsedDate, partial::PartialYearMonth};

use crate::{
    runtime::{
        Context, Handle, Realm, Value,
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        error::type_error,
        eval_result::EvalResult,
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            temporal::{
                plain_year_month_object::PlainYearMonthObject,
                utils::{
                    DateField, RequiredFieldNames, get_calendar_identifier_with_iso_default,
                    get_overflow_option, map_temporal_result, parse_calendar_argument,
                    prepare_calendar_fields, to_integer_with_truncation, validate_options_object,
                },
            },
        },
        object_value::ObjectValue,
    },
    runtime_fn,
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

    runtime_fn! {
    /// Temporal.PlainYearMonth (https://tc39.es/proposal-temporal/#sec-temporal-plainyearmonth)
    fn construct(cx, _, arguments) {
        const NAME: &str = "Temporal.PlainYearMonth constructor";

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.PlainYearMonth constructor must be called with new");
        };

        let year_arg = arguments.get(cx, 0);
        let month_arg = arguments.get(cx, 1);

        // Truncation for year, month, and day will trigger an error later in creation
        let year = to_integer_with_truncation(cx, year_arg, NAME)?;
        let month = to_integer_with_truncation(cx, month_arg, NAME)?;

        let calendar_arg = arguments.get(cx, 2);
        let calendar = parse_calendar_argument(cx, calendar_arg, NAME)?;

        let reference_iso_day_arg = arguments.get(cx, 3);
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
    }}

    runtime_fn! {
    /// Temporal.PlainYearMonth.from (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.from)
    fn from(cx, _, arguments) {
        let item_arg = arguments.get(cx, 0);
        let options_arg = arguments.get(cx, 1);

        let plain_year_month =
            to_temporal_year_month(cx, item_arg, Some(options_arg), "PlainYearMonth.from")?;

        Ok(PlainYearMonthObject::new(cx, plain_year_month)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainYearMonth.compare (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.compare)
    fn compare(cx, _, arguments) {
        const NAME: &str = "PlainYearMonth.compare";

        let arg_1 = arguments.get(cx, 0);
        let arg_2 = arguments.get(cx, 1);

        let year_month_1 = to_temporal_year_month(cx, arg_1, None, NAME)?;
        let year_month_2 = to_temporal_year_month(cx, arg_2, None, NAME)?;

        Ok(cx.smi(year_month_1.compare_iso(&year_month_2) as i8))
    }}
}

/// ToTemporalYearMonth (https://tc39.es/proposal-temporal/#sec-temporal-totemporalyearmonth)
pub fn to_temporal_year_month(
    cx: Context,
    item: Handle<Value>,
    options_arg: Option<Handle<Value>>,
    method_name: &str,
) -> EvalResult<PlainYearMonth> {
    let options_arg = options_arg.unwrap_or_else(|| cx.undefined());

    if item.is_object() {
        // Check if item is a Temporal object of some kind
        let item_object = item.as_object();
        if let Some(plain_year_month) = item_object.as_plain_year_month_object() {
            let options = validate_options_object(cx, options_arg, method_name)?;
            get_overflow_option(cx, options, method_name)?;

            return Ok(plain_year_month.year_month().clone());
        }

        // Otherwise treat like a date-like object
        let calendar = get_calendar_identifier_with_iso_default(cx, item_object, method_name)?;

        let prepared_fields = prepare_calendar_fields(
            cx,
            item_object,
            &[DateField::Year, DateField::Month, DateField::MonthCode],
            &[],
            RequiredFieldNames::Defaults,
            method_name,
        )?;

        let options = validate_options_object(cx, options_arg, method_name)?;
        let overflow = get_overflow_option(cx, options, method_name)?;

        let calendar_fields = prepared_fields.into_validated_date_fields().into();

        let partial_date = PartialYearMonth { calendar, calendar_fields };
        let year_month_result = PlainYearMonth::from_partial(partial_date, Some(overflow));

        return map_temporal_result(cx, year_month_result, method_name);
    }

    // Otherwise parse PlainYearMonth from string
    if !item.is_string() {
        return type_error(cx, &format!("{method_name} year-month must be a string or object"));
    }

    let item_string = item.as_string().to_wtf8_string()?;
    let parsed_result = ParsedDate::year_month_from_utf8(item_string.as_bytes());
    let parsed = map_temporal_result(cx, parsed_result, method_name)?;

    let options = validate_options_object(cx, options_arg, method_name)?;
    get_overflow_option(cx, options, method_name)?;

    let year_month_result = PlainYearMonth::from_parsed(parsed);
    let year_month = map_temporal_result(cx, year_month_result, method_name)?;

    Ok(year_month)
}
