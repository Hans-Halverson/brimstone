use temporal_rs::{
    PlainDate, options::Overflow, parsed_intermediates::ParsedDate, partial::PartialDate,
};

use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, Realm, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            temporal::{
                plain_date_object::PlainDateObject,
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

pub struct PlainDateConstructor;

impl PlainDateConstructor {
    /// Temporal.PlainDate Constructor (https://tc39.es/proposal-temporal/#sec-temporal-plaindate-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::PlainDateConstructor_construct,
            3,
            cx.names.plain_date(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::PlainDatePrototype)?;

        intrinsic_methods!(cx, builder, {
            compare PlainDateConstructor_compare (2),
            from    PlainDateConstructor_from    (1),
        });

        builder.build()
    }

    runtime_fn! {
    /// Temporal.PlainDate (https://tc39.es/proposal-temporal/#sec-temporal-plaindate)
    fn construct(cx, _, arguments) {
        const NAME: &str = "Temporal.PlainDate constructor";

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.PlainDate constructor must be called with new");
        };

        let year_arg = arguments.get(cx, 0);
        let month_arg = arguments.get(cx, 1);
        let day_arg = arguments.get(cx, 2);

        // Truncation for year, month, and day will trigger an error later in creation
        let year = to_integer_with_truncation(cx, year_arg, NAME)?;
        let month = to_integer_with_truncation(cx, month_arg, NAME)?;
        let day = to_integer_with_truncation(cx, day_arg, NAME)?;

        // Validate calendar argument
        let calendar_arg = arguments.get(cx, 3);
        let calendar = parse_calendar_argument(cx, calendar_arg, NAME)?;

        // Clamp year, month, and day into range for `temporal_rs`
        let plain_date_result = PlainDate::try_new(year, month, day, calendar);
        let plain_date = map_temporal_result(cx, plain_date_result, NAME)?;

        Ok(PlainDateObject::new_from_constructor(cx, new_target, plain_date)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.compare (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.compare)
    fn compare(cx, _, arguments) {
        const NAME: &str = "PlainDate.compare";

        let arg_1 = arguments.get(cx, 0);
        let arg_2 = arguments.get(cx, 1);

        let date_1 = to_temporal_date(cx, arg_1, NAME)?;
        let date_2 = to_temporal_date(cx, arg_2, NAME)?;

        Ok(cx.smi(date_1.compare_iso(&date_2) as i8))
    }}

    runtime_fn! {
    /// Temporal.PlainDate.from (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.from)
    fn from(cx, _, arguments) {
        let item_arg = arguments.get(cx, 0);
        let options_arg = arguments.get(cx, 1);

        let plain_date =
            to_temporal_date_with_options(cx, item_arg, options_arg, "PlainDate.from")?;

        Ok(PlainDateObject::new(cx, plain_date)?.as_value())
    }}
}

/// ToTemporalDate (https://tc39.es/proposal-temporal/#sec-temporal-totemporaldate)
pub fn to_temporal_date(
    cx: Context,
    item: Handle<Value>,
    method_name: &str,
) -> EvalResult<PlainDate> {
    to_temporal_date_with_options(cx, item, cx.undefined(), method_name)
}

pub fn to_temporal_date_with_options(
    cx: Context,
    item: Handle<Value>,
    options: Handle<Value>,
    method_name: &str,
) -> EvalResult<PlainDate> {
    fn validate_overflow_option(
        cx: Context,
        options: Handle<Value>,
        method_name: &str,
    ) -> EvalResult<Overflow> {
        let options = validate_options_object(cx, options, method_name)?;
        get_overflow_option(cx, options, method_name)
    }

    if item.is_object() {
        // Check if item is a Temporal object of some kind
        let item_object = item.as_object();
        if let Some(plain_date) = item_object.as_plain_date_object() {
            validate_overflow_option(cx, options, method_name)?;
            return Ok(plain_date.date().clone());
        } else if let Some(zoned_date_time) = item_object.as_zoned_date_time_object() {
            validate_overflow_option(cx, options, method_name)?;
            return Ok(zoned_date_time.zoned_date_time().to_plain_date());
        } else if let Some(plain_date_time) = item_object.as_plain_date_time_object() {
            validate_overflow_option(cx, options, method_name)?;
            return Ok(plain_date_time.date_time().to_plain_date());
        }

        // Otherwise treat as a date-like object
        let calendar = get_calendar_identifier_with_iso_default(cx, item_object, method_name)?;

        let prepared_fields = prepare_calendar_fields(
            cx,
            item_object,
            &[
                DateField::Year,
                DateField::Month,
                DateField::MonthCode,
                DateField::Day,
            ],
            &[],
            RequiredFieldNames::Defaults,
            method_name,
        )?;

        let overflow = validate_overflow_option(cx, options, method_name)?;

        let calendar_fields = prepared_fields.into_validated_date_fields();

        let partial_date = PartialDate { calendar, calendar_fields };
        let new_date = PlainDate::from_partial(partial_date, Some(overflow));

        return map_temporal_result(cx, new_date, method_name);
    }

    // Otherwise parse PlainDate from string
    if !item.is_string() {
        return type_error(cx, &format!("{method_name} date must be a string or object"));
    }

    let item_string = item.as_string().to_wtf8_string()?;
    let parsed_result = ParsedDate::from_utf8(item_string.as_bytes());
    let parsed = map_temporal_result(cx, parsed_result, method_name)?;

    validate_overflow_option(cx, options, method_name)?;

    let date_result = PlainDate::from_parsed(parsed);
    let date = map_temporal_result(cx, date_result, method_name)?;

    Ok(date)
}
