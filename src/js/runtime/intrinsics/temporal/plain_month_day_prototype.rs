use temporal_rs::options::DisplayCalendar;

use crate::{
    runtime::{
        Context, EvalResult, Handle, Realm, Value,
        alloc_error::AllocResult,
        error::type_error,
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            temporal::{
                plain_date_object::PlainDateObject,
                plain_month_day_constructor::to_temporal_month_day,
                plain_month_day_object::PlainMonthDayObject,
                utils::{
                    DateField, RequiredFieldNames, get_overflow_option,
                    get_show_calendar_name_option, is_partial_temporal_object, map_temporal_result,
                    prepare_calendar_fields, validate_options_object,
                },
            },
        },
        object_value::ObjectValue,
        property::Property,
    },
    runtime_fn,
};

pub struct PlainMonthDayPrototype;

impl PlainMonthDayPrototype {
    /// Properties of the Temporal.PlainMonthDay Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plainmonthday-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once PlainMonthDayConstructor has been created

        let to_string_tag_key = cx.symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(
                cx.names.temporal_plain_month_day().as_string().into(),
                false,
                false,
                true,
            ),
        )?;

        // Getters
        object.intrinsic_getter(
            cx,
            cx.names.calendar_id(),
            RuntimeFunction::PlainMonthDayPrototype_calendarId,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month_code(),
            RuntimeFunction::PlainMonthDayPrototype_monthCode,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day(),
            RuntimeFunction::PlainMonthDayPrototype_day,
            realm,
        )?;

        // Methods
        object.intrinsic_func(
            cx,
            cx.names.equals(),
            RuntimeFunction::PlainMonthDayPrototype_equals,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_date(),
            RuntimeFunction::PlainMonthDayPrototype_toPlainDate,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::PlainMonthDayPrototype_toString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            RuntimeFunction::PlainMonthDayPrototype_toLocaleString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_json(),
            RuntimeFunction::PlainMonthDayPrototype_toJSON,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::PlainMonthDayPrototype_valueOf,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with(),
            RuntimeFunction::PlainMonthDayPrototype_with,
            1,
            realm,
        )?;

        Ok(object)
    }

    runtime_fn! {
    /// get Temporal.PlainMonthDay.prototype.calendarId (https://tc39.es/proposal-temporal/#sec-get-temporal.plainmonthday.prototype.calendarid)
    fn calendar_id(cx, this_value, _) {
        let this_month_day =
            this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.calendarId")?;
        let calendar_str = this_month_day.month_day().calendar_id();

        Ok(cx.alloc_static_string(calendar_str)?.as_value())
    }}

    runtime_fn! {
    /// get Temporal.PlainMonthDay.prototype.monthCode (https://tc39.es/proposal-temporal/#sec-get-temporal.plainmonthday.prototype.monthcode)
    fn month_code(cx, this_value, _) {
        let this_month_day =
            this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.monthCode")?;
        let month_code = this_month_day.month_day().month_code();

        Ok(cx.alloc_string(month_code.as_str())?.as_value())
    }}

    runtime_fn! {
    /// get Temporal.PlainMonthDay.prototype.day (https://tc39.es/proposal-temporal/#sec-get-temporal.plainmonthday.prototype.day)
    fn day(cx, this_value, _) {
        let this_month_day = this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.day")?;
        let day = this_month_day.month_day().day();

        Ok(cx.smi(day))
    }}

    runtime_fn! {
    /// Temporal.PlainMonthDay.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.equals)
    fn equals(cx, this_value, arguments) {
        const NAME: &str = "PlainMonthDay.prototype.equals";

        let this_month_day = this_plain_month_day(cx, this_value, NAME)?;

        let other_arg = arguments.get(cx, 0);
        let other_month_day = to_temporal_month_day(cx, other_arg, None, NAME)?;

        Ok(cx.bool(this_month_day.month_day() == &other_month_day))
    }}

    runtime_fn! {
    /// Temporal.PlainMonthDay.prototype.toPlainDate (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.toplaindate)
    fn to_plain_date(cx, this_value, arguments) {
        const NAME: &str = "PlainMonthDay.prototype.toPlainDate";

        let this_month_day = this_plain_month_day(cx, this_value, NAME)?;

        let item_arg = arguments.get(cx, 0);
        if !item_arg.is_object() {
            return type_error(
                cx,
                "PlainMonthDay.prototype.toPlainDate argument must be a date-like object",
            );
        }

        let prepared_fields = prepare_calendar_fields(
            cx,
            item_arg.as_object(),
            &[DateField::Year],
            &[],
            RequiredFieldNames::Partial,
            NAME,
        )?;

        let fields = prepared_fields.into_validated_date_fields();

        let plain_date_result = this_month_day.month_day().to_plain_date(Some(fields));
        let plain_date = map_temporal_result(cx, plain_date_result, NAME)?;

        Ok(PlainDateObject::new(cx, plain_date)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainMonthDay.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.tostring)
    fn to_string(cx, this_value, arguments) {
        const NAME: &str = "PlainMonthDay.prototype.toString";

        let this_month_day = this_plain_month_day(cx, this_value, NAME)?;

        let options_arg = arguments.get(cx, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let display_calendar = get_show_calendar_name_option(cx, options, NAME)?;

        let month_day_string = this_month_day.month_day().to_ixdtf_string(display_calendar);

        Ok(cx.alloc_string(&month_day_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainMonthDay.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.tolocalestring)
    fn to_locale_string(cx, this_value, _) {
        let this_month_day =
            this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.toLocaleString")?;
        let month_day_string = this_month_day
            .month_day()
            .to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&month_day_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainMonthDay.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.tojson)
    fn to_json(cx, this_value, _) {
        let this_month_day =
            this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.toJSON")?;
        let month_day_string = this_month_day
            .month_day()
            .to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&month_day_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainMonthDay.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.valueof)
    fn value_of(cx, _, _) {
        type_error(cx, "PlainMonthDay.prototype.valueOf must not be called")
    }}

    runtime_fn! {
    /// Temporal.PlainMonthDay.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.with)
    fn with(cx, this_value, arguments) {
        const NAME: &str = "PlainMonthDay.prototype.with";

        let this_month_day = this_plain_month_day(cx, this_value, NAME)?;

        let date_like_arg = arguments.get(cx, 0);
        if !is_partial_temporal_object(cx, date_like_arg)? {
            return type_error(
                cx,
                "PlainMonthDay.prototype.with argument must be a date-like object",
            );
        }

        let date_like_object = date_like_arg.as_object();

        let prepared_fields = prepare_calendar_fields(
            cx,
            date_like_object,
            &[
                DateField::Year,
                DateField::Month,
                DateField::MonthCode,
                DateField::Day,
            ],
            &[],
            RequiredFieldNames::Partial,
            NAME,
        )?;

        let options_arg = arguments.get(cx, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let fields = prepared_fields.into_validated_date_fields();

        let new_month_day_result = this_month_day.month_day().with(fields, Some(overflow));
        let new_month_day = map_temporal_result(cx, new_month_day_result, NAME)?;

        Ok(PlainMonthDayObject::new(cx, new_month_day)?.as_value())
    }}
}

fn this_plain_month_day(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<PlainMonthDayObject>> {
    if value.is_object() {
        if let Some(plain_month_day) = value.as_object().as_plain_month_day_object() {
            return Ok(plain_month_day);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a PlainMonthDay"))
}
