use temporal_rs::options::DisplayCalendar;

use crate::runtime::{
    Context, EvalResult, Handle, Realm, Value,
    alloc_error::AllocResult,
    error::type_error,
    function::get_argument,
    intrinsics::{
        intrinsics::Intrinsic,
        rust_runtime::RuntimeFunction,
        temporal::{
            plain_month_day_constructor::to_temporal_month_day,
            plain_month_day_object::PlainMonthDayObject,
            utils::{get_show_calendar_name_option, validate_options_object},
        },
    },
    object_value::ObjectValue,
    property::Property,
};

pub struct PlainMonthDayPrototype;

impl PlainMonthDayPrototype {
    /// Properties of the Temporal.PlainMonthDay Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plainmonthday-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once PlainMonthDayConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
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

    /// get Temporal.PlainMonthDay.prototype.calendarId (https://tc39.es/proposal-temporal/#sec-get-temporal.plainmonthday.prototype.calendarid)
    pub fn calendar_id(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_month_day =
            this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.calendarId")?;
        let calendar_str = this_month_day.month_day().calendar_id();

        Ok(cx.alloc_static_string(calendar_str)?.as_value())
    }

    /// get Temporal.PlainMonthDay.prototype.monthCode (https://tc39.es/proposal-temporal/#sec-get-temporal.plainmonthday.prototype.monthcode)
    pub fn month_code(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_month_day =
            this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.monthCode")?;
        let month_code = this_month_day.month_day().month_code();

        Ok(cx.alloc_string(month_code.as_str())?.as_value())
    }

    /// get Temporal.PlainMonthDay.prototype.day (https://tc39.es/proposal-temporal/#sec-get-temporal.plainmonthday.prototype.day)
    pub fn day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_month_day = this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.day")?;
        let day = this_month_day.month_day().day();

        Ok(cx.smi(day as i32))
    }

    /// Temporal.PlainMonthDay.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.equals)
    pub fn equals(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainMonthDay.prototype.equals";

        let this_month_day = this_plain_month_day(cx, this_value, NAME)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other_month_day = to_temporal_month_day(cx, other_arg, NAME)?;

        Ok(cx.bool(this_month_day.month_day() == &other_month_day))
    }

    /// Temporal.PlainMonthDay.prototype.toPlainDate (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.toplaindate)
    pub fn to_plain_date(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.toPlainDate")?;
        unimplemented!("PlainMonthDay.prototype.toPlainDate")
    }

    /// Temporal.PlainMonthDay.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainMonthDay.prototype.toString";

        let this_month_day = this_plain_month_day(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let display_calendar = get_show_calendar_name_option(cx, options, NAME)?;

        let month_day_string = this_month_day.month_day().to_ixdtf_string(display_calendar);

        Ok(cx.alloc_string(&month_day_string)?.as_value())
    }

    /// Temporal.PlainMonthDay.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.tolocalestring)
    pub fn to_locale_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_month_day =
            this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.toLocaleString")?;
        let month_day_string = this_month_day
            .month_day()
            .to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&month_day_string)?.as_value())
    }

    /// Temporal.PlainMonthDay.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.tojson)
    pub fn to_json(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_month_day =
            this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.toJSON")?;
        let month_day_string = this_month_day
            .month_day()
            .to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&month_day_string)?.as_value())
    }

    /// Temporal.PlainMonthDay.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.valueof)
    pub fn value_of(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "PlainMonthDay.prototype.valueOf must not be called")
    }

    /// Temporal.PlainMonthDay.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.plainmonthday.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_month_day(cx, this_value, "PlainMonthDay.prototype.with")?;
        unimplemented!("PlainMonthDay.prototype.with")
    }
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
