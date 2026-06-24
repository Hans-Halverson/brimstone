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
            duration_constructor::to_temporal_duration,
            plain_date_constructor::to_temporal_date,
            plain_date_object::PlainDateObject,
            utils::{
                get_overflow_option, get_show_calendar_name_option, map_temporal_result,
                validate_options_object,
            },
        },
    },
    object_value::ObjectValue,
    property::Property,
};

pub struct PlainDatePrototype;

impl PlainDatePrototype {
    /// Properties of the Temporal.PlainDate Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindate-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once PlainDateConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.temporal_plain_date().as_string().into(), false, false, true),
        )?;

        // Getters
        object.intrinsic_getter(
            cx,
            cx.names.calendar_id(),
            RuntimeFunction::PlainDatePrototype_calendarId,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.era(),
            RuntimeFunction::PlainDatePrototype_era,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.era_year(),
            RuntimeFunction::PlainDatePrototype_eraYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.year(),
            RuntimeFunction::PlainDatePrototype_year,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month(),
            RuntimeFunction::PlainDatePrototype_month,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month_code(),
            RuntimeFunction::PlainDatePrototype_monthCode,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day(),
            RuntimeFunction::PlainDatePrototype_day,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day_of_week(),
            RuntimeFunction::PlainDatePrototype_dayOfWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day_of_year(),
            RuntimeFunction::PlainDatePrototype_dayOfYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.week_of_year(),
            RuntimeFunction::PlainDatePrototype_weekOfYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.year_of_week(),
            RuntimeFunction::PlainDatePrototype_yearOfWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_week(),
            RuntimeFunction::PlainDatePrototype_daysInWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_month(),
            RuntimeFunction::PlainDatePrototype_daysInMonth,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_year(),
            RuntimeFunction::PlainDatePrototype_daysInYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.months_in_year(),
            RuntimeFunction::PlainDatePrototype_monthsInYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.in_leap_year(),
            RuntimeFunction::PlainDatePrototype_inLeapYear,
            realm,
        )?;

        // Methods
        object.intrinsic_func(
            cx,
            cx.names.add(),
            RuntimeFunction::PlainDatePrototype_add,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.subtract(),
            RuntimeFunction::PlainDatePrototype_subtract,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.until(),
            RuntimeFunction::PlainDatePrototype_until,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.since(),
            RuntimeFunction::PlainDatePrototype_since,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.equals(),
            RuntimeFunction::PlainDatePrototype_equals,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_date_time(),
            RuntimeFunction::PlainDatePrototype_toPlainDateTime,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_month_day(),
            RuntimeFunction::PlainDatePrototype_toPlainMonthDay,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_year_month(),
            RuntimeFunction::PlainDatePrototype_toPlainYearMonth,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_zoned_date_time(),
            RuntimeFunction::PlainDatePrototype_toZonedDateTime,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::PlainDatePrototype_toString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            RuntimeFunction::PlainDatePrototype_toLocaleString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_json(),
            RuntimeFunction::PlainDatePrototype_toJSON,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::PlainDatePrototype_valueOf,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with(),
            RuntimeFunction::PlainDatePrototype_with,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with_calendar(),
            RuntimeFunction::PlainDatePrototype_withCalendar,
            1,
            realm,
        )?;

        Ok(object)
    }

    /// get Temporal.PlainDate.prototype.calendarId (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.calendarid)
    pub fn calendar_id(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.calendarId")?;
        let calendar_str = this_date.date().calendar().identifier();

        Ok(cx.alloc_static_string(calendar_str)?.as_value())
    }

    /// get Temporal.PlainDate.prototype.era (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.era)
    pub fn era(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.era")?;

        match this_date.date().era() {
            None => Ok(cx.undefined()),
            Some(era) => Ok(cx.alloc_string(era.as_str())?.as_value()),
        }
    }

    /// get Temporal.PlainDate.prototype.eraYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.erayear)
    pub fn era_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.eraYear")?;

        match this_date.date().era_year() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }

    /// get Temporal.PlainDate.prototype.year (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.year)
    pub fn year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.year")?;
        let year_number = this_date.date().year();

        Ok(cx.smi(year_number))
    }

    /// get Temporal.PlainDate.prototype.month (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.month)
    pub fn month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.month")?;
        let month_number = this_date.date().month();

        Ok(cx.smi(month_number as i32))
    }

    /// get Temporal.PlainDate.prototype.monthCode (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.monthcode)
    pub fn month_code(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.monthCode")?;
        let month_code = this_date.date().month_code();

        Ok(cx.alloc_string(month_code.as_str())?.as_value())
    }

    /// get Temporal.PlainDate.prototype.day (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.day)
    pub fn day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.day")?;
        let day_number = this_date.date().day();

        Ok(cx.smi(day_number as i32))
    }

    /// get Temporal.PlainDate.prototype.dayOfWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.dayofweek)
    pub fn day_of_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.dayOfWeek")?;
        let day_of_week_number = this_date.date().day_of_week();

        Ok(cx.smi(day_of_week_number as i32))
    }

    /// get Temporal.PlainDate.prototype.dayOfYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.dayofyear)
    pub fn day_of_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.dayOfYear")?;
        let day_of_year_number = this_date.date().day_of_year();

        Ok(cx.smi(day_of_year_number as i32))
    }

    /// get Temporal.PlainDate.prototype.weekOfYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.weekofyear)
    pub fn week_of_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.weekOfYear")?;

        match this_date.date().week_of_year() {
            None => Ok(cx.undefined()),
            Some(week_number) => Ok(cx.smi(week_number as i32)),
        }
    }

    /// get Temporal.PlainDate.prototype.yearOfWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.yearofweek)
    pub fn year_of_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.yearOfWeek")?;

        match this_date.date().year_of_week() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }

    /// get Temporal.PlainDate.prototype.daysInWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinweek)
    pub fn days_in_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.daysInWeek")?;
        let num_days_in_week = this_date.date().days_in_week();

        Ok(cx.smi(num_days_in_week as i32))
    }

    /// get Temporal.PlainDate.prototype.daysInMonth (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinmonth)
    pub fn days_in_month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.daysInMonth")?;
        let num_days_in_month = this_date.date().days_in_month();

        Ok(cx.smi(num_days_in_month as i32))
    }

    /// get Temporal.PlainDate.prototype.daysInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinyear)
    pub fn days_in_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.daysInYear")?;
        let num_days_in_year = this_date.date().days_in_year();

        Ok(cx.smi(num_days_in_year as i32))
    }

    /// get Temporal.PlainDate.prototype.monthsInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.monthsinyear)
    pub fn months_in_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.monthsInYear")?;
        let num_months_in_year = this_date.date().months_in_year();

        Ok(cx.smi(num_months_in_year as i32))
    }

    /// get Temporal.PlainDate.prototype.inLeapYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.inleapyear)
    pub fn in_leap_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.inLeapYear")?;
        let in_leap_year = this_date.date().in_leap_year();

        Ok(cx.bool(in_leap_year))
    }

    /// Temporal.PlainDate.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDate.prototype.add";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_date_result = this_date.date().add(&duration, Some(overflow));
        let new_date = map_temporal_result(cx, new_date_result, NAME)?;

        Ok(PlainDateObject::new(cx, new_date)?.as_value())
    }

    /// Temporal.PlainDate.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.subtract)
    pub fn subtract(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDate.prototype.subtract";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_date_result = this_date.date().subtract(&duration, Some(overflow));
        let new_date = map_temporal_result(cx, new_date_result, NAME)?;

        Ok(PlainDateObject::new(cx, new_date)?.as_value())
    }

    /// Temporal.PlainDate.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.until)
    pub fn until(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_date(cx, this_value, "PlainDate.prototype.until")?;
        unimplemented!("PlainDate.prototype.until")
    }

    /// Temporal.PlainDate.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.since)
    pub fn since(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_date(cx, this_value, "PlainDate.prototype.since")?;
        unimplemented!("PlainDate.prototype.since")
    }

    /// Temporal.PlainDate.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.equals)
    pub fn equals(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDate.prototype.equals";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other_date = to_temporal_date(cx, other_arg, NAME)?;

        Ok(cx.bool(this_date.date() == &other_date))
    }

    /// Temporal.PlainDate.prototype.toPlainDateTime (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.toplaindatetime)
    pub fn to_plain_date_time(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_date(cx, this_value, "PlainDate.prototype.toPlainDateTime")?;
        unimplemented!("PlainDate.prototype.toPlainDateTime")
    }

    /// Temporal.PlainDate.prototype.toPlainMonthDay (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.toplainmonthday)
    pub fn to_plain_month_day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_date(cx, this_value, "PlainDate.prototype.toPlainMonthDay")?;
        unimplemented!("PlainDate.prototype.toPlainMonthDay")
    }

    /// Temporal.PlainDate.prototype.toPlainYearMonth (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.toplainyearmonth)
    pub fn to_plain_year_month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_date(cx, this_value, "PlainDate.prototype.toPlainYearMonth")?;
        unimplemented!("PlainDate.prototype.toPlainYearMonth")
    }

    /// Temporal.PlainDate.prototype.toZonedDateTime (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tozoneddatetime)
    pub fn to_zoned_date_time(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_date(cx, this_value, "PlainDate.prototype.toZonedDateTime")?;
        unimplemented!("PlainDate.prototype.toZonedDateTime")
    }

    /// Temporal.PlainDate.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDate.prototype.toString";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        // Parse calendar format from options
        let options_arg = get_argument(cx, arguments, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let display_name = get_show_calendar_name_option(cx, options, NAME)?;

        let date_string = this_date.date().to_ixdtf_string(display_name);

        Ok(cx.alloc_string(&date_string)?.as_value())
    }

    /// Temporal.PlainDate.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tolocalestring)
    pub fn to_locale_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.toLocaleString")?;
        let date_string = this_date.date().to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&date_string)?.as_value())
    }

    /// Temporal.PlainDate.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tojson)
    pub fn to_json(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.toJSON")?;
        let date_string = this_date.date().to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&date_string)?.as_value())
    }

    /// Temporal.PlainDate.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.valueof)
    pub fn value_of(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "PlainDate.prototype.valueOf must not be called")
    }

    /// Temporal.PlainDate.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_date(cx, this_value, "PlainDate.prototype.with")?;
        unimplemented!("PlainDate.prototype.with")
    }

    /// Temporal.PlainDate.prototype.withCalendar (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.withcalendar)
    pub fn with_calendar(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_date(cx, this_value, "PlainDate.prototype.withCalendar")?;
        unimplemented!("PlainDate.prototype.withCalendar")
    }
}

fn this_plain_date(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<PlainDateObject>> {
    if value.is_object() {
        if let Some(plain_date) = value.as_object().as_plain_date_object() {
            return Ok(plain_date);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a PlainDate"))
}
