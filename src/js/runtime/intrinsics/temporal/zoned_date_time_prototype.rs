use num_bigint::BigInt;
use temporal_rs::options::{
    DisplayCalendar, DisplayOffset, DisplayTimeZone, RoundingMode, RoundingOptions,
    ToStringRoundingOptions,
};

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
            duration_object::DurationObject,
            instant_object::InstantObject,
            plain_date_object::PlainDateObject,
            plain_date_time_object::PlainDateTimeObject,
            plain_time_constructor::to_temporal_time,
            plain_time_object::PlainTimeObject,
            utils::{
                DiffOperation, get_difference_settings, get_fractional_second_digits_option,
                get_overflow_option, get_rounding_increment_option, get_rounding_mode_option,
                get_show_calendar_name_option, get_show_offset_option,
                get_show_time_zone_name_option, get_unit_valued_option, map_temporal_result,
                parse_round_options_argument, to_temporal_calendar_identifier,
                to_time_zone_identifier, validate_options_object,
            },
            zoned_date_time_constructor::to_temporal_zoned_date_time,
            zoned_date_time_object::ZonedDateTimeObject,
        },
    },
    object_value::ObjectValue,
    property::Property,
    value::BigIntValue,
};

pub struct ZonedDateTimePrototype;

impl ZonedDateTimePrototype {
    /// Properties of the Temporal.ZonedDateTime Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-zoneddatetime-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once ZonedDateTimeConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(
                cx.names.temporal_zoned_date_time().as_string().into(),
                false,
                false,
                true,
            ),
        )?;

        // Getters
        object.intrinsic_getter(
            cx,
            cx.names.calendar_id(),
            RuntimeFunction::ZonedDateTimePrototype_calendarId,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.time_zone_id(),
            RuntimeFunction::ZonedDateTimePrototype_timeZoneId,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.epoch_nanoseconds(),
            RuntimeFunction::ZonedDateTimePrototype_epochNanoseconds,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.era(),
            RuntimeFunction::ZonedDateTimePrototype_era,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.era_year(),
            RuntimeFunction::ZonedDateTimePrototype_eraYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.year(),
            RuntimeFunction::ZonedDateTimePrototype_year,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month(),
            RuntimeFunction::ZonedDateTimePrototype_month,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month_code(),
            RuntimeFunction::ZonedDateTimePrototype_monthCode,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day(),
            RuntimeFunction::ZonedDateTimePrototype_day,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.hour(),
            RuntimeFunction::ZonedDateTimePrototype_hour,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.minute(),
            RuntimeFunction::ZonedDateTimePrototype_minute,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.second(),
            RuntimeFunction::ZonedDateTimePrototype_second,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.millisecond(),
            RuntimeFunction::ZonedDateTimePrototype_millisecond,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.microsecond(),
            RuntimeFunction::ZonedDateTimePrototype_microsecond,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.nanosecond(),
            RuntimeFunction::ZonedDateTimePrototype_nanosecond,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.offset(),
            RuntimeFunction::ZonedDateTimePrototype_offset,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.offset_nanoseconds(),
            RuntimeFunction::ZonedDateTimePrototype_offsetNanoseconds,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day_of_week(),
            RuntimeFunction::ZonedDateTimePrototype_dayOfWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day_of_year(),
            RuntimeFunction::ZonedDateTimePrototype_dayOfYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.week_of_year(),
            RuntimeFunction::ZonedDateTimePrototype_weekOfYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.year_of_week(),
            RuntimeFunction::ZonedDateTimePrototype_yearOfWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_week(),
            RuntimeFunction::ZonedDateTimePrototype_daysInWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_month(),
            RuntimeFunction::ZonedDateTimePrototype_daysInMonth,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_year(),
            RuntimeFunction::ZonedDateTimePrototype_daysInYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.months_in_year(),
            RuntimeFunction::ZonedDateTimePrototype_monthsInYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.in_leap_year(),
            RuntimeFunction::ZonedDateTimePrototype_inLeapYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.hours_in_day(),
            RuntimeFunction::ZonedDateTimePrototype_hoursInDay,
            realm,
        )?;

        // Methods
        object.intrinsic_func(
            cx,
            cx.names.add(),
            RuntimeFunction::ZonedDateTimePrototype_add,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.subtract(),
            RuntimeFunction::ZonedDateTimePrototype_subtract,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.until(),
            RuntimeFunction::ZonedDateTimePrototype_until,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.since(),
            RuntimeFunction::ZonedDateTimePrototype_since,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.round(),
            RuntimeFunction::ZonedDateTimePrototype_round,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.equals(),
            RuntimeFunction::ZonedDateTimePrototype_equals,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_instant(),
            RuntimeFunction::ZonedDateTimePrototype_toInstant,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_date(),
            RuntimeFunction::ZonedDateTimePrototype_toPlainDate,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_time(),
            RuntimeFunction::ZonedDateTimePrototype_toPlainTime,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_date_time(),
            RuntimeFunction::ZonedDateTimePrototype_toPlainDateTime,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::ZonedDateTimePrototype_toString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            RuntimeFunction::ZonedDateTimePrototype_toLocaleString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_json(),
            RuntimeFunction::ZonedDateTimePrototype_toJSON,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::ZonedDateTimePrototype_valueOf,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with(),
            RuntimeFunction::ZonedDateTimePrototype_with,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with_plain_time(),
            RuntimeFunction::ZonedDateTimePrototype_withPlainTime,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with_time_zone(),
            RuntimeFunction::ZonedDateTimePrototype_withTimeZone,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with_calendar(),
            RuntimeFunction::ZonedDateTimePrototype_withCalendar,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.start_of_day(),
            RuntimeFunction::ZonedDateTimePrototype_startOfDay,
            0,
            realm,
        )?;

        Ok(object)
    }

    /// get Temporal.ZonedDateTime.prototype.calendarId (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.calendarid)
    pub fn calendar_id(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.calendarId")?;
        let calendar_str = this_zoned_date_time
            .zoned_date_time()
            .calendar()
            .identifier();

        Ok(cx.alloc_static_string(calendar_str)?.as_value())
    }

    /// get Temporal.ZonedDateTime.prototype.timeZoneId (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.timezoneid)
    pub fn time_zone_id(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.timeZoneId";
        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;
        let time_zone_id_result = this_zoned_date_time
            .zoned_date_time()
            .time_zone()
            .identifier();
        let time_zone_id = map_temporal_result(cx, time_zone_id_result, NAME)?;

        Ok(cx.alloc_string(&time_zone_id)?.as_value())
    }

    /// get Temporal.ZonedDateTime.prototype.epochNanoseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.epochnanoseconds)
    pub fn epoch_nanoseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.epochNanoseconds")?;
        let epoch_nanos = this_zoned_date_time
            .zoned_date_time()
            .epoch_nanoseconds()
            .as_i128();

        Ok(BigIntValue::new(cx, BigInt::from(epoch_nanos))?.into())
    }

    /// get Temporal.ZonedDateTime.prototype.era (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.era)
    pub fn era(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.era")?;

        match this_zoned_date_time.zoned_date_time().era() {
            None => Ok(cx.undefined()),
            Some(era) => Ok(cx.alloc_string(era.as_str())?.as_value()),
        }
    }

    /// get Temporal.ZonedDateTime.prototype.eraYear (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.erayear)
    pub fn era_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.eraYear")?;

        match this_zoned_date_time.zoned_date_time().era_year() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }

    /// get Temporal.ZonedDateTime.prototype.year (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.year)
    pub fn year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.year")?;
        let year = this_zoned_date_time.zoned_date_time().year();

        Ok(cx.smi(year))
    }

    /// get Temporal.ZonedDateTime.prototype.month (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.month)
    pub fn month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.month")?;
        let month = this_zoned_date_time.zoned_date_time().month();

        Ok(cx.smi(month as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.monthCode (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.monthcode)
    pub fn month_code(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.monthCode")?;
        let month_code = this_zoned_date_time.zoned_date_time().month_code();

        Ok(cx.alloc_string(month_code.as_str())?.as_value())
    }

    /// get Temporal.ZonedDateTime.prototype.day (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.day)
    pub fn day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.day")?;
        let day = this_zoned_date_time.zoned_date_time().day();

        Ok(cx.smi(day as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.hour (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.hour)
    pub fn hour(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.hour")?;
        let hour = this_zoned_date_time.zoned_date_time().hour();

        Ok(cx.smi(hour as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.minute (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.minute)
    pub fn minute(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.minute")?;
        let minute = this_zoned_date_time.zoned_date_time().minute();

        Ok(cx.smi(minute as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.second (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.second)
    pub fn second(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.second")?;
        let second = this_zoned_date_time.zoned_date_time().second();

        Ok(cx.smi(second as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.millisecond (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.millisecond)
    pub fn millisecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.millisecond")?;
        let millis = this_zoned_date_time.zoned_date_time().millisecond();

        Ok(cx.smi(millis as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.microsecond (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.microsecond)
    pub fn microsecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.microsecond")?;
        let micros = this_zoned_date_time.zoned_date_time().microsecond();

        Ok(cx.smi(micros as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.nanosecond (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.nanosecond)
    pub fn nanosecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.nanosecond")?;
        let nanos = this_zoned_date_time.zoned_date_time().nanosecond();

        Ok(cx.smi(nanos as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.offset (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.offset)
    pub fn offset(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.offset")?;
        let offset_str = this_zoned_date_time.zoned_date_time().offset();

        Ok(cx.alloc_string(&offset_str)?.as_value())
    }

    /// get Temporal.ZonedDateTime.prototype.offsetNanoseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.offsetnanoseconds)
    pub fn offset_nanoseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.offsetNanoseconds")?;
        let offset_nanos = this_zoned_date_time.zoned_date_time().offset_nanoseconds();

        Ok(Value::from(offset_nanos).to_handle(cx))
    }

    /// get Temporal.ZonedDateTime.prototype.dayOfWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.dayofweek)
    pub fn day_of_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.dayOfWeek")?;
        let day_of_week = this_zoned_date_time.zoned_date_time().day_of_week();

        Ok(cx.smi(day_of_week as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.dayOfYear (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.dayofyear)
    pub fn day_of_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.dayOfYear")?;
        let day_of_year = this_zoned_date_time.zoned_date_time().day_of_year();

        Ok(cx.smi(day_of_year as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.weekOfYear (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.weekofyear)
    pub fn week_of_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.weekOfYear")?;

        match this_zoned_date_time.zoned_date_time().week_of_year() {
            None => Ok(cx.undefined()),
            Some(week_number) => Ok(cx.smi(week_number as i32)),
        }
    }

    /// get Temporal.ZonedDateTime.prototype.yearOfWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.yearofweek)
    pub fn year_of_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.yearOfWeek")?;

        match this_zoned_date_time.zoned_date_time().year_of_week() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }

    /// get Temporal.ZonedDateTime.prototype.daysInWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.daysinweek)
    pub fn days_in_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.daysInWeek")?;
        let days_in_week = this_zoned_date_time.zoned_date_time().days_in_week();

        Ok(cx.smi(days_in_week as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.daysInMonth (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.daysinmonth)
    pub fn days_in_month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.daysInMonth")?;
        let days_in_month = this_zoned_date_time.zoned_date_time().days_in_month();

        Ok(cx.smi(days_in_month as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.daysInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.daysinyear)
    pub fn days_in_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.daysInYear")?;
        let days_in_year = this_zoned_date_time.zoned_date_time().days_in_year();

        Ok(cx.smi(days_in_year as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.monthsInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.monthsinyear)
    pub fn months_in_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.monthsInYear")?;
        let months_in_year = this_zoned_date_time.zoned_date_time().months_in_year();

        Ok(cx.smi(months_in_year as i32))
    }

    /// get Temporal.ZonedDateTime.prototype.inLeapYear (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.inleapyear)
    pub fn in_leap_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.inLeapYear")?;
        let in_leap_year = this_zoned_date_time.zoned_date_time().in_leap_year();

        Ok(cx.bool(in_leap_year))
    }

    /// get Temporal.ZonedDateTime.prototype.hoursInDay (https://tc39.es/proposal-temporal/#sec-get-temporal.zoneddatetime.prototype.hoursinday)
    pub fn hours_in_day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.hoursInDay";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let hours_in_day_result = this_zoned_date_time.zoned_date_time().hours_in_day();
        let hours_in_day = map_temporal_result(cx, hours_in_day_result, NAME)?;

        Ok(cx.number(hours_in_day))
    }

    /// Temporal.ZonedDateTime.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.add";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_zoned_date_time_result = this_zoned_date_time
            .zoned_date_time()
            .add(&duration, Some(overflow));
        let new_zoned_date_time = map_temporal_result(cx, new_zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, new_zoned_date_time)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.subtract)
    pub fn subtract(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.subtract";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_zoned_date_time_result = this_zoned_date_time
            .zoned_date_time()
            .subtract(&duration, Some(overflow));
        let new_zoned_date_time = map_temporal_result(cx, new_zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, new_zoned_date_time)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.until)
    pub fn until(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        Self::diff(cx, this_value, arguments, DiffOperation::Until, "ZonedDateTime.prototype.until")
    }

    /// Temporal.ZonedDateTime.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.since)
    pub fn since(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        Self::diff(cx, this_value, arguments, DiffOperation::Since, "ZonedDateTime.prototype.since")
    }

    fn diff(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        operation: DiffOperation,
        method_name: &str,
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time = this_zoned_date_time(cx, this_value, method_name)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other = to_temporal_zoned_date_time(cx, other_arg, method_name)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, method_name)?;
        let difference_settings = get_difference_settings(cx, options, method_name)?;

        let duration_result = match operation {
            DiffOperation::Until => this_zoned_date_time
                .zoned_date_time()
                .until(&other, difference_settings),
            DiffOperation::Since => this_zoned_date_time
                .zoned_date_time()
                .since(&other, difference_settings),
        };

        let duration = map_temporal_result(cx, duration_result, method_name)?;

        Ok(DurationObject::new(cx, duration)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.round (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.round)
    pub fn round(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.round";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
        let options = parse_round_options_argument(cx, options_arg, NAME)?;

        // Parse rounding options from options object
        let increment = get_rounding_increment_option(cx, options, NAME)?;
        let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::HalfExpand, NAME)?;
        let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;

        let mut rounding_options = RoundingOptions::default();
        rounding_options.increment = Some(increment);
        rounding_options.rounding_mode = Some(rounding_mode);
        rounding_options.smallest_unit = smallest_unit;

        let rounded_result = this_zoned_date_time
            .zoned_date_time()
            .round(rounding_options);
        let rounded = map_temporal_result(cx, rounded_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, rounded)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.equals)
    pub fn equals(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.equals";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other_zoned_date_time = to_temporal_zoned_date_time(cx, other_arg, NAME)?;

        let is_equal_result = this_zoned_date_time
            .zoned_date_time()
            .equals(&other_zoned_date_time);
        let is_equal = map_temporal_result(cx, is_equal_result, NAME)?;

        Ok(cx.bool(is_equal))
    }

    /// Temporal.ZonedDateTime.prototype.toInstant (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.toinstant)
    pub fn to_instant(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.toInstant")?;
        let instant = this_zoned_date_time.zoned_date_time().to_instant();

        Ok(InstantObject::new(cx, instant)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.toPlainDate (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.toplaindate)
    pub fn to_plain_date(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.toPlainDate")?;
        let plain_date = this_zoned_date_time.zoned_date_time().to_plain_date();

        Ok(PlainDateObject::new(cx, plain_date)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.toPlainTime (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.toplaintime)
    pub fn to_plain_time(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.toPlainTime")?;
        let plain_time = this_zoned_date_time.zoned_date_time().to_plain_time();

        Ok(PlainTimeObject::new(cx, plain_time)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.toPlainDateTime (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.toplaindatetime)
    pub fn to_plain_date_time(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_zoned_date_time =
            this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.toPlainDateTime")?;
        let plain_date_time = this_zoned_date_time.zoned_date_time().to_plain_date_time();

        Ok(PlainDateTimeObject::new(cx, plain_date_time)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.toString";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;

        // Parse display and rounding options from options object
        let display_calendar = get_show_calendar_name_option(cx, options, NAME)?;
        let display_offset = get_show_offset_option(cx, options, NAME)?;
        let precision = get_fractional_second_digits_option(cx, options, NAME)?;
        let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::Trunc, NAME)?;
        let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;
        let display_time_zone = get_show_time_zone_name_option(cx, options, NAME)?;

        let to_string_options = ToStringRoundingOptions {
            precision,
            smallest_unit,
            rounding_mode: Some(rounding_mode),
        };

        let zoned_date_time_string_result = this_zoned_date_time.zoned_date_time().to_ixdtf_string(
            display_offset,
            display_time_zone,
            display_calendar,
            to_string_options,
        );
        let zoned_date_time_string = map_temporal_result(cx, zoned_date_time_string_result, NAME)?;

        Ok(cx.alloc_string(&zoned_date_time_string)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.tolocalestring)
    pub fn to_locale_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.toLocaleString";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let zoned_date_time_string_result = this_zoned_date_time.zoned_date_time().to_ixdtf_string(
            DisplayOffset::default(),
            DisplayTimeZone::default(),
            DisplayCalendar::default(),
            ToStringRoundingOptions::default(),
        );
        let zoned_date_time_string = map_temporal_result(cx, zoned_date_time_string_result, NAME)?;

        Ok(cx.alloc_string(&zoned_date_time_string)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.tojson)
    pub fn to_json(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.toJSON";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let zoned_date_time_string_result = this_zoned_date_time.zoned_date_time().to_ixdtf_string(
            DisplayOffset::default(),
            DisplayTimeZone::default(),
            DisplayCalendar::default(),
            ToStringRoundingOptions::default(),
        );
        let zoned_date_time_string = map_temporal_result(cx, zoned_date_time_string_result, NAME)?;

        Ok(cx.alloc_string(&zoned_date_time_string)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.valueof)
    pub fn value_of(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "ZonedDateTime.prototype.valueOf must not be called")
    }

    /// Temporal.ZonedDateTime.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_zoned_date_time(cx, this_value, "ZonedDateTime.prototype.with")?;
        unimplemented!("ZonedDateTime.prototype.with")
    }

    /// Temporal.ZonedDateTime.prototype.withPlainTime (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.withplaintime)
    pub fn with_plain_time(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.withPlainTime";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let time_arg = get_argument(cx, arguments, 0);
        let time = if time_arg.is_undefined() {
            None
        } else {
            Some(to_temporal_time(cx, time_arg, NAME)?)
        };

        let new_zoned_date_time_result =
            this_zoned_date_time.zoned_date_time().with_plain_time(time);
        let new_zoned_date_time = map_temporal_result(cx, new_zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, new_zoned_date_time)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.withTimeZone (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.withtimezone)
    pub fn with_time_zone(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.withTimeZone";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let time_zone_arg = get_argument(cx, arguments, 0);
        let time_zone = to_time_zone_identifier(cx, time_zone_arg, NAME)?;

        let new_zoned_date_time_result = this_zoned_date_time
            .zoned_date_time()
            .with_timezone(time_zone);
        let new_zoned_date_time = map_temporal_result(cx, new_zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, new_zoned_date_time)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.withCalendar (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.withcalendar)
    pub fn with_calendar(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.withCalendar";

        let this_zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let calendar_arg = get_argument(cx, arguments, 0);
        let calendar = to_temporal_calendar_identifier(cx, calendar_arg, NAME)?;

        let new_zoned_date_time = this_zoned_date_time
            .zoned_date_time()
            .with_calendar(calendar);

        Ok(ZonedDateTimeObject::new(cx, new_zoned_date_time)?.as_value())
    }

    /// Temporal.ZonedDateTime.prototype.startOfDay (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.prototype.startofday)
    pub fn start_of_day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.prototype.startOfDay";

        let zoned_date_time = this_zoned_date_time(cx, this_value, NAME)?;

        let start_of_day_result = zoned_date_time.zoned_date_time().start_of_day();
        let start_of_day = map_temporal_result(cx, start_of_day_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, start_of_day)?.as_value())
    }
}

fn this_zoned_date_time(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<ZonedDateTimeObject>> {
    if value.is_object() {
        if let Some(zoned_date_time) = value.as_object().as_zoned_date_time_object() {
            return Ok(zoned_date_time);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a ZonedDateTime"))
}
