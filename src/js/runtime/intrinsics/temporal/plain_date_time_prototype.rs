use temporal_rs::options::{
    DisplayCalendar, RoundingMode, RoundingOptions, ToStringRoundingOptions,
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
            plain_date_object::PlainDateObject,
            plain_date_time_constructor::to_temporal_date_time,
            plain_date_time_object::PlainDateTimeObject,
            plain_time_constructor::to_temporal_time,
            plain_time_object::PlainTimeObject,
            utils::{
                DateField, DiffOperation, RequiredFieldNames, TimeField, get_difference_settings,
                get_disambiguation_option, get_fractional_second_digits_option,
                get_overflow_option, get_rounding_increment_option, get_rounding_mode_option,
                get_show_calendar_name_option, get_unit_valued_option, is_partial_temporal_object,
                map_temporal_result, parse_round_options_argument, prepare_calendar_fields,
                to_temporal_calendar_identifier, to_time_zone_identifier, validate_options_object,
            },
            zoned_date_time_object::ZonedDateTimeObject,
        },
    },
    object_value::ObjectValue,
    property::Property,
};

pub struct PlainDateTimePrototype;

impl PlainDateTimePrototype {
    /// Properties of the Temporal.PlainDateTime Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindatetime-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once PlainDateTimeConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(
                cx.names.temporal_plain_date_time().as_string().into(),
                false,
                false,
                true,
            ),
        )?;

        // Date getters
        object.intrinsic_getter(
            cx,
            cx.names.calendar_id(),
            RuntimeFunction::PlainDateTimePrototype_calendarId,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.era(),
            RuntimeFunction::PlainDateTimePrototype_era,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.era_year(),
            RuntimeFunction::PlainDateTimePrototype_eraYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.year(),
            RuntimeFunction::PlainDateTimePrototype_year,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month(),
            RuntimeFunction::PlainDateTimePrototype_month,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month_code(),
            RuntimeFunction::PlainDateTimePrototype_monthCode,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day(),
            RuntimeFunction::PlainDateTimePrototype_day,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day_of_week(),
            RuntimeFunction::PlainDateTimePrototype_dayOfWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.day_of_year(),
            RuntimeFunction::PlainDateTimePrototype_dayOfYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.week_of_year(),
            RuntimeFunction::PlainDateTimePrototype_weekOfYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.year_of_week(),
            RuntimeFunction::PlainDateTimePrototype_yearOfWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_week(),
            RuntimeFunction::PlainDateTimePrototype_daysInWeek,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_month(),
            RuntimeFunction::PlainDateTimePrototype_daysInMonth,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_year(),
            RuntimeFunction::PlainDateTimePrototype_daysInYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.months_in_year(),
            RuntimeFunction::PlainDateTimePrototype_monthsInYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.in_leap_year(),
            RuntimeFunction::PlainDateTimePrototype_inLeapYear,
            realm,
        )?;

        // Time getters
        object.intrinsic_getter(
            cx,
            cx.names.hour(),
            RuntimeFunction::PlainDateTimePrototype_hour,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.minute(),
            RuntimeFunction::PlainDateTimePrototype_minute,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.second(),
            RuntimeFunction::PlainDateTimePrototype_second,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.millisecond(),
            RuntimeFunction::PlainDateTimePrototype_millisecond,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.microsecond(),
            RuntimeFunction::PlainDateTimePrototype_microsecond,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.nanosecond(),
            RuntimeFunction::PlainDateTimePrototype_nanosecond,
            realm,
        )?;

        // Methods
        object.intrinsic_func(
            cx,
            cx.names.add(),
            RuntimeFunction::PlainDateTimePrototype_add,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.subtract(),
            RuntimeFunction::PlainDateTimePrototype_subtract,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.until(),
            RuntimeFunction::PlainDateTimePrototype_until,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.since(),
            RuntimeFunction::PlainDateTimePrototype_since,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.round(),
            RuntimeFunction::PlainDateTimePrototype_round,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.equals(),
            RuntimeFunction::PlainDateTimePrototype_equals,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_date(),
            RuntimeFunction::PlainDateTimePrototype_toPlainDate,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_time(),
            RuntimeFunction::PlainDateTimePrototype_toPlainTime,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_zoned_date_time(),
            RuntimeFunction::PlainDateTimePrototype_toZonedDateTime,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::PlainDateTimePrototype_toString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            RuntimeFunction::PlainDateTimePrototype_toLocaleString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_json(),
            RuntimeFunction::PlainDateTimePrototype_toJSON,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::PlainDateTimePrototype_valueOf,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with(),
            RuntimeFunction::PlainDateTimePrototype_with,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with_plain_time(),
            RuntimeFunction::PlainDateTimePrototype_withPlainTime,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with_calendar(),
            RuntimeFunction::PlainDateTimePrototype_withCalendar,
            1,
            realm,
        )?;

        Ok(object)
    }

    /// get Temporal.PlainDateTime.prototype.calendarId (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.calendarid)
    pub fn calendar_id(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.calendarId")?;
        let calendar_str = this_date_time.date_time().calendar().identifier();

        Ok(cx.alloc_static_string(calendar_str)?.as_value())
    }

    /// get Temporal.PlainDateTime.prototype.era (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.era)
    pub fn era(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time = this_plain_date_time(cx, this_value, "PlainDateTime.prototype.era")?;

        match this_date_time.date_time().era() {
            None => Ok(cx.undefined()),
            Some(era) => Ok(cx.alloc_string(era.as_str())?.as_value()),
        }
    }

    /// get Temporal.PlainDateTime.prototype.eraYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.erayear)
    pub fn era_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.eraYear")?;

        match this_date_time.date_time().era_year() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }

    /// get Temporal.PlainDateTime.prototype.year (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.year)
    pub fn year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time = this_plain_date_time(cx, this_value, "PlainDateTime.prototype.year")?;
        let year = this_date_time.date_time().year();

        Ok(cx.smi(year))
    }

    /// get Temporal.PlainDateTime.prototype.month (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.month)
    pub fn month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time = this_plain_date_time(cx, this_value, "PlainDateTime.prototype.month")?;
        let month = this_date_time.date_time().month();

        Ok(cx.smi(month as i32))
    }

    /// get Temporal.PlainDateTime.prototype.monthCode (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.monthcode)
    pub fn month_code(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.monthCode")?;
        let month_code = this_date_time.date_time().month_code();

        Ok(cx.alloc_string(month_code.as_str())?.as_value())
    }

    /// get Temporal.PlainDateTime.prototype.day (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.day)
    pub fn day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time = this_plain_date_time(cx, this_value, "PlainDateTime.prototype.day")?;
        let day = this_date_time.date_time().day();

        Ok(cx.smi(day as i32))
    }

    /// get Temporal.PlainDateTime.prototype.dayOfWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.dayofweek)
    pub fn day_of_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.dayOfWeek")?;
        let day_of_week = this_date_time.date_time().day_of_week();

        Ok(cx.smi(day_of_week as i32))
    }

    /// get Temporal.PlainDateTime.prototype.dayOfYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.dayofyear)
    pub fn day_of_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.dayOfYear")?;
        let day_of_year = this_date_time.date_time().day_of_year();

        Ok(cx.smi(day_of_year as i32))
    }

    /// get Temporal.PlainDateTime.prototype.weekOfYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.weekofyear)
    pub fn week_of_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.weekOfYear")?;

        match this_date_time.date_time().week_of_year() {
            None => Ok(cx.undefined()),
            Some(week_number) => Ok(cx.smi(week_number as i32)),
        }
    }

    /// get Temporal.PlainDateTime.prototype.yearOfWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.yearofweek)
    pub fn year_of_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.yearOfWeek")?;

        match this_date_time.date_time().year_of_week() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }

    /// get Temporal.PlainDateTime.prototype.daysInWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.daysinweek)
    pub fn days_in_week(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.daysInWeek")?;
        let days_in_week = this_date_time.date_time().days_in_week();

        Ok(cx.smi(days_in_week as i32))
    }

    /// get Temporal.PlainDateTime.prototype.daysInMonth (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.daysinmonth)
    pub fn days_in_month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.daysInMonth")?;
        let days_in_month = this_date_time.date_time().days_in_month();

        Ok(cx.smi(days_in_month as i32))
    }

    /// get Temporal.PlainDateTime.prototype.daysInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.daysinyear)
    pub fn days_in_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.daysInYear")?;
        let days_in_year = this_date_time.date_time().days_in_year();

        Ok(cx.smi(days_in_year as i32))
    }

    /// get Temporal.PlainDateTime.prototype.monthsInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.monthsinyear)
    pub fn months_in_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.monthsInYear")?;
        let months_in_year = this_date_time.date_time().months_in_year();

        Ok(cx.smi(months_in_year as i32))
    }

    /// get Temporal.PlainDateTime.prototype.inLeapYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.inleapyear)
    pub fn in_leap_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.inLeapYear")?;
        let in_leap_year = this_date_time.date_time().in_leap_year();

        Ok(cx.bool(in_leap_year))
    }

    /// get Temporal.PlainDateTime.prototype.hour (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.hour)
    pub fn hour(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time = this_plain_date_time(cx, this_value, "PlainDateTime.prototype.hour")?;
        let hour = this_date_time.date_time().hour();

        Ok(cx.smi(hour as i32))
    }

    /// get Temporal.PlainDateTime.prototype.minute (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.minute)
    pub fn minute(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.minute")?;
        let minute = this_date_time.date_time().minute();

        Ok(cx.smi(minute as i32))
    }

    /// get Temporal.PlainDateTime.prototype.second (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.second)
    pub fn second(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.second")?;
        let second = this_date_time.date_time().second();

        Ok(cx.smi(second as i32))
    }

    /// get Temporal.PlainDateTime.prototype.millisecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.millisecond)
    pub fn millisecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.millisecond")?;
        let millis = this_date_time.date_time().millisecond();

        Ok(cx.smi(millis as i32))
    }

    /// get Temporal.PlainDateTime.prototype.microsecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.microsecond)
    pub fn microsecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.microsecond")?;
        let micros = this_date_time.date_time().microsecond();

        Ok(cx.smi(micros as i32))
    }

    /// get Temporal.PlainDateTime.prototype.nanosecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindatetime.prototype.nanosecond)
    pub fn nanosecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.nanosecond")?;
        let nanos = this_date_time.date_time().nanosecond();

        Ok(cx.smi(nanos as i32))
    }

    /// Temporal.PlainDateTime.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.add";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_date_time_result = this_date_time.date_time().add(&duration, Some(overflow));
        let new_date_time = map_temporal_result(cx, new_date_time_result, NAME)?;

        Ok(PlainDateTimeObject::new(cx, new_date_time)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.subtract)
    pub fn subtract(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.subtract";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_date_time_result = this_date_time
            .date_time()
            .subtract(&duration, Some(overflow));
        let new_date_time = map_temporal_result(cx, new_date_time_result, NAME)?;

        Ok(PlainDateTimeObject::new(cx, new_date_time)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.until)
    pub fn until(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        Self::diff(cx, this_value, arguments, DiffOperation::Until, "PlainDateTime.prototype.until")
    }

    /// Temporal.PlainDateTime.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.since)
    pub fn since(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        Self::diff(cx, this_value, arguments, DiffOperation::Since, "PlainDateTime.prototype.since")
    }

    fn diff(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        operation: DiffOperation,
        method_name: &str,
    ) -> EvalResult<Handle<Value>> {
        let plain_date_time = this_plain_date_time(cx, this_value, method_name)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other = to_temporal_date_time(cx, other_arg, method_name)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, method_name)?;
        let difference_settings = get_difference_settings(cx, options, method_name)?;

        let duration_result = match operation {
            DiffOperation::Until => plain_date_time
                .date_time()
                .until(&other, difference_settings),
            DiffOperation::Since => plain_date_time
                .date_time()
                .since(&other, difference_settings),
        };

        let duration = map_temporal_result(cx, duration_result, method_name)?;

        Ok(DurationObject::new(cx, duration)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.round (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.round)
    pub fn round(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.round";

        let plain_date_time = this_plain_date_time(cx, this_value, NAME)?;

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

        let rounded_result = plain_date_time.date_time().round(rounding_options);
        let rounded = map_temporal_result(cx, rounded_result, NAME)?;

        Ok(PlainDateTimeObject::new(cx, rounded)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.equals)
    pub fn equals(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.equals";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other_date_time = to_temporal_date_time(cx, other_arg, NAME)?;

        Ok(cx.bool(this_date_time.date_time() == &other_date_time))
    }

    /// Temporal.PlainDateTime.prototype.toPlainDate (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.toplaindate)
    pub fn to_plain_date(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.toPlainDate")?;
        let plain_date = this_date_time.date_time().to_plain_date();

        Ok(PlainDateObject::new(cx, plain_date)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.toPlainTime (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.toplaintime)
    pub fn to_plain_time(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_date_time =
            this_plain_date_time(cx, this_value, "PlainDateTime.prototype.toPlainTime")?;
        let plain_time = this_date_time.date_time().to_plain_time();

        Ok(PlainTimeObject::new(cx, plain_time)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.toZonedDateTime (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.tozoneddatetime)
    pub fn to_zoned_date_time(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.toZonedDateTime";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;

        let time_zone_arg = get_argument(cx, arguments, 0);
        let time_zone = to_time_zone_identifier(cx, time_zone_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let disambiguation = get_disambiguation_option(cx, options, NAME)?;

        let zoned_date_time_result = this_date_time
            .date_time()
            .to_zoned_date_time(time_zone, disambiguation);
        let zoned_date_time = map_temporal_result(cx, zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, zoned_date_time)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.toString";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;

        // Parse rounding options from options object
        let display_calendar = get_show_calendar_name_option(cx, options, NAME)?;
        let precision = get_fractional_second_digits_option(cx, options, NAME)?;
        let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::Trunc, NAME)?;
        let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;

        let to_string_options = ToStringRoundingOptions {
            precision,
            smallest_unit,
            rounding_mode: Some(rounding_mode),
        };

        let date_time_string_result = this_date_time
            .date_time()
            .to_ixdtf_string(to_string_options, display_calendar);
        let date_time_string = map_temporal_result(cx, date_time_string_result, NAME)?;

        Ok(cx.alloc_string(&date_time_string)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.tolocalestring)
    pub fn to_locale_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.toLocaleString";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;
        let date_time_string_result = this_date_time
            .date_time()
            .to_ixdtf_string(ToStringRoundingOptions::default(), DisplayCalendar::Auto);
        let date_time_string = map_temporal_result(cx, date_time_string_result, NAME)?;

        Ok(cx.alloc_string(&date_time_string)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.tojson)
    pub fn to_json(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.toJSON";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;
        let date_time_string_result = this_date_time
            .date_time()
            .to_ixdtf_string(ToStringRoundingOptions::default(), DisplayCalendar::Auto);
        let date_time_string = map_temporal_result(cx, date_time_string_result, NAME)?;

        Ok(cx.alloc_string(&date_time_string)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.valueof)
    pub fn value_of(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "PlainDateTime.prototype.valueOf must not be called")
    }

    /// Temporal.PlainDateTime.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.with";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;

        let date_like_arg = get_argument(cx, arguments, 0);
        if !is_partial_temporal_object(cx, date_like_arg)? {
            return type_error(
                cx,
                "PlainDateTime.prototype.with argument must be a date-like object",
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
            &[
                TimeField::Hour,
                TimeField::Minute,
                TimeField::Second,
                TimeField::Millisecond,
                TimeField::Microsecond,
                TimeField::Nanosecond,
            ],
            RequiredFieldNames::Partial,
            NAME,
        )?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_date_time_result = this_date_time
            .date_time()
            .with(prepared_fields.into_partial_date_time(), Some(overflow));
        let new_date_time = map_temporal_result(cx, new_date_time_result, NAME)?;

        Ok(PlainDateTimeObject::new(cx, new_date_time)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.withPlainTime (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.withplaintime)
    pub fn with_plain_time(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.withPlainTime";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;

        let time_arg = get_argument(cx, arguments, 0);
        let time = if time_arg.is_undefined() {
            None
        } else {
            Some(to_temporal_time(cx, time_arg, NAME)?)
        };

        let new_date_time_result = this_date_time.date_time().with_time(time);
        let new_date_time = map_temporal_result(cx, new_date_time_result, NAME)?;

        Ok(PlainDateTimeObject::new(cx, new_date_time)?.as_value())
    }

    /// Temporal.PlainDateTime.prototype.withCalendar (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.prototype.withcalendar)
    pub fn with_calendar(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.prototype.withCalendar";

        let this_date_time = this_plain_date_time(cx, this_value, NAME)?;

        let calendar_arg = get_argument(cx, arguments, 0);
        let calendar = to_temporal_calendar_identifier(cx, calendar_arg, NAME)?;

        let new_date_time = this_date_time.date_time().with_calendar(calendar);

        Ok(PlainDateTimeObject::new(cx, new_date_time)?.as_value())
    }
}

fn this_plain_date_time(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<PlainDateTimeObject>> {
    if value.is_object() {
        if let Some(plain_date_time) = value.as_object().as_plain_date_time_object() {
            return Ok(plain_date_time);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a PlainDateTime"))
}
