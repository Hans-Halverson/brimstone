use temporal_rs::options::DisplayCalendar;

use crate::{
    intrinsic_getter_methods, intrinsic_methods,
    runtime::{
        Arguments, Context, EvalResult, Handle, Realm, Value,
        alloc_error::AllocResult,
        error::type_error,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            temporal::{
                duration_constructor::to_temporal_duration,
                duration_object::DurationObject,
                plain_date_constructor::to_temporal_date,
                plain_date_object::PlainDateObject,
                plain_date_time_object::PlainDateTimeObject,
                plain_month_day_object::PlainMonthDayObject,
                plain_time_constructor::to_temporal_time,
                plain_year_month_object::PlainYearMonthObject,
                utils::{
                    DateField, DiffOperation, RequiredFieldNames, get_difference_settings,
                    get_overflow_option, get_show_calendar_name_option, is_partial_temporal_object,
                    map_temporal_result, prepare_calendar_fields, to_temporal_calendar_identifier,
                    to_time_zone_identifier, validate_options_object,
                },
                zoned_date_time_object::ZonedDateTimeObject,
            },
        },
        object_value::ObjectValue,
    },
    runtime_fn,
};

pub struct PlainDatePrototype;

impl PlainDatePrototype {
    /// Properties of the Temporal.PlainDate Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindate-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once PlainDateConstructor has been created

        intrinsic_methods!(cx, builder, {
            add                 PlainDatePrototype_add              (1),
            subtract            PlainDatePrototype_subtract         (1),
            until               PlainDatePrototype_until            (1),
            since               PlainDatePrototype_since            (1),
            equals              PlainDatePrototype_equals           (1),
            to_plain_date_time  PlainDatePrototype_toPlainDateTime  (0),
            to_plain_month_day  PlainDatePrototype_toPlainMonthDay  (0),
            to_plain_year_month PlainDatePrototype_toPlainYearMonth (0),
            to_zoned_date_time  PlainDatePrototype_toZonedDateTime  (1),
            to_string           PlainDatePrototype_toString         (0),
            to_locale_string    PlainDatePrototype_toLocaleString   (0),
            to_json             PlainDatePrototype_toJSON           (0),
            value_of            PlainDatePrototype_valueOf          (0),
            with                PlainDatePrototype_with             (1),
            with_calendar       PlainDatePrototype_withCalendar     (1),
        });

        intrinsic_getter_methods!(cx, builder, {
            calendar_id    PlainDatePrototype_calendarId,
            era            PlainDatePrototype_era,
            era_year       PlainDatePrototype_eraYear,
            year           PlainDatePrototype_year,
            month          PlainDatePrototype_month,
            month_code     PlainDatePrototype_monthCode,
            day            PlainDatePrototype_day,
            day_of_week    PlainDatePrototype_dayOfWeek,
            day_of_year    PlainDatePrototype_dayOfYear,
            week_of_year   PlainDatePrototype_weekOfYear,
            year_of_week   PlainDatePrototype_yearOfWeek,
            days_in_week   PlainDatePrototype_daysInWeek,
            days_in_month  PlainDatePrototype_daysInMonth,
            days_in_year   PlainDatePrototype_daysInYear,
            months_in_year PlainDatePrototype_monthsInYear,
            in_leap_year   PlainDatePrototype_inLeapYear,
        });

        builder.to_string_tag(cx.names.temporal_plain_date())?;

        builder.build()
    }

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.calendarId (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.calendarid)
    fn calendar_id(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.calendarId")?;
        let calendar_str = this_date.date().calendar().identifier();

        Ok(cx.alloc_static_string(calendar_str)?.as_value())
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.era (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.era)
    fn era(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.era")?;

        match this_date.date().era() {
            None => Ok(cx.undefined()),
            Some(era) => Ok(cx.alloc_string(era.as_str())?.as_value()),
        }
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.eraYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.erayear)
    fn era_year(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.eraYear")?;

        match this_date.date().era_year() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.year (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.year)
    fn year(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.year")?;
        let year_number = this_date.date().year();

        Ok(cx.smi(year_number))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.month (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.month)
    fn month(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.month")?;
        let month_number = this_date.date().month();

        Ok(cx.smi(month_number))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.monthCode (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.monthcode)
    fn month_code(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.monthCode")?;
        let month_code = this_date.date().month_code();

        Ok(cx.alloc_string(month_code.as_str())?.as_value())
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.day (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.day)
    fn day(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.day")?;
        let day_number = this_date.date().day();

        Ok(cx.smi(day_number))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.dayOfWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.dayofweek)
    fn day_of_week(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.dayOfWeek")?;
        let day_of_week_number = this_date.date().day_of_week();

        Ok(cx.smi(day_of_week_number))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.dayOfYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.dayofyear)
    fn day_of_year(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.dayOfYear")?;
        let day_of_year_number = this_date.date().day_of_year();

        Ok(cx.smi(day_of_year_number))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.weekOfYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.weekofyear)
    fn week_of_year(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.weekOfYear")?;

        match this_date.date().week_of_year() {
            None => Ok(cx.undefined()),
            Some(week_number) => Ok(cx.smi(week_number)),
        }
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.yearOfWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.yearofweek)
    fn year_of_week(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.yearOfWeek")?;

        match this_date.date().year_of_week() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.daysInWeek (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinweek)
    fn days_in_week(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.daysInWeek")?;
        let num_days_in_week = this_date.date().days_in_week();

        Ok(cx.smi(num_days_in_week))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.daysInMonth (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinmonth)
    fn days_in_month(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.daysInMonth")?;
        let num_days_in_month = this_date.date().days_in_month();

        Ok(cx.smi(num_days_in_month))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.daysInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.daysinyear)
    fn days_in_year(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.daysInYear")?;
        let num_days_in_year = this_date.date().days_in_year();

        Ok(cx.smi(num_days_in_year))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.monthsInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.monthsinyear)
    fn months_in_year(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.monthsInYear")?;
        let num_months_in_year = this_date.date().months_in_year();

        Ok(cx.smi(num_months_in_year))
    }}

    runtime_fn! {
    /// get Temporal.PlainDate.prototype.inLeapYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plaindate.prototype.inleapyear)
    fn in_leap_year(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.inLeapYear")?;
        let in_leap_year = this_date.date().in_leap_year();

        Ok(cx.bool(in_leap_year))
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.add)
    fn add(cx, this_value, arguments) {
        const NAME: &str = "PlainDate.prototype.add";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let duration_arg = arguments.get(cx, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = arguments.get(cx, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_date_result = this_date.date().add(&duration, Some(overflow));
        let new_date = map_temporal_result(cx, new_date_result, NAME)?;

        Ok(PlainDateObject::new(cx, new_date)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.subtract)
    fn subtract(cx, this_value, arguments) {
        const NAME: &str = "PlainDate.prototype.subtract";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let duration_arg = arguments.get(cx, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = arguments.get(cx, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_date_result = this_date.date().subtract(&duration, Some(overflow));
        let new_date = map_temporal_result(cx, new_date_result, NAME)?;

        Ok(PlainDateObject::new(cx, new_date)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.until)
    fn until(cx, this_value, arguments) {
        Self::diff(cx, this_value, arguments, DiffOperation::Until, "PlainDate.prototype.until")
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.since)
    fn since(cx, this_value, arguments) {
        Self::diff(cx, this_value, arguments, DiffOperation::Since, "PlainDate.prototype.since")
    }}

    fn diff(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
        operation: DiffOperation,
        method_name: &str,
    ) -> EvalResult<Handle<Value>> {
        let this_date = this_plain_date(cx, this_value, method_name)?;

        let other_arg = arguments.get(cx, 0);
        let other = to_temporal_date(cx, other_arg, method_name)?;

        let options_arg = arguments.get(cx, 1);
        let options = validate_options_object(cx, options_arg, method_name)?;
        let difference_settings = get_difference_settings(cx, options, method_name)?;

        let duration_result = match operation {
            DiffOperation::Until => this_date.date().until(&other, difference_settings),
            DiffOperation::Since => this_date.date().since(&other, difference_settings),
        };

        let duration = map_temporal_result(cx, duration_result, method_name)?;

        Ok(DurationObject::new(cx, duration)?.as_value())
    }

    runtime_fn! {
    /// Temporal.PlainDate.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.equals)
    fn equals(cx, this_value, arguments) {
        const NAME: &str = "PlainDate.prototype.equals";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let other_arg = arguments.get(cx, 0);
        let other_date = to_temporal_date(cx, other_arg, NAME)?;

        Ok(cx.bool(this_date.date() == &other_date))
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.toPlainDateTime (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.toplaindatetime)
    fn to_plain_date_time(cx, this_value, arguments) {
        const NAME: &str = "PlainDate.prototype.toPlainDateTime";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let time_arg = arguments.get(cx, 0);
        let time = if time_arg.is_undefined() {
            None
        } else {
            Some(to_temporal_time(cx, time_arg, NAME)?)
        };

        let plain_date_time_result = this_date.date().to_plain_date_time(time);
        let plain_date_time = map_temporal_result(cx, plain_date_time_result, NAME)?;

        Ok(PlainDateTimeObject::new(cx, plain_date_time)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.toPlainMonthDay (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.toplainmonthday)
    fn to_plain_month_day(cx, this_value, _) {
        const NAME: &str = "PlainDate.prototype.toPlainMonthDay";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let plain_month_day_result = this_date.date().to_plain_month_day();
        let plain_month_day = map_temporal_result(cx, plain_month_day_result, NAME)?;

        Ok(PlainMonthDayObject::new(cx, plain_month_day)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.toPlainYearMonth (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.toplainyearmonth)
    fn to_plain_year_month(cx, this_value, _) {
        const NAME: &str = "PlainDate.prototype.toPlainYearMonth";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let plain_year_month_result = this_date.date().to_plain_year_month();
        let plain_year_month = map_temporal_result(cx, plain_year_month_result, NAME)?;

        Ok(PlainYearMonthObject::new(cx, plain_year_month)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.toZonedDateTime (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tozoneddatetime)
    fn to_zoned_date_time(cx, this_value, arguments) {
        const NAME: &str = "PlainDate.prototype.toZonedDateTime";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let item_arg = arguments.get(cx, 0);

        let plain_time_opt;
        let time_zone;

        // Argument may be a time zone directly or an object that contains a time zone and
        // optionally a plain time.
        if item_arg.is_object() {
            let item_object = item_arg.as_object();
            let time_zone_like = get(cx, item_object, cx.names.time_zone())?;
            if time_zone_like.is_undefined() {
                time_zone = to_time_zone_identifier(cx, item_arg, NAME)?;
                plain_time_opt = None;
            } else {
                time_zone = to_time_zone_identifier(cx, time_zone_like, NAME)?;

                let plain_time_value = get(cx, item_object, cx.names.plain_time_())?;
                if plain_time_value.is_undefined() {
                    plain_time_opt = None;
                } else {
                    plain_time_opt = Some(to_temporal_time(cx, plain_time_value, NAME)?);
                }
            }
        } else {
            time_zone = to_time_zone_identifier(cx, item_arg, NAME)?;
            plain_time_opt = None;
        };

        let zoned_date_time_result = this_date.date().to_zoned_date_time_with_provider(
            time_zone,
            plain_time_opt,
            cx.temporal_provider(),
        );
        let zoned_date_time = map_temporal_result(cx, zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, zoned_date_time)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tostring)
    fn to_string(cx, this_value, arguments) {
        const NAME: &str = "PlainDate.prototype.toString";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        // Parse calendar format from options
        let options_arg = arguments.get(cx, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let display_name = get_show_calendar_name_option(cx, options, NAME)?;

        let date_string = this_date.date().to_ixdtf_string(display_name);

        Ok(cx.alloc_string(&date_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tolocalestring)
    fn to_locale_string(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.toLocaleString")?;
        let date_string = this_date.date().to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&date_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.tojson)
    fn to_json(cx, this_value, _) {
        let this_date = this_plain_date(cx, this_value, "PlainDate.prototype.toJSON")?;
        let date_string = this_date.date().to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&date_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.valueof)
    fn value_of(cx, _, _) {
        type_error(cx, "PlainDate.prototype.valueOf must not be called")
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.with)
    fn with(cx, this_value, arguments) {
        const NAME: &str = "PlainDate.prototype.with";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let date_like_arg = arguments.get(cx, 0);
        if !is_partial_temporal_object(cx, date_like_arg)? {
            return type_error(cx, "PlainDate.prototype.with argument must be a date-like object");
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

        let new_date_result = this_date.date().with(fields, Some(overflow));
        let new_date = map_temporal_result(cx, new_date_result, NAME)?;

        Ok(PlainDateObject::new(cx, new_date)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainDate.prototype.withCalendar (https://tc39.es/proposal-temporal/#sec-temporal.plaindate.prototype.withcalendar)
    fn with_calendar(cx, this_value, arguments) {
        const NAME: &str = "PlainDate.prototype.withCalendar";

        let this_date = this_plain_date(cx, this_value, NAME)?;

        let calendar_arg = arguments.get(cx, 0);
        let calendar = to_temporal_calendar_identifier(cx, calendar_arg, NAME)?;

        let new_date = this_date.date().with_calendar(calendar);

        Ok(PlainDateObject::new(cx, new_date)?.as_value())
    }}
}

fn this_plain_date(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<PlainDateObject>> {
    if value.is_object() {
        if let Some(plain_date) = value.as_opt::<PlainDateObject>() {
            return Ok(plain_date);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a PlainDate"))
}
