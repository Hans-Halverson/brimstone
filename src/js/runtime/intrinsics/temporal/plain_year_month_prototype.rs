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
            plain_year_month_constructor::to_temporal_year_month,
            plain_year_month_object::PlainYearMonthObject,
            utils::{
                get_overflow_option, get_show_calendar_name_option, map_temporal_result,
                validate_options_object,
            },
        },
    },
    object_value::ObjectValue,
    property::Property,
};

pub struct PlainYearMonthPrototype;

impl PlainYearMonthPrototype {
    /// Properties of the Temporal.PlainYearMonth Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plainyearmonth-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once PlainYearMonthConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(
                cx.names.temporal_plain_year_month().as_string().into(),
                false,
                false,
                true,
            ),
        )?;

        // Getters
        object.intrinsic_getter(
            cx,
            cx.names.calendar_id(),
            RuntimeFunction::PlainYearMonthPrototype_calendarId,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.era(),
            RuntimeFunction::PlainYearMonthPrototype_era,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.era_year(),
            RuntimeFunction::PlainYearMonthPrototype_eraYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.year(),
            RuntimeFunction::PlainYearMonthPrototype_year,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month(),
            RuntimeFunction::PlainYearMonthPrototype_month,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.month_code(),
            RuntimeFunction::PlainYearMonthPrototype_monthCode,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_year(),
            RuntimeFunction::PlainYearMonthPrototype_daysInYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days_in_month(),
            RuntimeFunction::PlainYearMonthPrototype_daysInMonth,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.months_in_year(),
            RuntimeFunction::PlainYearMonthPrototype_monthsInYear,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.in_leap_year(),
            RuntimeFunction::PlainYearMonthPrototype_inLeapYear,
            realm,
        )?;

        // Methods
        object.intrinsic_func(
            cx,
            cx.names.add(),
            RuntimeFunction::PlainYearMonthPrototype_add,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.subtract(),
            RuntimeFunction::PlainYearMonthPrototype_subtract,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.until(),
            RuntimeFunction::PlainYearMonthPrototype_until,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.since(),
            RuntimeFunction::PlainYearMonthPrototype_since,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.equals(),
            RuntimeFunction::PlainYearMonthPrototype_equals,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_date(),
            RuntimeFunction::PlainYearMonthPrototype_toPlainDate,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::PlainYearMonthPrototype_toString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            RuntimeFunction::PlainYearMonthPrototype_toLocaleString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_json(),
            RuntimeFunction::PlainYearMonthPrototype_toJSON,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::PlainYearMonthPrototype_valueOf,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with(),
            RuntimeFunction::PlainYearMonthPrototype_with,
            1,
            realm,
        )?;

        Ok(object)
    }

    /// get Temporal.PlainYearMonth.prototype.calendarId (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.calendarid)
    pub fn calendar_id(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.calendarId")?;
        let calendar_str = this_year_month.year_month().calendar_id();

        Ok(cx.alloc_static_string(calendar_str)?.as_value())
    }

    /// get Temporal.PlainYearMonth.prototype.era (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.era)
    pub fn era(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.era")?;

        match this_year_month.year_month().era() {
            None => Ok(cx.undefined()),
            Some(era) => Ok(cx.alloc_string(era.as_str())?.as_value()),
        }
    }

    /// get Temporal.PlainYearMonth.prototype.eraYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.erayear)
    pub fn era_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.eraYear")?;

        match this_year_month.year_month().era_year() {
            None => Ok(cx.undefined()),
            Some(year_number) => Ok(cx.smi(year_number)),
        }
    }

    /// get Temporal.PlainYearMonth.prototype.year (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.year)
    pub fn year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.year")?;
        let year = this_year_month.year_month().year();

        Ok(cx.smi(year))
    }

    /// get Temporal.PlainYearMonth.prototype.month (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.month)
    pub fn month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.month")?;
        let month = this_year_month.year_month().month();

        Ok(cx.smi(month as i32))
    }

    /// get Temporal.PlainYearMonth.prototype.monthCode (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.monthcode)
    pub fn month_code(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.monthCode")?;
        let month_code = this_year_month.year_month().month_code();

        Ok(cx.alloc_string(month_code.as_str())?.as_value())
    }

    /// get Temporal.PlainYearMonth.prototype.daysInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.daysinyear)
    pub fn days_in_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.daysInYear")?;
        let days_in_year = this_year_month.year_month().days_in_year();

        Ok(cx.smi(days_in_year as i32))
    }

    /// get Temporal.PlainYearMonth.prototype.daysInMonth (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.daysinmonth)
    pub fn days_in_month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.daysInMonth")?;
        let days_in_month = this_year_month.year_month().days_in_month();

        Ok(cx.smi(days_in_month as i32))
    }

    /// get Temporal.PlainYearMonth.prototype.monthsInYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.monthsinyear)
    pub fn months_in_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.monthsInYear")?;
        let months_in_year = this_year_month.year_month().months_in_year();

        Ok(cx.smi(months_in_year as i32))
    }

    /// get Temporal.PlainYearMonth.prototype.inLeapYear (https://tc39.es/proposal-temporal/#sec-get-temporal.plainyearmonth.prototype.inleapyear)
    pub fn in_leap_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.inLeapYear")?;
        let in_leap_year = this_year_month.year_month().in_leap_year();

        Ok(cx.bool(in_leap_year))
    }

    /// Temporal.PlainYearMonth.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainYearMonth.prototype.add";

        let this_year_month = this_plain_year_month(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_year_month_result = this_year_month.year_month().add(&duration, overflow);
        let new_year_month = map_temporal_result(cx, new_year_month_result, NAME)?;

        Ok(PlainYearMonthObject::new(cx, new_year_month)?.as_value())
    }

    /// Temporal.PlainYearMonth.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.subtract)
    pub fn subtract(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainYearMonth.prototype.subtract";

        let this_year_month = this_plain_year_month(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_year_month_result = this_year_month.year_month().subtract(&duration, overflow);
        let new_year_month = map_temporal_result(cx, new_year_month_result, NAME)?;

        Ok(PlainYearMonthObject::new(cx, new_year_month)?.as_value())
    }

    /// Temporal.PlainYearMonth.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.until)
    pub fn until(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.until")?;
        unimplemented!("PlainYearMonth.prototype.until")
    }

    /// Temporal.PlainYearMonth.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.since)
    pub fn since(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.since")?;
        unimplemented!("PlainYearMonth.prototype.since")
    }

    /// Temporal.PlainYearMonth.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.equals)
    pub fn equals(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainYearMonth.prototype.equals";

        let this_year_month = this_plain_year_month(cx, this_value, NAME)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other_year_month = to_temporal_year_month(cx, other_arg, NAME)?;

        Ok(cx.bool(this_year_month.year_month() == &other_year_month))
    }

    /// Temporal.PlainYearMonth.prototype.toPlainDate (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.toplaindate)
    pub fn to_plain_date(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.toPlainDate")?;
        unimplemented!("PlainYearMonth.prototype.toPlainDate")
    }

    /// Temporal.PlainYearMonth.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainYearMonth.prototype.toString";

        let this_year_month = this_plain_year_month(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let display_calendar = get_show_calendar_name_option(cx, options, NAME)?;

        let year_month_string = this_year_month
            .year_month()
            .to_ixdtf_string(display_calendar);

        Ok(cx.alloc_string(&year_month_string)?.as_value())
    }

    /// Temporal.PlainYearMonth.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.tolocalestring)
    pub fn to_locale_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.toLocaleString")?;
        let year_month_string = this_year_month
            .year_month()
            .to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&year_month_string)?.as_value())
    }

    /// Temporal.PlainYearMonth.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.tojson)
    pub fn to_json(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_year_month =
            this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.toJSON")?;
        let year_month_string = this_year_month
            .year_month()
            .to_ixdtf_string(DisplayCalendar::Auto);

        Ok(cx.alloc_string(&year_month_string)?.as_value())
    }

    /// Temporal.PlainYearMonth.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.valueof)
    pub fn value_of(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "PlainYearMonth.prototype.valueOf must not be called")
    }

    /// Temporal.PlainYearMonth.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.plainyearmonth.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_year_month(cx, this_value, "PlainYearMonth.prototype.with")?;
        unimplemented!("PlainYearMonth.prototype.with")
    }
}

fn this_plain_year_month(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<PlainYearMonthObject>> {
    if value.is_object() {
        if let Some(plain_year_month) = value.as_object().as_plain_year_month_object() {
            return Ok(plain_year_month);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a PlainYearMonth"))
}
