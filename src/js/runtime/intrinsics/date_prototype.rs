use crate::runtime::{
    abstract_operations::invoke,
    builtin_function::BuiltinFunction,
    error::{range_error, type_error},
    function::get_argument,
    intrinsics::date_object::{day, make_date, make_time, time_clip},
    object_value::ObjectValue,
    property::Property,
    string_value::StringValue,
    type_utilities::{
        ordinary_to_primitive, to_number, to_object, to_primitive, ToPrimitivePreferredType,
    },
    Context, EvalResult, Handle, Realm, Value,
};

use super::{
    date_object::{
        date_from_time, hour_from_time, local_time, make_day, millisecond_from_time,
        minute_from_time, month_from_time, second_from_time, time_within_day, utc, week_day,
        year_from_time, DateObject, MS_PER_MINUTE,
    },
    intrinsics::Intrinsic,
};

pub struct DatePrototype;

impl DatePrototype {
    /// Properties of the Date Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-date-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once DateConstructor has been created
        object.intrinsic_func(cx, cx.names.get_date(), Self::get_date, 0, realm);
        object.intrinsic_func(cx, cx.names.get_day(), Self::get_day, 0, realm);
        object.intrinsic_func(cx, cx.names.get_full_year(), Self::get_full_year, 0, realm);
        object.intrinsic_func(cx, cx.names.get_hours(), Self::get_hours, 0, realm);
        object.intrinsic_func(cx, cx.names.get_milliseconds(), Self::get_milliseconds, 0, realm);
        object.intrinsic_func(cx, cx.names.get_minutes(), Self::get_minutes, 0, realm);
        object.intrinsic_func(cx, cx.names.get_month(), Self::get_month, 0, realm);
        object.intrinsic_func(cx, cx.names.get_seconds(), Self::get_seconds, 0, realm);
        object.intrinsic_func(cx, cx.names.get_time(), Self::get_time, 0, realm);
        object.intrinsic_func(
            cx,
            cx.names.get_timezone_offset(),
            Self::get_timezone_offset,
            0,
            realm,
        );
        object.intrinsic_func(cx, cx.names.get_utc_date(), Self::get_utc_date, 0, realm);
        object.intrinsic_func(cx, cx.names.get_utc_day(), Self::get_utc_day, 0, realm);
        object.intrinsic_func(cx, cx.names.get_utc_full_year(), Self::get_utc_full_year, 0, realm);
        object.intrinsic_func(cx, cx.names.get_utc_hours(), Self::get_utc_hours, 0, realm);
        object.intrinsic_func(
            cx,
            cx.names.get_utc_milliseconds(),
            Self::get_utc_milliseconds,
            0,
            realm,
        );
        object.intrinsic_func(cx, cx.names.get_utc_minutes(), Self::get_utc_minutes, 0, realm);
        object.intrinsic_func(cx, cx.names.get_utc_month(), Self::get_utc_month, 0, realm);
        object.intrinsic_func(cx, cx.names.get_utc_seconds(), Self::get_utc_seconds, 0, realm);
        object.intrinsic_func(cx, cx.names.set_date(), Self::set_date, 1, realm);
        object.intrinsic_func(cx, cx.names.set_full_year(), Self::set_full_year, 3, realm);
        object.intrinsic_func(cx, cx.names.set_hours(), Self::set_hours, 4, realm);
        object.intrinsic_func(cx, cx.names.set_milliseconds(), Self::set_milliseconds, 1, realm);
        object.intrinsic_func(cx, cx.names.set_minutes(), Self::set_minutes, 3, realm);
        object.intrinsic_func(cx, cx.names.set_month(), Self::set_month, 2, realm);
        object.intrinsic_func(cx, cx.names.set_seconds(), Self::set_seconds, 2, realm);
        object.intrinsic_func(cx, cx.names.set_time(), Self::set_time, 1, realm);
        object.intrinsic_func(cx, cx.names.set_utc_date(), Self::set_utc_date, 1, realm);
        object.intrinsic_func(cx, cx.names.set_utc_full_year(), Self::set_utc_full_year, 3, realm);
        object.intrinsic_func(cx, cx.names.set_utc_hours(), Self::set_utc_hours, 4, realm);
        object.intrinsic_func(
            cx,
            cx.names.set_utc_milliseconds(),
            Self::set_utc_milliseconds,
            1,
            realm,
        );
        object.intrinsic_func(cx, cx.names.set_utc_minutes(), Self::set_utc_minutes, 3, realm);
        object.intrinsic_func(cx, cx.names.set_utc_month(), Self::set_utc_month, 2, realm);
        object.intrinsic_func(cx, cx.names.set_utc_seconds(), Self::set_utc_seconds, 2, realm);
        object.intrinsic_func(cx, cx.names.to_date_string(), Self::to_date_string, 0, realm);
        object.intrinsic_func(cx, cx.names.to_iso_string(), Self::to_iso_string, 0, realm);
        object.intrinsic_func(cx, cx.names.to_json(), Self::to_json, 1, realm);
        object.intrinsic_func(
            cx,
            cx.names.to_locale_date_string(),
            Self::to_locale_date_string,
            0,
            realm,
        );
        object.intrinsic_func(cx, cx.names.to_locale_string(), Self::to_locale_string, 0, realm);
        object.intrinsic_func(
            cx,
            cx.names.to_locale_time_string(),
            Self::to_locale_time_string,
            0,
            realm,
        );
        object.intrinsic_func(cx, cx.names.to_time_string(), Self::to_time_string, 0, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, cx.names.to_utc_string(), Self::to_utc_string, 0, realm);
        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        // [Symbol.toPrimitive] property
        let to_primitive_key = cx.well_known_symbols.to_primitive();
        let to_primitive_func =
            BuiltinFunction::create(cx, Self::to_primitive, 1, to_primitive_key, realm, None, None)
                .into();
        object.set_property(
            cx,
            to_primitive_key,
            Property::data(to_primitive_func, false, false, true),
        );

        object
    }

    /// Date.prototype.getDate (https://tc39.es/ecma262/#sec-date.prototype.getdate)
    pub fn get_date(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.getDate method must be called on Date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let date = date_from_time(local_time(date_value));

        Ok(Value::from(date).to_handle(cx))
    }

    /// Date.prototype.getDay (https://tc39.es/ecma262/#sec-date.prototype.getday)
    pub fn get_day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.getDay method must be called on Date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let day = week_day(local_time(date_value));

        Ok(Value::from(day).to_handle(cx))
    }

    /// Date.prototype.getFullYear (https://tc39.es/ecma262/#sec-date.prototype.getfullyear)
    pub fn get_full_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.getFullYear method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let year = year_from_time(local_time(date_value));

        Ok(Value::from(year).to_handle(cx))
    }

    /// Date.prototype.getHours (https://tc39.es/ecma262/#sec-date.prototype.gethours)
    pub fn get_hours(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.getHours method must be called on Date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let hour = hour_from_time(local_time(date_value));

        Ok(Value::from(hour).to_handle(cx))
    }

    /// Date.prototype.getMilliseconds (https://tc39.es/ecma262/#sec-date.prototype.getmilliseconds)
    pub fn get_milliseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.getMilliseconds method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let millisecond = millisecond_from_time(local_time(date_value));

        Ok(Value::from(millisecond).to_handle(cx))
    }

    /// Date.prototype.getMinutes (https://tc39.es/ecma262/#sec-date.prototype.getminutes)
    pub fn get_minutes(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.getMinutes method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let minute = minute_from_time(local_time(date_value));

        Ok(Value::from(minute).to_handle(cx))
    }

    /// Date.prototype.getMonth (https://tc39.es/ecma262/#sec-date.prototype.getmonth)
    pub fn get_month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.getMonth method must be called on Date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let month = month_from_time(local_time(date_value));

        Ok(Value::from(month).to_handle(cx))
    }

    /// Date.prototype.getSeconds (https://tc39.es/ecma262/#sec-date.prototype.getseconds)
    pub fn get_seconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.getSeconds method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let second = second_from_time(local_time(date_value));

        Ok(Value::from(second).to_handle(cx))
    }

    /// Date.prototype.getTime (https://tc39.es/ecma262/#sec-date.prototype.gettime)
    pub fn get_time(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if let Some(date_value) = this_date_value(this_value) {
            Ok(Value::from(date_value).to_handle(cx))
        } else {
            type_error(cx, "getTime method must be called on date object")
        }
    }

    /// Date.prototype.getTimezoneOffset (https://tc39.es/ecma262/#sec-date.prototype.gettimezoneoffset)
    pub fn get_timezone_offset(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "getTimezeonOffset method must be called on date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let timezone_offset = (date_value - local_time(date_value)) / MS_PER_MINUTE;

        Ok(Value::from(timezone_offset).to_handle(cx))
    }

    /// Date.prototype.getUTCDate (https://tc39.es/ecma262/#sec-date.prototype.getutcdate)
    pub fn get_utc_date(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.getUTCDate must be called on Date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let date = date_from_time(date_value);

        Ok(Value::from(date).to_handle(cx))
    }

    /// Date.prototype.getUTCDay (https://tc39.es/ecma262/#sec-date.prototype.getutcday)
    pub fn get_utc_day(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.getUTCDay must be called on Date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let hour = week_day(date_value);

        Ok(Value::from(hour).to_handle(cx))
    }

    /// Date.prototype.getUTCFullYear (https://tc39.es/ecma262/#sec-date.prototype.getutcfullyear)
    pub fn get_utc_full_year(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.getUTCFullYear must be called on Date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let year = year_from_time(date_value);

        Ok(Value::from(year).to_handle(cx))
    }

    /// Date.prototype.getUTCHours (https://tc39.es/ecma262/#sec-date.prototype.getutchours)
    pub fn get_utc_hours(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.getUTCHours must be called on Date object");
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let hour = hour_from_time(date_value);

        Ok(Value::from(hour).to_handle(cx))
    }

    /// Date.prototype.getUTCMilliseconds (https://tc39.es/ecma262/#sec-date.prototype.getutcmilliseconds)
    pub fn get_utc_milliseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.getUTCMilliseconds method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let millisecond = millisecond_from_time(date_value);

        Ok(Value::from(millisecond).to_handle(cx))
    }

    /// Date.prototype.getUTCMinutes (https://tc39.es/ecma262/#sec-date.prototype.getutcminutes)
    pub fn get_utc_minutes(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.getUTCMinutes method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let minute = minute_from_time(date_value);

        Ok(Value::from(minute).to_handle(cx))
    }

    /// Date.prototype.getUTCMonth (https://tc39.es/ecma262/#sec-date.prototype.getutcmonth)
    pub fn get_utc_month(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.getUTCMonth method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let month = month_from_time(date_value);

        Ok(Value::from(month).to_handle(cx))
    }

    /// Date.prototype.getUTCSeconds (https://tc39.es/ecma262/#sec-date.prototype.getutcseconds)
    pub fn get_utc_seconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.getUTCSeconds method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let second = second_from_time(date_value);

        Ok(Value::from(second).to_handle(cx))
    }

    /// Date.prototype.setDate (https://tc39.es/ecma262/#sec-date.prototype.setdate)
    pub fn set_date(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.setDate method must be called on Date object");
        };

        let date_arg = get_argument(cx, arguments, 0);
        let date = to_number(cx, date_arg)?.as_number();

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let date_value = local_time(date_value);

        let new_date = time_clip(utc(make_date(
            make_day(year_from_time(date_value), month_from_time(date_value), date),
            time_within_day(date_value),
        )));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setFullYear (https://tc39.es/ecma262/#sec-date.prototype.setfullyear)
    pub fn set_full_year(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let mut date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setFullYear method must be called on Date object",
            );
        };

        let year_arg = get_argument(cx, arguments, 0);
        let year = to_number(cx, year_arg)?.as_number();

        if date_value.is_nan() {
            date_value = 0.0;
        } else {
            date_value = local_time(date_value);
        }

        let month = if arguments.len() >= 2 {
            let month_arg = get_argument(cx, arguments, 1);
            to_number(cx, month_arg)?.as_number()
        } else {
            month_from_time(date_value)
        };

        let date = if arguments.len() >= 3 {
            let date_arg = get_argument(cx, arguments, 2);
            to_number(cx, date_arg)?.as_number()
        } else {
            date_from_time(date_value)
        };

        let new_date =
            time_clip(utc(make_date(make_day(year, month, date), time_within_day(date_value))));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setHours (https://tc39.es/ecma262/#sec-date.prototype.sethours)
    pub fn set_hours(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.setHours method must be called on Date object");
        };

        let hours_arg = get_argument(cx, arguments, 0);
        let hours = to_number(cx, hours_arg)?.as_number();

        let has_minutes = arguments.len() >= 2;
        let mut minutes = 0.0;

        let has_seconds = arguments.len() >= 3;
        let mut seconds = 0.0;

        let has_milliseconds = arguments.len() >= 4;
        let mut milliseconds = 0.0;

        if has_minutes {
            let minutes_arg = get_argument(cx, arguments, 1);
            minutes = to_number(cx, minutes_arg)?.as_number();
        }

        if has_seconds {
            let seconds_arg = get_argument(cx, arguments, 2);
            seconds = to_number(cx, seconds_arg)?.as_number();
        }

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 3);
            milliseconds = to_number(cx, milliseconds_arg)?.as_number();
        }

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let date_value = local_time(date_value);

        if !has_minutes {
            minutes = minute_from_time(date_value);
        }

        if !has_seconds {
            seconds = second_from_time(date_value);
        }

        if !has_milliseconds {
            milliseconds = millisecond_from_time(date_value);
        }

        let new_date = time_clip(utc(make_date(
            day(date_value),
            make_time(hours, minutes, seconds, milliseconds),
        )));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setMilliseconds (https://tc39.es/ecma262/#sec-date.prototype.setmilliseconds)
    pub fn set_milliseconds(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setMilliseconds method must be called on Date object",
            );
        };

        let milliseconds_arg = get_argument(cx, arguments, 0);
        let milliseconds = to_number(cx, milliseconds_arg)?.as_number();

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let date_value = local_time(date_value);

        let new_date = time_clip(utc(make_date(
            day(date_value),
            make_time(
                hour_from_time(date_value),
                minute_from_time(date_value),
                second_from_time(date_value),
                milliseconds,
            ),
        )));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setMinutes (https://tc39.es/ecma262/#sec-date.prototype.setminutes)
    pub fn set_minutes(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setMinutes method must be called on Date object",
            );
        };

        let minutes_arg = get_argument(cx, arguments, 0);
        let minutes = to_number(cx, minutes_arg)?.as_number();

        let has_seconds = arguments.len() >= 2;
        let mut seconds = 0.0;

        let has_milliseconds = arguments.len() >= 3;
        let mut milliseconds = 0.0;

        if has_seconds {
            let seconds_arg = get_argument(cx, arguments, 1);
            seconds = to_number(cx, seconds_arg)?.as_number();
        }

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 2);
            milliseconds = to_number(cx, milliseconds_arg)?.as_number();
        }

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let date_value = local_time(date_value);

        if !has_seconds {
            seconds = second_from_time(date_value);
        }

        if !has_milliseconds {
            milliseconds = millisecond_from_time(date_value);
        }

        let new_date = time_clip(utc(make_date(
            day(date_value),
            make_time(hour_from_time(date_value), minutes, seconds, milliseconds),
        )));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setMonth (https://tc39.es/ecma262/#sec-date.prototype.setmonth)
    pub fn set_month(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.setMonth method must be called on Date object");
        };

        let month_arg = get_argument(cx, arguments, 0);
        let month = to_number(cx, month_arg)?.as_number();

        let has_date = arguments.len() >= 2;
        let mut date = 1.0;

        if has_date {
            let date_arg = get_argument(cx, arguments, 1);
            date = to_number(cx, date_arg)?.as_number();
        }

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let date_value = local_time(date_value);

        if !has_date {
            date = date_from_time(date_value);
        }

        let new_date = time_clip(utc(make_date(
            make_day(year_from_time(date_value), month, date),
            time_within_day(date_value),
        )));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setSeconds (https://tc39.es/ecma262/#sec-date.prototype.setseconds)
    pub fn set_seconds(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setSeconds method must be called on Date object",
            );
        };

        let seconds_arg = get_argument(cx, arguments, 0);
        let seconds = to_number(cx, seconds_arg)?.as_number();

        let has_milliseconds = arguments.len() >= 2;
        let mut milliseconds = 0.0;

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 1);
            milliseconds = to_number(cx, milliseconds_arg)?.as_number();
        }

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let date_value = local_time(date_value);

        if !has_milliseconds {
            milliseconds = millisecond_from_time(date_value);
        }

        let new_date = time_clip(utc(make_date(
            day(date_value),
            make_time(
                hour_from_time(date_value),
                minute_from_time(date_value),
                seconds,
                milliseconds,
            ),
        )));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setTime (https://tc39.es/ecma262/#sec-date.prototype.settime)
    pub fn set_time(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if this_date_value(this_value).is_none() {
            return type_error(cx, "Date.prototype.setTime method must be called on Date object");
        };

        let time_arg = get_argument(cx, arguments, 0);
        let time_num = time_clip(to_number(cx, time_arg)?.as_number());

        set_date_value(this_value, time_num);

        Ok(Value::from(time_num).to_handle(cx))
    }

    /// Date.prototype.setUTCDate (https://tc39.es/ecma262/#sec-date.prototype.setutcdate)
    pub fn set_utc_date(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setUTCDate method must be called on Date object",
            );
        };

        let date_arg = get_argument(cx, arguments, 0);
        let date = to_number(cx, date_arg)?.as_number();

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let new_date = time_clip(make_date(
            make_day(year_from_time(date_value), month_from_time(date_value), date),
            time_within_day(date_value),
        ));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setUTCFullYear (https://tc39.es/ecma262/#sec-date.prototype.setutcfullyear)
    pub fn set_utc_full_year(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let mut date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setUTCFullYear method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            date_value = 0.0;
        }

        let year_arg = get_argument(cx, arguments, 0);
        let year = to_number(cx, year_arg)?.as_number();

        let month = if arguments.len() >= 2 {
            let month_arg = get_argument(cx, arguments, 1);
            to_number(cx, month_arg)?.as_number()
        } else {
            month_from_time(date_value)
        };

        let date = if arguments.len() >= 3 {
            let date_arg = get_argument(cx, arguments, 2);
            to_number(cx, date_arg)?.as_number()
        } else {
            date_from_time(date_value)
        };

        let new_date =
            time_clip(make_date(make_day(year, month, date), time_within_day(date_value)));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setUTCHours (https://tc39.es/ecma262/#sec-date.prototype.setutchours)
    pub fn set_utc_hours(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setUTCHours method must be called on Date object",
            );
        };

        let hours_arg = get_argument(cx, arguments, 0);
        let hours = to_number(cx, hours_arg)?.as_number();

        let has_minutes = arguments.len() >= 2;
        let mut minutes = 0.0;

        let has_seconds = arguments.len() >= 3;
        let mut seconds = 0.0;

        let has_milliseconds = arguments.len() >= 4;
        let mut milliseconds = 0.0;

        if has_minutes {
            let minutes_arg = get_argument(cx, arguments, 1);
            minutes = to_number(cx, minutes_arg)?.as_number();
        }

        if has_seconds {
            let seconds_arg = get_argument(cx, arguments, 2);
            seconds = to_number(cx, seconds_arg)?.as_number();
        }

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 3);
            milliseconds = to_number(cx, milliseconds_arg)?.as_number();
        }

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        if !has_minutes {
            minutes = minute_from_time(date_value);
        }

        if !has_seconds {
            seconds = second_from_time(date_value);
        }

        if !has_milliseconds {
            milliseconds = millisecond_from_time(date_value);
        }

        let new_date =
            time_clip(make_date(day(date_value), make_time(hours, minutes, seconds, milliseconds)));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setUTCMilliseconds (https://tc39.es/ecma262/#sec-date.prototype.setutcmilliseconds)
    pub fn set_utc_milliseconds(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setUTCMilliseconds method must be called on Date object",
            );
        };

        let milliseconds_arg = get_argument(cx, arguments, 0);
        let milliseconds = to_number(cx, milliseconds_arg)?.as_number();

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        let new_date = time_clip(make_date(
            day(date_value),
            make_time(
                hour_from_time(date_value),
                minute_from_time(date_value),
                second_from_time(date_value),
                milliseconds,
            ),
        ));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setUTCMinutes (https://tc39.es/ecma262/#sec-date.prototype.setutcminutes)
    pub fn set_utc_minutes(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setUTCMinutes method must be called on Date object",
            );
        };

        let minutes_arg = get_argument(cx, arguments, 0);
        let minutes = to_number(cx, minutes_arg)?.as_number();

        let has_seconds = arguments.len() >= 2;
        let mut seconds = 0.0;

        let has_milliseconds = arguments.len() >= 3;
        let mut milliseconds = 0.0;

        if has_seconds {
            let seconds_arg = get_argument(cx, arguments, 1);
            seconds = to_number(cx, seconds_arg)?.as_number();
        }

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 2);
            milliseconds = to_number(cx, milliseconds_arg)?.as_number();
        }

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        if !has_seconds {
            seconds = second_from_time(date_value);
        }

        if !has_milliseconds {
            milliseconds = millisecond_from_time(date_value);
        }

        let new_date = time_clip(make_date(
            day(date_value),
            make_time(hour_from_time(date_value), minutes, seconds, milliseconds),
        ));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setUTCMonth (https://tc39.es/ecma262/#sec-date.prototype.setutcmonth)
    pub fn set_utc_month(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setUTCMonth method must be called on Date object",
            );
        };

        let month_arg = get_argument(cx, arguments, 0);
        let month = to_number(cx, month_arg)?.as_number();

        let has_date = arguments.len() >= 2;
        let mut date = 1.0;

        if has_date {
            let date_arg = get_argument(cx, arguments, 1);
            date = to_number(cx, date_arg)?.as_number();
        }

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        if !has_date {
            date = date_from_time(date_value);
        }

        let new_date = time_clip(make_date(
            make_day(year_from_time(date_value), month, date),
            time_within_day(date_value),
        ));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.setUTCSeconds (https://tc39.es/ecma262/#sec-date.prototype.setutcseconds)
    pub fn set_utc_seconds(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.setUTCSeconds method must be called on Date object",
            );
        };

        let seconds_arg = get_argument(cx, arguments, 0);
        let seconds = to_number(cx, seconds_arg)?.as_number();

        let has_milliseconds = arguments.len() >= 2;
        let mut milliseconds = 0.0;

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 1);
            milliseconds = to_number(cx, milliseconds_arg)?.as_number();
        }

        if date_value.is_nan() {
            return Ok(cx.nan());
        }

        if !has_milliseconds {
            milliseconds = millisecond_from_time(date_value);
        }

        let new_date = time_clip(make_date(
            day(date_value),
            make_time(
                hour_from_time(date_value),
                minute_from_time(date_value),
                seconds,
                milliseconds,
            ),
        ));

        set_date_value(this_value, new_date);

        Ok(Value::from(new_date).to_handle(cx))
    }

    /// Date.prototype.toDateString (https://tc39.es/ecma262/#sec-date.prototype.todatestring)
    pub fn to_date_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.toDateString method must be called on Date object",
            );
        };

        Self::to_date_string_shared(cx, date_value)
    }

    fn to_date_string_shared(mut cx: Context, date_value: f64) -> EvalResult<Handle<Value>> {
        if date_value.is_nan() {
            return Ok(cx.alloc_string("Invalid Date").as_value());
        }

        let date_value = local_time(date_value);

        let mut string = String::new();
        date_string(&mut string, date_value);

        Ok(cx.alloc_string(&string).as_value())
    }

    /// Date.prototype.toISOString (https://tc39.es/ecma262/#sec-date.prototype.toisostring)
    pub fn to_iso_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.toISOString method must be called on Date object",
            );
        };

        if !date_value.is_finite() {
            return range_error(cx, "Date value is not finite");
        }

        let year = year_from_time(date_value) as i64;
        let year_string = if (0..=9999).contains(&year) {
            format!("{year:04}")
        } else {
            let year_sign = if year.is_positive() { '+' } else { '-' };

            format!("{}{:06}", year_sign, year.abs())
        };

        let string = format!(
            "{}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
            year_string,
            month_from_time(date_value) as i64 + 1, // 1-indexed
            date_from_time(date_value) as i64,      // 1-indexed
            hour_from_time(date_value) as i64,
            minute_from_time(date_value) as i64,
            second_from_time(date_value) as i64,
            millisecond_from_time(date_value) as i64
        );

        Ok(cx.alloc_string(&string).as_value())
    }

    /// Date.prototype.toJSON (https://tc39.es/ecma262/#sec-date.prototype.tojson)
    pub fn to_json(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;

        let time_value = to_primitive(cx, object.into(), ToPrimitivePreferredType::Number)?;

        if time_value.is_number() && !time_value.as_number().is_finite() {
            return Ok(cx.null());
        }

        invoke(cx, object.into(), cx.names.to_iso_string(), &[])
    }

    /// Date.prototype.toLocaleDateString (https://tc39.es/ecma262/#sec-date.prototype.tolocaledatestring)
    pub fn to_locale_date_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.toLocaleDateString method must be called on Date object",
            );
        };

        Self::to_date_string_shared(cx, date_value)
    }

    /// Date.prototype.toLocaleString (https://tc39.es/ecma262/#sec-date.prototype.tolocalestring)
    pub fn to_locale_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.toLocaleString method must be called on Date object",
            );
        };

        Ok(to_date_string(cx, date_value).as_value())
    }

    /// Date.prototype.toLocaleTimeString (https://tc39.es/ecma262/#sec-date.prototype.tolocaletimestring)
    pub fn to_locale_time_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.toLocaleTimeString method must be called on Date object",
            );
        };

        Self::to_time_string_shared(cx, date_value)
    }

    /// Date.prototype.toString (https://tc39.es/ecma262/#sec-date.prototype.tostring)
    pub fn to_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.toString method must be called on Date object");
        };

        Ok(to_date_string(cx, date_value).as_value())
    }

    /// Date.prototype.toTimeString (https://tc39.es/ecma262/#sec-date.prototype.totimestring)
    pub fn to_time_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.toTimeString method must be called on Date object",
            );
        };

        Self::to_time_string_shared(cx, date_value)
    }

    fn to_time_string_shared(mut cx: Context, date_value: f64) -> EvalResult<Handle<Value>> {
        if date_value.is_nan() {
            return Ok(cx.alloc_string("Invalid Date").as_value());
        }

        let local_date_value = local_time(date_value);

        let mut string = String::new();

        time_string(&mut string, local_date_value);
        time_zone_string(&mut string, date_value);

        Ok(cx.alloc_string(&string).as_value())
    }

    /// Date.prototype.toUTCString (https://tc39.es/ecma262/#sec-date.prototype.toutcstring)
    pub fn to_utc_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(
                cx,
                "Date.prototype.toUTCString method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Ok(cx.alloc_string("Invalid Date").as_value());
        }

        let year = year_from_time(date_value);
        let year_sign = if year.is_sign_negative() { "-" } else { "" };

        let mut string = format!(
            "{}, {:02} {} {}{:04} ",
            week_day_string(date_value),
            date_from_time(date_value) as i64,
            month_string(date_value),
            year_sign,
            year.abs() as i64,
        );

        time_string(&mut string, date_value);

        Ok(cx.alloc_string(&string).as_value())
    }

    /// Date.prototype.valueOf (https://tc39.es/ecma262/#sec-date.prototype.valueof)
    pub fn value_of(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error(cx, "Date.prototype.valueOf method must be called on Date object");
        };

        Ok(Value::from(date_value).to_handle(cx))
    }

    /// Date.prototype [ @@toPrimitive ] (https://tc39.es/ecma262/#sec-date.prototype-%symbol.toprimitive%)
    pub fn to_primitive(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Date.prototype[@@toPrimitive] must be called on object");
        }

        let hint = get_argument(cx, arguments, 0);
        if hint.is_string() {
            let hint = hint.as_string().flatten();
            if *hint == cx.names.default.as_string().as_flat()
                || *hint == cx.names.string_.as_string().as_flat()
            {
                return ordinary_to_primitive(
                    cx,
                    this_value.as_object(),
                    ToPrimitivePreferredType::String,
                );
            } else if *hint == cx.names.number_.as_string().as_flat() {
                return ordinary_to_primitive(
                    cx,
                    this_value.as_object(),
                    ToPrimitivePreferredType::Number,
                );
            }
        }

        type_error(cx, "Invalid hint to Date.prototype[@@toPrimitive]")
    }
}

/// TimeString (https://tc39.es/ecma262/#sec-timestring)
fn time_string(string: &mut String, time_value: f64) {
    string.push_str(&format!(
        "{:02}:{:02}:{:02} GMT",
        hour_from_time(time_value),
        minute_from_time(time_value),
        second_from_time(time_value),
    ));
}

/// DateString (https://tc39.es/ecma262/#sec-datestring)
fn date_string(string: &mut String, time_value: f64) {
    let year = year_from_time(time_value);
    let year_sign = if year.is_sign_negative() { "-" } else { "" };

    string.push_str(&format!(
        "{} {} {:02} {}{:04}",
        week_day_string(time_value),
        month_string(time_value),
        date_from_time(time_value) as i64,
        year_sign,
        year.abs() as i64,
    ));
}

fn week_day_string(time_value: f64) -> &'static str {
    match week_day(time_value) as i64 {
        0 => "Sun",
        1 => "Mon",
        2 => "Tue",
        3 => "Wed",
        4 => "Thu",
        5 => "Fri",
        6 => "Sat",
        _ => unreachable!("Invalid week day"),
    }
}

fn month_string(time_value: f64) -> &'static str {
    match month_from_time(time_value) as i64 {
        0 => "Jan",
        1 => "Feb",
        2 => "Mar",
        3 => "Apr",
        4 => "May",
        5 => "Jun",
        6 => "Jul",
        7 => "Aug",
        8 => "Sep",
        9 => "Oct",
        10 => "Nov",
        11 => "Dec",
        _ => unreachable!("Invalid month"),
    }
}

/// TimeZoneString (https://tc39.es/ecma262/#sec-timezoneestring)
fn time_zone_string(string: &mut String, _time_value: f64) {
    // TODO: Handle time zones
    let offset: f64 = 0.0;

    let sign = if offset.is_sign_positive() { '+' } else { '-' };
    let offset = offset.abs();

    let hour = hour_from_time(offset);
    let minute = minute_from_time(offset);

    string.push(sign);
    string.push_str(&format!("{hour:02}{minute:02}"));
}

/// ToDateString (https://tc39.es/ecma262/#sec-todatestring)
pub fn to_date_string(mut cx: Context, time_value: f64) -> Handle<StringValue> {
    if time_value.is_nan() {
        return cx.alloc_string("Invalid Date").as_string();
    }

    let local_time_value = local_time(time_value);

    let mut string = String::new();

    date_string(&mut string, local_time_value);
    string.push(' ');
    time_string(&mut string, local_time_value);
    time_zone_string(&mut string, time_value);

    cx.alloc_string(&string).as_string()
}

#[inline]
pub fn this_date_value(value: Handle<Value>) -> Option<f64> {
    if !value.is_object() {
        return None;
    }

    value
        .as_object()
        .as_date_object()
        .map(|date| date.date_value())
}

#[inline]
fn set_date_value(date: Handle<Value>, date_value: f64) {
    date.as_object()
        .cast::<DateObject>()
        .set_date_value(date_value);
}
