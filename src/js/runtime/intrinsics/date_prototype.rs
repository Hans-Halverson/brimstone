use crate::{
    js::runtime::{
        error::type_error_,
        function::get_argument,
        intrinsics::date_object::{day, make_date, make_time, time_clip},
        object_value::ObjectValue,
        type_utilities::to_number,
        Context, EvalResult, Handle, Realm, Value,
    },
    maybe,
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
    // 21.4.4 Properties of the Date Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
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
        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        object
    }

    // 21.4.4.2 Date.prototype.getDate
    fn get_date(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.getDate method must be called on Date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let date = date_from_time(local_time(date_value));

        Value::from(date).to_handle(cx).into()
    }

    // 21.4.4.3 Date.prototype.getDay
    fn get_day(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.getDay method must be called on Date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let day = week_day(local_time(date_value));

        Value::from(day).to_handle(cx).into()
    }

    // 21.4.4.4 Date.prototype.getFullYear
    fn get_full_year(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.getFullYear method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let year = year_from_time(local_time(date_value));

        Value::from(year).to_handle(cx).into()
    }

    // 21.4.4.5 Date.prototype.getHours
    fn get_hours(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.getHours method must be called on Date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let hour = hour_from_time(local_time(date_value));

        Value::from(hour).to_handle(cx).into()
    }

    // 21.4.4.6 Date.prototype.getMilliseconds
    fn get_milliseconds(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.getMilliseconds method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let millisecond = millisecond_from_time(local_time(date_value));

        Value::from(millisecond).to_handle(cx).into()
    }

    // 21.4.4.7 Date.prototype.getMinutes
    fn get_minutes(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.getMinutes method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let minute = minute_from_time(local_time(date_value));

        Value::from(minute).to_handle(cx).into()
    }

    // 21.4.4.8 Date.prototype.getMonth
    fn get_month(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.getMonth method must be called on Date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let month = month_from_time(local_time(date_value));

        Value::from(month).to_handle(cx).into()
    }

    // 21.4.4.9 Date.prototype.getSeconds
    fn get_seconds(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.getSeconds method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let second = second_from_time(local_time(date_value));

        Value::from(second).to_handle(cx).into()
    }

    // 21.4.4.10 Date.prototype.getTime
    fn get_time(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if let Some(date_value) = this_date_value(this_value) {
            Value::from(date_value).to_handle(cx).into()
        } else {
            type_error_(cx, "getTime method must be called on date object")
        }
    }

    // 21.4.4.11 Date.prototype.getTimezoneOffset
    fn get_timezone_offset(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "getTimezeonOffset method must be called on date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let timezone_offset = (date_value - local_time(date_value)) / MS_PER_MINUTE;

        Value::from(timezone_offset).to_handle(cx).into()
    }

    // 21.4.4.12 Date.prototype.getUTCDate
    fn get_utc_date(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.getUTCDate must be called on Date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let date = date_from_time(date_value);

        Value::from(date).to_handle(cx).into()
    }

    // 21.4.4.13 Date.prototype.getUTCDay
    fn get_utc_day(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.getUTCDay must be called on Date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let hour = week_day(date_value);

        Value::from(hour).to_handle(cx).into()
    }

    // 21.4.4.14 Date.prototype.getUTCFullYear
    fn get_utc_full_year(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.getUTCFullYear must be called on Date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let year = year_from_time(date_value);

        Value::from(year).to_handle(cx).into()
    }

    // 21.4.4.15 Date.prototype.getUTCHours
    fn get_utc_hours(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.getUTCHours must be called on Date object");
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let hour = hour_from_time(date_value);

        Value::from(hour).to_handle(cx).into()
    }

    // 21.4.4.16 Date.prototype.getUTCMilliseconds
    fn get_utc_milliseconds(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.getUTCMilliseconds method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let millisecond = millisecond_from_time(date_value);

        Value::from(millisecond).to_handle(cx).into()
    }

    // 21.4.4.17 Date.prototype.getUTCMinutes
    fn get_utc_minutes(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.getUTCMinutes method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let minute = minute_from_time(date_value);

        Value::from(minute).to_handle(cx).into()
    }

    // 21.4.4.18 Date.prototype.getUTCMonth
    fn get_utc_month(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.getUTCMonth method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let month = month_from_time(date_value);

        Value::from(month).to_handle(cx).into()
    }

    // 21.4.4.19 Date.prototype.getUTCSeconds
    fn get_utc_seconds(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.getUTCSeconds method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let second = second_from_time(date_value);

        Value::from(second).to_handle(cx).into()
    }

    // 21.4.4.20 Date.prototype.setDate
    fn set_date(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.setDate method must be called on Date object");
        };

        let date_arg = get_argument(cx, arguments, 0);
        let date = maybe!(to_number(cx, date_arg)).as_number();

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let date_value = local_time(date_value);

        let new_date = time_clip(utc(make_date(
            make_day(year_from_time(date_value), month_from_time(date_value), date),
            time_within_day(date_value),
        )));

        set_date_value(this_value, new_date);

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.21 Date.prototype.setFullYear
    fn set_full_year(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setFullYear method must be called on Date object",
            );
        };

        let year_arg = get_argument(cx, arguments, 0);
        let year = maybe!(to_number(cx, year_arg)).as_number();

        if date_value.is_nan() {
            date_value = 0.0;
        } else {
            date_value = local_time(date_value);
        }

        let month = if arguments.len() >= 2 {
            let month_arg = get_argument(cx, arguments, 1);
            maybe!(to_number(cx, month_arg)).as_number()
        } else {
            month_from_time(date_value)
        };

        let date = if arguments.len() >= 3 {
            let date_arg = get_argument(cx, arguments, 2);
            maybe!(to_number(cx, date_arg)).as_number()
        } else {
            date_from_time(date_value)
        };

        let new_date =
            time_clip(utc(make_date(make_day(year, month, date), time_within_day(date_value))));

        set_date_value(this_value, new_date);

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.22 Date.prototype.setHours
    fn set_hours(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.setHours method must be called on Date object");
        };

        let hours_arg = get_argument(cx, arguments, 0);
        let hours = maybe!(to_number(cx, hours_arg)).as_number();

        let has_minutes = arguments.len() >= 2;
        let mut minutes = 0.0;

        let has_seconds = arguments.len() >= 3;
        let mut seconds = 0.0;

        let has_milliseconds = arguments.len() >= 4;
        let mut milliseconds = 0.0;

        if has_minutes {
            let minutes_arg = get_argument(cx, arguments, 1);
            minutes = maybe!(to_number(cx, minutes_arg)).as_number();
        }

        if has_seconds {
            let seconds_arg = get_argument(cx, arguments, 2);
            seconds = maybe!(to_number(cx, seconds_arg)).as_number();
        }

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 3);
            milliseconds = maybe!(to_number(cx, milliseconds_arg)).as_number();
        }

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.23 Date.prototype.setMilliseconds
    fn set_milliseconds(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setMilliseconds method must be called on Date object",
            );
        };

        let milliseconds_arg = get_argument(cx, arguments, 0);
        let milliseconds = maybe!(to_number(cx, milliseconds_arg)).as_number();

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.24 Date.prototype.setMinutes
    fn set_minutes(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setMinutes method must be called on Date object",
            );
        };

        let minutes_arg = get_argument(cx, arguments, 0);
        let minutes = maybe!(to_number(cx, minutes_arg)).as_number();

        let has_seconds = arguments.len() >= 2;
        let mut seconds = 0.0;

        let has_milliseconds = arguments.len() >= 3;
        let mut milliseconds = 0.0;

        if has_seconds {
            let seconds_arg = get_argument(cx, arguments, 1);
            seconds = maybe!(to_number(cx, seconds_arg)).as_number();
        }

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 2);
            milliseconds = maybe!(to_number(cx, milliseconds_arg)).as_number();
        }

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.25 Date.prototype.setMonth
    fn set_month(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.setMonth method must be called on Date object");
        };

        let month_arg = get_argument(cx, arguments, 0);
        let month = maybe!(to_number(cx, month_arg)).as_number();

        let has_date = arguments.len() >= 2;
        let mut date = 1.0;

        if has_date {
            let date_arg = get_argument(cx, arguments, 1);
            date = maybe!(to_number(cx, date_arg)).as_number();
        }

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.26 Date.prototype.setSeconds
    fn set_seconds(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setSeconds method must be called on Date object",
            );
        };

        let seconds_arg = get_argument(cx, arguments, 0);
        let seconds = maybe!(to_number(cx, seconds_arg)).as_number();

        let has_milliseconds = arguments.len() >= 2;
        let mut milliseconds = 0.0;

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 1);
            milliseconds = maybe!(to_number(cx, milliseconds_arg)).as_number();
        }

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.27 Date.prototype.setTime
    fn set_time(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if this_date_value(this_value).is_none() {
            return type_error_(cx, "Date.prototype.setTime method must be called on Date object");
        };

        let time_arg = get_argument(cx, arguments, 0);
        let time_num = time_clip(maybe!(to_number(cx, time_arg)).as_number());

        set_date_value(this_value, time_num);

        Value::from(time_num).to_handle(cx).into()
    }

    // 21.4.4.28 Date.prototype.setUTCDate
    fn set_utc_date(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setUTCDate method must be called on Date object",
            );
        };

        let date_arg = get_argument(cx, arguments, 0);
        let date = maybe!(to_number(cx, date_arg)).as_number();

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        let new_date = time_clip(make_date(
            make_day(year_from_time(date_value), month_from_time(date_value), date),
            time_within_day(date_value),
        ));

        set_date_value(this_value, new_date);

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.29 Date.prototype.setUTCFullYear
    fn set_utc_full_year(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setUTCFullYear method must be called on Date object",
            );
        };

        if date_value.is_nan() {
            date_value = 0.0;
        }

        let year_arg = get_argument(cx, arguments, 0);
        let year = maybe!(to_number(cx, year_arg)).as_number();

        let month = if arguments.len() >= 2 {
            let month_arg = get_argument(cx, arguments, 1);
            maybe!(to_number(cx, month_arg)).as_number()
        } else {
            month_from_time(date_value)
        };

        let date = if arguments.len() >= 3 {
            let date_arg = get_argument(cx, arguments, 2);
            maybe!(to_number(cx, date_arg)).as_number()
        } else {
            date_from_time(date_value)
        };

        let new_date =
            time_clip(make_date(make_day(year, month, date), time_within_day(date_value)));

        set_date_value(this_value, new_date);

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.30 Date.prototype.setUTCHours
    fn set_utc_hours(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setUTCHours method must be called on Date object",
            );
        };

        let hours_arg = get_argument(cx, arguments, 0);
        let hours = maybe!(to_number(cx, hours_arg)).as_number();

        let has_minutes = arguments.len() >= 2;
        let mut minutes = 0.0;

        let has_seconds = arguments.len() >= 3;
        let mut seconds = 0.0;

        let has_milliseconds = arguments.len() >= 4;
        let mut milliseconds = 0.0;

        if has_minutes {
            let minutes_arg = get_argument(cx, arguments, 1);
            minutes = maybe!(to_number(cx, minutes_arg)).as_number();
        }

        if has_seconds {
            let seconds_arg = get_argument(cx, arguments, 2);
            seconds = maybe!(to_number(cx, seconds_arg)).as_number();
        }

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 3);
            milliseconds = maybe!(to_number(cx, milliseconds_arg)).as_number();
        }

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.31 Date.prototype.setUTCMilliseconds
    fn set_utc_milliseconds(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setUTCMilliseconds method must be called on Date object",
            );
        };

        let milliseconds_arg = get_argument(cx, arguments, 0);
        let milliseconds = maybe!(to_number(cx, milliseconds_arg)).as_number();

        if milliseconds.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.32 Date.prototype.setUTCMinutes
    fn set_utc_minutes(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setUTCMinutes method must be called on Date object",
            );
        };

        let minutes_arg = get_argument(cx, arguments, 0);
        let minutes = maybe!(to_number(cx, minutes_arg)).as_number();

        let has_seconds = arguments.len() >= 2;
        let mut seconds = 0.0;

        let has_milliseconds = arguments.len() >= 3;
        let mut milliseconds = 0.0;

        if has_seconds {
            let seconds_arg = get_argument(cx, arguments, 1);
            seconds = maybe!(to_number(cx, seconds_arg)).as_number();
        }

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 2);
            milliseconds = maybe!(to_number(cx, milliseconds_arg)).as_number();
        }

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.33 Date.prototype.setUTCMonth
    fn set_utc_month(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setUTCMonth method must be called on Date object",
            );
        };

        let month_arg = get_argument(cx, arguments, 0);
        let month = maybe!(to_number(cx, month_arg)).as_number();

        let has_date = arguments.len() >= 2;
        let mut date = 1.0;

        if has_date {
            let date_arg = get_argument(cx, arguments, 1);
            date = maybe!(to_number(cx, date_arg)).as_number();
        }

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
        }

        if !has_date {
            date = date_from_time(date_value);
        }

        let new_date = time_clip(make_date(
            make_day(year_from_time(date_value), month, date),
            time_within_day(date_value),
        ));

        set_date_value(this_value, new_date);

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.34 Date.prototype.setUTCSeconds
    fn set_utc_seconds(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(
                cx,
                "Date.prototype.setUTCSeconds method must be called on Date object",
            );
        };

        let seconds_arg = get_argument(cx, arguments, 0);
        let seconds = maybe!(to_number(cx, seconds_arg)).as_number();

        let has_milliseconds = arguments.len() >= 2;
        let mut milliseconds = 0.0;

        if has_milliseconds {
            let milliseconds_arg = get_argument(cx, arguments, 1);
            milliseconds = maybe!(to_number(cx, milliseconds_arg)).as_number();
        }

        if date_value.is_nan() {
            return Value::nan().to_handle(cx).into();
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

        Value::from(new_date).to_handle(cx).into()
    }

    // 21.4.4.44 Date.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "Date.prototype.valueOf method must be called on Date object");
        };

        Value::from(date_value).to_handle(cx).into()
    }
}

#[inline]
pub fn this_date_value(value: Handle<Value>) -> Option<f64> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_date_object() {
        return None;
    }

    Some(object.cast::<DateObject>().date_value())
}

#[inline]
fn set_date_value(date: Handle<Value>, date_value: f64) {
    date.as_object()
        .cast::<DateObject>()
        .set_date_value(date_value);
}
