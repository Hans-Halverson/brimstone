use crate::js::runtime::{
    error::type_error_, object_value::ObjectValue, Context, EvalResult, Handle, Realm, Value,
};

use super::{
    date_object::{
        date_from_time, hour_from_time, local_time, millisecond_from_time, minute_from_time,
        month_from_time, second_from_time, week_day, year_from_time, DateObject, MS_PER_MINUTE,
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
