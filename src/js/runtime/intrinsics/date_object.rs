use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        gc::HeapObject,
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_from_constructor},
        type_utilities::to_integer_or_infinity_f64,
        Context, EvalResult, Handle, HeapPtr,
    },
    maybe, set_uninit,
};

// 21.4 Date Objects
extend_object! {
    pub struct DateObject {
        // The instant in time (as milliseconds since the Unix epoch). May also be NaN to represent
        // no specific instant.
        date_value: f64,
    }
}

impl DateObject {
    pub fn new(cx: &mut Context, date_value: f64) -> HeapPtr<DateObject> {
        let mut date =
            object_create::<DateObject>(cx, ObjectKind::DateObject, Intrinsic::DatePrototype);

        set_uninit!(date.date_value, date_value);

        date
    }

    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
        date_value: f64,
    ) -> EvalResult<HeapPtr<DateObject>> {
        let mut object = maybe!(object_create_from_constructor::<DateObject>(
            cx,
            constructor,
            ObjectKind::DateObject,
            Intrinsic::DatePrototype
        ));

        set_uninit!(object.date_value, date_value);

        object.into()
    }

    pub fn date_value(&self) -> f64 {
        self.date_value
    }
}

/// Time values are capped to this value
const MAX_TIME_VALUE: f64 = 8.64e15;

const MS_PER_SECOND: f64 = 1000.0;
const SECONDS_PER_MINUTE: f64 = 60.0;
const MINUTES_PER_HOUR: f64 = 60.0;
const HOURS_PER_DAY: f64 = 24.0;

const MS_PER_MINUTE: f64 = MS_PER_SECOND * SECONDS_PER_MINUTE;
const MS_PER_HOUR: f64 = MS_PER_MINUTE * MINUTES_PER_HOUR;
const MS_PER_DAY: f64 = MS_PER_HOUR * HOURS_PER_DAY;

// 21.4.1.11 LocalTime
pub fn local_time(time: f64) -> f64 {
    // TODO: Handle time zones
    time
}

// 21.4.1.12 UTC
pub fn utc(time: f64) -> f64 {
    // TODO: Handle time zones
    time
}

// 21.4.1.14 MakeTime
pub fn make_time(hour: f64, minute: f64, second: f64, millisecond: f64) -> f64 {
    if !hour.is_finite() || !minute.is_finite() || !second.is_finite() || !millisecond.is_finite() {
        return f64::NAN;
    }

    let hour = to_integer_or_infinity_f64(hour);
    let minute = to_integer_or_infinity_f64(minute);
    let second = to_integer_or_infinity_f64(second);
    let millisecond = to_integer_or_infinity_f64(millisecond);

    hour * MS_PER_HOUR + minute * MS_PER_MINUTE + second * MS_PER_SECOND + millisecond
}

fn is_leap_year(year: i64) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

// Calculate number of leap years in a time period between two years
fn leap_years_between_years(start_year: i64, end_year: i64) -> i64 {
    leap_years_before_year(end_year) - leap_years_before_year(start_year)
}

// Calculate number of leap years after year 0 but before the given year
fn leap_years_before_year(year: i64) -> i64 {
    let year = year - 1;
    (year / 4) - (year / 100) + (year / 400)
}

fn year_to_days_since_unix_epoch(year: i64) -> i64 {
    let years_since_epoch = year - 1970;
    if years_since_epoch >= 0 {
        years_since_epoch * 365 + leap_years_between_years(1970, year)
    } else {
        years_since_epoch * 365 - leap_years_between_years(year, 1970)
    }
}

// Months and days are 1-indexed. Month must be between 1 and 12, day is not not constrained.
fn year_month_day_to_days_since_year_start(year: i64, month: i64, day: i64) -> i64 {
    assert!(month >= 1 && month <= 12);

    // Table to lookup one less than the number of days until the start of each month, excluding
    // leap days.
    const DAYS_TO_MONTH_START: [i64; 12] = [
        0,   // January
        30,  // February
        58,  // March
        89,  // April
        119, // May
        150, // June
        180, // July
        211, // August
        242, // September
        272, // October
        303, // November
        333, // December
    ];

    let leap_day = if is_leap_year(year) && month > 2 {
        1
    } else {
        0
    };

    DAYS_TO_MONTH_START[month as usize] + day + leap_day
}

// Year + month + day to the number of days since the Unix epoch. Months and days are 1-indexed.
// Month must be between 1 and 12, day is not constrained.
fn year_month_day_to_days_since_unix_epoch(year: i64, month: i64, day: i64) -> i64 {
    year_to_days_since_unix_epoch(year) + year_month_day_to_days_since_year_start(year, month, day)
}

// 21.4.1.15 MakeDay
pub fn make_day(year: f64, month: f64, date: f64) -> f64 {
    if !year.is_finite() || !month.is_finite() || !date.is_finite() {
        return f64::NAN;
    }

    let year = to_integer_or_infinity_f64(year);
    let month = to_integer_or_infinity_f64(month);
    let date = to_integer_or_infinity_f64(date);

    // Every 12 months is another year
    let calculated_year = year + f64::floor(month / 12.0);
    if !calculated_year.is_finite() {
        return f64::NAN;
    }

    // Separate out the month in the calculated year
    let calculated_month = month % 12.0;

    // TODO: Handle lossy casts
    let calculated_year = calculated_year as i64;
    let calculated_month = calculated_month as i64 + 1;

    let num_days_until_month_start =
        year_month_day_to_days_since_unix_epoch(calculated_year, calculated_month, 1) as f64;

    num_days_until_month_start + date - 1.0
}

// 21.4.1.16 MakeDate
pub fn make_date(day: f64, time: f64) -> f64 {
    if !day.is_finite() || !time.is_finite() {
        return f64::NAN;
    }

    let result = day * MS_PER_DAY + time;
    if !result.is_finite() {
        return f64::NAN;
    }

    result
}

// 21.4.1.17 TimeClip
pub fn time_clip(time: f64) -> f64 {
    if !time.is_finite() {
        return f64::NAN;
    }

    if time.abs() > MAX_TIME_VALUE {
        return f64::NAN;
    }

    to_integer_or_infinity_f64(time)
}

impl HeapObject for HeapPtr<DateObject> {
    fn byte_size(&self) -> usize {
        size_of::<DateObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl crate::js::runtime::gc::HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}
