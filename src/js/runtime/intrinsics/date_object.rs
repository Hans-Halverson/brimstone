use std::mem::size_of;

use crate::{
    common::math::modulo,
    extend_object,
    runtime::{
        gc::HeapObject, intrinsics::intrinsics::Intrinsic, object_descriptor::ObjectKind,
        object_value::ObjectValue, ordinary_object::object_create_from_constructor,
        type_utilities::to_integer_or_infinity_f64, Context, EvalResult, Handle, HeapPtr,
    },
    set_uninit,
};

// Date Objects (https://tc39.es/ecma262/#sec-date-objects)
extend_object! {
    pub struct DateObject {
        // The instant in time (as milliseconds since the Unix epoch). May also be NaN to represent
        // no specific instant.
        date_value: f64,
    }
}

impl DateObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        date_value: f64,
    ) -> EvalResult<HeapPtr<DateObject>> {
        let mut object = object_create_from_constructor::<DateObject>(
            cx,
            constructor,
            ObjectKind::DateObject,
            Intrinsic::DatePrototype,
        )?;

        set_uninit!(object.date_value, date_value);

        Ok(object)
    }

    pub fn date_value(&self) -> f64 {
        self.date_value
    }

    pub fn set_date_value(&mut self, date_value: f64) {
        self.date_value = date_value;
    }
}

/// Time values are capped to this value
pub const MAX_TIME_VALUE: f64 = 8.64e15;

pub const MS_PER_SECOND: f64 = 1000.0;
const SECONDS_PER_MINUTE: f64 = 60.0;
const MINUTES_PER_HOUR: f64 = 60.0;
const HOURS_PER_DAY: f64 = 24.0;

pub const MS_PER_MINUTE: f64 = MS_PER_SECOND * SECONDS_PER_MINUTE;
pub const MS_PER_HOUR: f64 = MS_PER_MINUTE * MINUTES_PER_HOUR;
pub const MS_PER_DAY: f64 = MS_PER_HOUR * HOURS_PER_DAY;

/// Day Number (https://tc39.es/ecma262/#sec-day)
pub fn day(time: f64) -> f64 {
    f64::floor(time / MS_PER_DAY)
}

pub fn time_within_day(time: f64) -> f64 {
    modulo(time, MS_PER_DAY)
}

/// DaysInYear (https://tc39.es/ecma262/#sec-daysinyear)
fn days_in_year(year: i64) -> f64 {
    if is_leap_year(year) {
        366.0
    } else {
        365.0
    }
}

fn day_from_year(year: f64) -> f64 {
    (365.0 * (year - 1970.0)) + f64::floor((year - 1969.0) / 4.0)
        - f64::floor((year - 1901.0) / 100.0)
        + f64::floor((year - 1601.0) / 400.0)
}

fn time_from_year(year: f64) -> f64 {
    MS_PER_DAY * day_from_year(year)
}

pub fn year_from_time(time: f64) -> f64 {
    let day = day(time);

    // Approximate year then find closest year within 1 year of approximate year.
    // Based off the logic in SerenityOS's LibJS.
    let year = f64::floor(day / 365.2425) + 1970.0;

    let year_time = time_from_year(year);
    if year_time > time {
        year - 1.0
    } else if year_time + days_in_year(year as i64) * MS_PER_DAY <= time {
        year + 1.0
    } else {
        year
    }
}

/// MonthFromTime (https://tc39.es/ecma262/#sec-monthfromtime)
pub fn month_from_time(time: f64) -> f64 {
    let year = year_from_time(time);
    let leap_year = if is_leap_year(year as i64) { 1.0 } else { 0.0 };

    let day_within_year = day_within_year(time);

    if (0.0..31.0).contains(&day_within_year) {
        0.0
    } else if 31.0 <= day_within_year && day_within_year < 59.0 + leap_year {
        1.0
    } else if 59.0 + leap_year <= day_within_year && day_within_year < 90.0 + leap_year {
        2.0
    } else if 90.0 + leap_year <= day_within_year && day_within_year < 120.0 + leap_year {
        3.0
    } else if 120.0 + leap_year <= day_within_year && day_within_year < 151.0 + leap_year {
        4.0
    } else if 151.0 + leap_year <= day_within_year && day_within_year < 181.0 + leap_year {
        5.0
    } else if 181.0 + leap_year <= day_within_year && day_within_year < 212.0 + leap_year {
        6.0
    } else if 212.0 + leap_year <= day_within_year && day_within_year < 243.0 + leap_year {
        7.0
    } else if 243.0 + leap_year <= day_within_year && day_within_year < 273.0 + leap_year {
        8.0
    } else if 273.0 + leap_year <= day_within_year && day_within_year < 304.0 + leap_year {
        9.0
    } else if 304.0 + leap_year <= day_within_year && day_within_year < 334.0 + leap_year {
        10.0
    } else if 334.0 + leap_year <= day_within_year && day_within_year < 365.0 + leap_year {
        11.0
    } else {
        unreachable!("invalid day")
    }
}

fn day_within_year(time: f64) -> f64 {
    day(time) - day_from_year(year_from_time(time))
}

/// DateFromTime (https://tc39.es/ecma262/#sec-datefromtime)
pub fn date_from_time(time: f64) -> f64 {
    let month_from_time = month_from_time(time);

    let year = year_from_time(time);
    let leap_year_day = if is_leap_year(year as i64) && month_from_time >= 2.0 {
        1.0
    } else {
        0.0
    };

    const MONTHS_TABLE: [f64; 12] = [
        -1.0,  // January
        30.0,  // February
        58.0,  // March
        89.0,  // April
        119.0, // May
        150.0, // June
        180.0, // July
        211.0, // August
        242.0, // September
        272.0, // October
        303.0, // November
        333.0, // December
    ];

    day_within_year(time) - MONTHS_TABLE[month_from_time as usize] - leap_year_day
}

/// WeekDay (https://tc39.es/ecma262/#sec-weekday)
pub fn week_day(time: f64) -> f64 {
    modulo(day(time) + 4.0, 7.0)
}

/// LocalTime (https://tc39.es/ecma262/#sec-localtime)
pub fn local_time(time: f64) -> f64 {
    // TODO: Handle time zones
    time
}

/// UTC (https://tc39.es/ecma262/#sec-utc-t)
pub fn utc(time: f64) -> f64 {
    // TODO: Handle time zones
    time
}

pub fn hour_from_time(time: f64) -> f64 {
    modulo(f64::floor(time / MS_PER_HOUR), HOURS_PER_DAY)
}

pub fn minute_from_time(time: f64) -> f64 {
    modulo(f64::floor(time / MS_PER_MINUTE), MINUTES_PER_HOUR)
}

pub fn second_from_time(time: f64) -> f64 {
    modulo(f64::floor(time / MS_PER_SECOND), SECONDS_PER_MINUTE)
}

pub fn millisecond_from_time(time: f64) -> f64 {
    modulo(time, MS_PER_SECOND)
}

/// MakeTime (https://tc39.es/ecma262/#sec-maketime)
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

// Floor division - round towards negative infintiy
fn floor_div(a: i64, b: i64) -> i64 {
    if a >= 0 {
        a / b
    } else {
        ((a + 1) / b) - 1
    }
}

fn is_leap_year(year: i64) -> bool {
    year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

// Calculate number of leap years after year 0 but before the given year
fn leap_years_before_year(year: i64) -> i64 {
    let year = year - 1;
    floor_div(year, 4) - floor_div(year, 100) + floor_div(year, 400)
}

// Calculate number of leap years in a time period between two years
fn leap_years_between_years(start_year: i64, end_year: i64) -> i64 {
    leap_years_before_year(end_year) - leap_years_before_year(start_year)
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
    assert!((1..=12).contains(&month));

    // Table to lookup one less than the number of days until the start of each month, excluding
    // leap days.
    const DAYS_TO_MONTH_START: [i64; 12] = [
        -1,  // January
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

    DAYS_TO_MONTH_START[month as usize - 1] + day + leap_day
}

/// Year + month + day to the number of days since the Unix epoch. Months and days are 1-indexed.
/// Month must be between 1 and 12, day is not constrained.
pub fn year_month_day_to_days_since_unix_epoch(year: i64, month: i64, day: i64) -> i64 {
    year_to_days_since_unix_epoch(year) + year_month_day_to_days_since_year_start(year, month, day)
}

/// MakeDay (https://tc39.es/ecma262/#sec-makeday)
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

    // Separate out the month in the calculated year. Compute modulus according to spec.
    let calculated_month = modulo(month, 12.0);

    // TODO: Handle lossy casts
    let calculated_year = calculated_year as i64;
    let calculated_month = calculated_month as i64 + 1;

    let num_days_until_month_start =
        year_month_day_to_days_since_unix_epoch(calculated_year, calculated_month, 1) as f64;

    num_days_until_month_start + date - 1.0
}

/// MakeDate (https://tc39.es/ecma262/#sec-makedate)
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

/// TimeClip (https://tc39.es/ecma262/#sec-timeclip)
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

    fn visit_pointers(&mut self, visitor: &mut impl crate::runtime::gc::HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
