/*---
description: Temporal.ZonedDateTime.from errors for out of bounds time and date fields.
---*/

// Hour out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', hour: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', hour: 24 }, { overflow: 'reject' }));

// Minute out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', minute: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', minute: 60 }, { overflow: 'reject' }));

// Second out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', second: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', second: 60 }, { overflow: 'reject' }));

// Millisecond out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', millisecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', millisecond: 1000 }, { overflow: 'reject' }));

// Microsecond out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', microsecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', microsecond: 1000 }, { overflow: 'reject' }));

// Nanosecond out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', nanosecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 1, timeZone: 'UTC', nanosecond: 1000 }, { overflow: 'reject' }));

// Day out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 0, timeZone: 'UTC' }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: -1, timeZone: 'UTC' }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 1, day: 32, timeZone: 'UTC' }, { overflow: 'reject' }));

// Month out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 0, day: 1, timeZone: 'UTC' }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: -1, day: 1, timeZone: 'UTC' }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 2020, month: 13, day: 1, timeZone: 'UTC' }, { overflow: 'reject' }));

// Year out of range
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: -271822, month: 12, day: 31, timeZone: 'UTC' }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: -999999, month: 1, day: 1, timeZone: 'UTC' }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 275761, month: 1, day: 1, timeZone: 'UTC' }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.ZonedDateTime.from({ year: 999999, month: 1, day: 1, timeZone: 'UTC' }, { overflow: 'reject' }));

// Valid minimum bounds
const minFields = Temporal.ZonedDateTime.from(
    { year: 2020, month: 1, day: 1, timeZone: 'UTC', hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 },
    { overflow: 'reject' }
);
assert.sameValue(minFields.month, 1);
assert.sameValue(minFields.day, 1);
assert.sameValue(minFields.hour, 0);
assert.sameValue(minFields.minute, 0);
assert.sameValue(minFields.second, 0);
assert.sameValue(minFields.millisecond, 0);
assert.sameValue(minFields.microsecond, 0);
assert.sameValue(minFields.nanosecond, 0);

const minYear = Temporal.ZonedDateTime.from({ year: -271821, month: 12, day: 31, timeZone: 'UTC' }, { overflow: 'reject' });
assert.sameValue(minYear.year, -271821);

// Valid maximum bounds
const maxFields = Temporal.ZonedDateTime.from(
    { year: 2020, month: 12, day: 31, timeZone: 'UTC', hour: 23, minute: 59, second: 59, millisecond: 999, microsecond: 999, nanosecond: 999 },
    { overflow: 'reject' }
);
assert.sameValue(maxFields.month, 12);
assert.sameValue(maxFields.day, 31);
assert.sameValue(maxFields.hour, 23);
assert.sameValue(maxFields.minute, 59);
assert.sameValue(maxFields.second, 59);
assert.sameValue(maxFields.millisecond, 999);
assert.sameValue(maxFields.microsecond, 999);
assert.sameValue(maxFields.nanosecond, 999);

const maxYear = Temporal.ZonedDateTime.from({ year: 275760, month: 1, day: 1, timeZone: 'UTC' }, { overflow: 'reject' });
assert.sameValue(maxYear.year, 275760);
