/*---
description: Temporal.PlainDateTime.from errors for out of bounds time and date fields.
---*/

// Hour out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, hour: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, hour: 24 }, { overflow: 'reject' }));

// Minute out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, minute: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, minute: 60 }, { overflow: 'reject' }));

// Second out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, second: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, second: 60 }, { overflow: 'reject' }));

// Millisecond out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, millisecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, millisecond: 1000 }, { overflow: 'reject' }));

// Microsecond out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, microsecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, microsecond: 1000 }, { overflow: 'reject' }));

// Nanosecond out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, nanosecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 1, nanosecond: 1000 }, { overflow: 'reject' }));

// Day out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 0 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 1, day: 32 }, { overflow: 'reject' }));

// Month out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 0, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: -1, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 2020, month: 13, day: 1 }, { overflow: 'reject' }));

// Year out of range
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: -271822, month: 12, day: 31 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: -999999, month: 1, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 275761, month: 1, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDateTime.from({ year: 999999, month: 1, day: 1 }, { overflow: 'reject' }));

// Valid minimum bounds
const minFields = Temporal.PlainDateTime.from(
    { year: 2020, month: 1, day: 1, hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 },
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

const minYear = Temporal.PlainDateTime.from({ year: -271821, month: 12, day: 31 }, { overflow: 'reject' });
assert.sameValue(minYear.year, -271821);

// Valid maximum bounds
const maxFields = Temporal.PlainDateTime.from(
    { year: 2020, month: 12, day: 31, hour: 23, minute: 59, second: 59, millisecond: 999, microsecond: 999, nanosecond: 999 },
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

const maxYear = Temporal.PlainDateTime.from({ year: 275760, month: 1, day: 1 }, { overflow: 'reject' });
assert.sameValue(maxYear.year, 275760);
