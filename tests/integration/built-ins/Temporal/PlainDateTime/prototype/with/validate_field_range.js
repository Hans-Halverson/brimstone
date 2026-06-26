/*---
description: Temporal.PlainDateTime.prototype.with errors for out of bounds time and date fields.
---*/

const yearStart = Temporal.PlainDateTime.from('2020-01-01T00:00:00');
const yearEnd   = Temporal.PlainDateTime.from('2020-12-31T00:00:00');

// Hour out of range
assert.throws(RangeError, () => yearStart.with({ hour: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ hour: 24 }, { overflow: 'reject' }));

// Minute out of range
assert.throws(RangeError, () => yearStart.with({ minute: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ minute: 60 }, { overflow: 'reject' }));

// Second out of range
assert.throws(RangeError, () => yearStart.with({ second: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ second: 60 }, { overflow: 'reject' }));

// Millisecond out of range
assert.throws(RangeError, () => yearStart.with({ millisecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ millisecond: 1000 }, { overflow: 'reject' }));

// Microsecond out of range
assert.throws(RangeError, () => yearStart.with({ microsecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ microsecond: 1000 }, { overflow: 'reject' }));

// Nanosecond out of range
assert.throws(RangeError, () => yearStart.with({ nanosecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ nanosecond: 1000 }, { overflow: 'reject' }));

// Day out of range
assert.throws(RangeError, () => yearStart.with({ day: 0 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ day: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ day: 32 }, { overflow: 'reject' }));

// Month out of range
assert.throws(RangeError, () => yearStart.with({ month: 0 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ month: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ month: 13 }, { overflow: 'reject' }));

// Year out of range
assert.throws(RangeError, () => yearEnd.with({ year: -271822 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ year: -999999 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ year: 275761 }, { overflow: 'reject' }));
assert.throws(RangeError, () => yearStart.with({ year: 999999 }, { overflow: 'reject' }));

// Valid minimum bounds
const minFields = yearStart.with(
    { month: 1, day: 1, hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 },
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

const minYear = yearEnd.with({ year: -271821 }, { overflow: 'reject' });
assert.sameValue(minYear.year, -271821);

// Valid maximum bounds
const maxFields = yearStart.with(
    { month: 12, day: 31, hour: 23, minute: 59, second: 59, millisecond: 999, microsecond: 999, nanosecond: 999 },
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

const maxYear = yearStart.with({ year: 275760 }, { overflow: 'reject' });
assert.sameValue(maxYear.year, 275760);
