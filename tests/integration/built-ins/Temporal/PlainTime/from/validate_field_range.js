/*---
description: Temporal.PlainTime.from errors for out of bounds time fields.
---*/

// Hour out of range
assert.throws(RangeError, () => Temporal.PlainTime.from({ hour: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainTime.from({ hour: 24 }, { overflow: 'reject' }));

// Minute out of range
assert.throws(RangeError, () => Temporal.PlainTime.from({ minute: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainTime.from({ minute: 60 }, { overflow: 'reject' }));

// Second out of range
assert.throws(RangeError, () => Temporal.PlainTime.from({ second: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainTime.from({ second: 60 }, { overflow: 'reject' }));

// Millisecond out of range
assert.throws(RangeError, () => Temporal.PlainTime.from({ millisecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainTime.from({ millisecond: 1000 }, { overflow: 'reject' }));

// Microsecond out of range
assert.throws(RangeError, () => Temporal.PlainTime.from({ microsecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainTime.from({ microsecond: 1000 }, { overflow: 'reject' }));

// Nanosecond out of range
assert.throws(RangeError, () => Temporal.PlainTime.from({ nanosecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainTime.from({ nanosecond: 1000 }, { overflow: 'reject' }));

// Valid minimum bounds
const minFields = Temporal.PlainTime.from(
    { hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 },
    { overflow: 'reject' }
);
assert.sameValue(minFields.hour, 0);
assert.sameValue(minFields.minute, 0);
assert.sameValue(minFields.second, 0);
assert.sameValue(minFields.millisecond, 0);
assert.sameValue(minFields.microsecond, 0);
assert.sameValue(minFields.nanosecond, 0);

// Valid maximum bounds
const maxFields = Temporal.PlainTime.from(
    { hour: 23, minute: 59, second: 59, millisecond: 999, microsecond: 999, nanosecond: 999 },
    { overflow: 'reject' }
);
assert.sameValue(maxFields.hour, 23);
assert.sameValue(maxFields.minute, 59);
assert.sameValue(maxFields.second, 59);
assert.sameValue(maxFields.millisecond, 999);
assert.sameValue(maxFields.microsecond, 999);
assert.sameValue(maxFields.nanosecond, 999);
