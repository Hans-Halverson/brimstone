/*---
description: Temporal.PlainTime.prototype.with errors for out of bounds time fields.
---*/

const base = Temporal.PlainTime.from('12:30:45.100200300');

// Hour out of range
assert.throws(RangeError, () => base.with({ hour: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ hour: 24 }, { overflow: 'reject' }));

// Minute out of range
assert.throws(RangeError, () => base.with({ minute: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ minute: 60 }, { overflow: 'reject' }));

// Second out of range
assert.throws(RangeError, () => base.with({ second: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ second: 60 }, { overflow: 'reject' }));

// Millisecond out of range
assert.throws(RangeError, () => base.with({ millisecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ millisecond: 1000 }, { overflow: 'reject' }));

// Microsecond out of range
assert.throws(RangeError, () => base.with({ microsecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ microsecond: 1000 }, { overflow: 'reject' }));

// Nanosecond out of range
assert.throws(RangeError, () => base.with({ nanosecond: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ nanosecond: 1000 }, { overflow: 'reject' }));

// Valid minimum bounds
const minFields = base.with(
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
const maxFields = base.with(
    { hour: 23, minute: 59, second: 59, millisecond: 999, microsecond: 999, nanosecond: 999 },
    { overflow: 'reject' }
);
assert.sameValue(maxFields.hour, 23);
assert.sameValue(maxFields.minute, 59);
assert.sameValue(maxFields.second, 59);
assert.sameValue(maxFields.millisecond, 999);
assert.sameValue(maxFields.microsecond, 999);
assert.sameValue(maxFields.nanosecond, 999);
