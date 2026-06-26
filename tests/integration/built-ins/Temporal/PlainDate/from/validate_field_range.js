/*---
description: Temporal.PlainDate.from errors for out of bounds date fields.
---*/

// Day out of range
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: 2000, month: 1, day: 0 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: 2000, month: 1, day: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: 2000, month: 1, day: 32 }, { overflow: 'reject' }));

// Month out of range
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: 2000, month: 0, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: 2000, month: -1, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: 2000, month: 13, day: 1 }, { overflow: 'reject' }));

// Year out of range
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: -271822, month: 12, day: 31 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: -999999, month: 1, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: 275761, month: 1, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainDate.from({ year: 999999, month: 1, day: 1 }, { overflow: 'reject' }));

// Valid minimum bounds
const minFields = Temporal.PlainDate.from({ year: 2000, month: 1, day: 1 }, { overflow: 'reject' });
assert.sameValue(minFields.month, 1);
assert.sameValue(minFields.day, 1);

const minYear = Temporal.PlainDate.from({ year: -271821, month: 12, day: 31 }, { overflow: 'reject' });
assert.sameValue(minYear.year, -271821);

// Valid maximum bounds
const maxFields = Temporal.PlainDate.from({ year: 2000, month: 12, day: 31 }, { overflow: 'reject' });
assert.sameValue(maxFields.month, 12);
assert.sameValue(maxFields.day, 31);

const maxYear = Temporal.PlainDate.from({ year: 275760, month: 1, day: 1 }, { overflow: 'reject' });
assert.sameValue(maxYear.year, 275760);
