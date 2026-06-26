/*---
description: Temporal.PlainDate.prototype.with errors for out of bounds date fields.
---*/

const yearStart = new Temporal.PlainDate(2000, 1, 1);
const yearEnd = new Temporal.PlainDate(2000, 12, 31);

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
const minFields = yearStart.with({ month: 1, day: 1 }, { overflow: 'reject' });
assert.sameValue(minFields.month, 1);
assert.sameValue(minFields.day, 1);

const minYear = yearEnd.with({ year: -271821 }, { overflow: 'reject' });
assert.sameValue(minYear.year, -271821);

// Valid maximum bounds
const maxFields = yearStart.with({ month: 12, day: 31 }, { overflow: 'reject' });
assert.sameValue(maxFields.month, 12);
assert.sameValue(maxFields.day, 31);

const maxYear = yearStart.with({ year: 275760 }, { overflow: 'reject' });
assert.sameValue(maxYear.year, 275760);
