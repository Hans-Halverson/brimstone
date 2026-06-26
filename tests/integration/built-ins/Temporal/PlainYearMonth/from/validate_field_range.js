/*---
description: Temporal.PlainYearMonth.from errors for out of bounds date fields.
---*/

// Month out of range
assert.throws(RangeError, () => Temporal.PlainYearMonth.from({ year: 2020, month: 0 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainYearMonth.from({ year: 2020, month: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainYearMonth.from({ year: 2020, month: 13 }, { overflow: 'reject' }));

// Year out of range
assert.throws(RangeError, () => Temporal.PlainYearMonth.from({ year: -271822, month: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainYearMonth.from({ year: -999999, month: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainYearMonth.from({ year: 275761, month: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainYearMonth.from({ year: 999999, month: 1 }, { overflow: 'reject' }));

// Valid minimum bounds
const minMonth = Temporal.PlainYearMonth.from({ year: 2020, month: 1 }, { overflow: 'reject' });
assert.sameValue(minMonth.month, 1);

const minYear = Temporal.PlainYearMonth.from({ year: -271821, month: 12 }, { overflow: 'reject' });
assert.sameValue(minYear.year, -271821);

// Valid maximum bounds
const maxMonth = Temporal.PlainYearMonth.from({ year: 2020, month: 12 }, { overflow: 'reject' });
assert.sameValue(maxMonth.month, 12);

const maxYear = Temporal.PlainYearMonth.from({ year: 275760, month: 1 }, { overflow: 'reject' });
assert.sameValue(maxYear.year, 275760);
