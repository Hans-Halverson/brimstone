/*---
description: Temporal.PlainYearMonth.prototype.with errors for out of bounds date fields.
---*/

const yearStart = Temporal.PlainYearMonth.from('2020-01');
const yearEnd   = Temporal.PlainYearMonth.from('2020-12');

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
const minMonth = yearStart.with({ month: 1 }, { overflow: 'reject' });
assert.sameValue(minMonth.month, 1);

const minYear = yearEnd.with({ year: -271821 }, { overflow: 'reject' });
assert.sameValue(minYear.year, -271821);

// Valid maximum bounds
const maxMonth = yearStart.with({ month: 12 }, { overflow: 'reject' });
assert.sameValue(maxMonth.month, 12);

const maxYear = yearStart.with({ year: 275760 }, { overflow: 'reject' });
assert.sameValue(maxYear.year, 275760);
