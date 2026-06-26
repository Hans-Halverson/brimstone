/*---
description: Temporal.PlainMonthDay.prototype.with errors for out of bounds date fields.
---*/

const base = Temporal.PlainMonthDay.from({ month: 1, day: 1 });

// Day out of range
assert.throws(RangeError, () => base.with({ day: 0 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ day: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ day: 32 }, { overflow: 'reject' }));

// Month out of range
assert.throws(RangeError, () => base.with({ month: 0 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ month: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => base.with({ month: 13 }, { overflow: 'reject' }));

// Valid minimum bounds
const minDay = base.with({ month: 1, day: 1 }, { overflow: 'reject' });
assert.sameValue(minDay.monthCode, "M01");
assert.sameValue(minDay.day, 1);

// Valid maximum bounds
const maxDay = base.with({ month: 12, day: 31 }, { overflow: 'reject' });
assert.sameValue(maxDay.monthCode, "M12");
assert.sameValue(maxDay.day, 31);
