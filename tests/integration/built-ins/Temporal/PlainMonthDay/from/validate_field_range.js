/*---
description: Temporal.PlainMonthDay.from errors for out of bounds date fields.
---*/

// Day out of range
assert.throws(RangeError, () => Temporal.PlainMonthDay.from({ month: 1, day: 0 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainMonthDay.from({ month: 1, day: -1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainMonthDay.from({ month: 1, day: 32 }, { overflow: 'reject' }));

// Month out of range
assert.throws(RangeError, () => Temporal.PlainMonthDay.from({ month: 0, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainMonthDay.from({ month: -1, day: 1 }, { overflow: 'reject' }));
assert.throws(RangeError, () => Temporal.PlainMonthDay.from({ month: 13, day: 1 }, { overflow: 'reject' }));

// Valid minimum bounds
const minDay = Temporal.PlainMonthDay.from({ month: 1, day: 1 }, { overflow: 'reject' });
assert.sameValue(minDay.monthCode, "M01");
assert.sameValue(minDay.day, 1);

// Valid maximum bounds
const maxDay = Temporal.PlainMonthDay.from({ month: 12, day: 31 }, { overflow: 'reject' });
assert.sameValue(maxDay.monthCode, "M12");
assert.sameValue(maxDay.day, 31);
