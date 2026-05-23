/*---
description: Overflow in TypedArray.prototype.set should not cause a crash
---*/

const ta1 = new Float32Array([0]);
assert.throws(RangeError, () => ta1.set(ta1, 1.0e+100));

const ta2 = new Float32Array([0]);
assert.throws(RangeError, () => ta2.set({ length: 1 }, 1.0e+100));
