/*---
description: >
  TypedArray.prototype.with handles the source ArrayBuffer being resized while coercing the value.
---*/

// Resizing the buffer smaller during coercion leaves the copy loop reading past
// the array's new length. Those reads produce `undefined`, and writing
// `undefined` into a BigInt array throws a TypeError rather than aborting.
const rab = new ArrayBuffer(8 * 4, { maxByteLength: 8 * 4 });
const ta = new BigInt64Array(rab);

const value = {
  valueOf() {
    rab.resize(8);
    return 0n;
  },
};

assert.throws(TypeError, () => ta.with(0, value));

// A number-typed array coerces the out-of-bounds reads to NaN instead, so the
// same shape completes without throwing.
const rab2 = new ArrayBuffer(8 * 4, { maxByteLength: 8 * 4 });
const ta2 = new Float64Array(rab2);
const value2 = {
  valueOf() {
    rab2.resize(8);
    return 1;
  },
};

const result = ta2.with(0, value2);
assert.sameValue(result.length, 4);
assert.sameValue(result[0], 1);
assert.sameValue(result[1], NaN);
