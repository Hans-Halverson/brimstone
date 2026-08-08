/*---
description: >
  TypedArray.prototype.filter handles the source ArrayBuffer being detached by the callback.
---*/

// Detaching the buffer from the callback leaves the collection loop reading past
// the array's end. Those reads produce `undefined`, and writing `undefined` into
// a BigInt array throws a TypeError rather than aborting.
const buffer = new ArrayBuffer(8 * 4);
const ta = new BigInt64Array(buffer);

assert.throws(TypeError, () =>
  ta.filter((_, i) => {
    if (i === 0) {
      buffer.transfer();
    }

    return true;
  })
);

// A number-typed array coerces the out-of-bounds reads to NaN instead, so the
// same shape completes without throwing.
const buffer2 = new ArrayBuffer(8 * 4);
const ta2 = new Float64Array(buffer2);
ta2[0] = 1;

const result = ta2.filter((_, i) => {
  if (i === 0) {
    buffer2.transfer();
  }

  return true;
});

assert.sameValue(result.length, 4);
assert.sameValue(result[0], 1);
assert.sameValue(result[1], NaN);
