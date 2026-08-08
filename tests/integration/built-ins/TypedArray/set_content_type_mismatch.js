/*---
description: >
  TypedArray.prototype.set checks the source and target content types.
---*/

// Content type check occurs, even for empty arrays
assert.throws(TypeError, () => new BigInt64Array(0).set(new Float64Array(0)));
assert.throws(TypeError, () => new BigInt64Array(1).set(new Float64Array(1)));
assert.throws(TypeError, () => new Float64Array(0).set(new BigInt64Array(0)));

// Matching content types
const target = new BigUint64Array(2);
target.set(new BigInt64Array([1n, 2n]));
assert.sameValue(target[0], 1n);
assert.sameValue(target[1], 2n);

// Offset RangeError is thrown before content type check
assert.throws(RangeError, () => new BigInt64Array(0).set(new Float64Array(0), 1));
