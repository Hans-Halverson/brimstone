/*---
description: Set methods that check the argument's `size` property properly validate it.
---*/

const set = new Set();

const missingSize = {};

assert.throws(TypeError, () => set.difference(missingSize));
assert.throws(TypeError, () => set.intersection(missingSize));
assert.throws(TypeError, () => set.isDisjointFrom(missingSize));
assert.throws(TypeError, () => set.isSubsetOf(missingSize));
assert.throws(TypeError, () => set.isSupersetOf(missingSize));
assert.throws(TypeError, () => set.symmetricDifference(missingSize));
assert.throws(TypeError, () => set.union(missingSize));

const incorrectSizeType = { size: {} };

assert.throws(TypeError, () => set.difference(incorrectSizeType));
assert.throws(TypeError, () => set.intersection(incorrectSizeType));
assert.throws(TypeError, () => set.isDisjointFrom(incorrectSizeType));
assert.throws(TypeError, () => set.isSubsetOf(incorrectSizeType));
assert.throws(TypeError, () => set.isSupersetOf(incorrectSizeType));
assert.throws(TypeError, () => set.symmetricDifference(incorrectSizeType));
assert.throws(TypeError, () => set.union(incorrectSizeType));

const nanSize = { size: NaN };

assert.throws(TypeError, () => set.difference(nanSize));
assert.throws(TypeError, () => set.intersection(nanSize));
assert.throws(TypeError, () => set.isDisjointFrom(nanSize));
assert.throws(TypeError, () => set.isSubsetOf(nanSize));
assert.throws(TypeError, () => set.isSupersetOf(nanSize));
assert.throws(TypeError, () => set.symmetricDifference(nanSize));
assert.throws(TypeError, () => set.union(nanSize));

const negativeSize = { size: -1 };

assert.throws(RangeError, () => set.difference(negativeSize));
assert.throws(RangeError, () => set.intersection(negativeSize));
assert.throws(RangeError, () => set.isDisjointFrom(negativeSize));
assert.throws(RangeError, () => set.isSubsetOf(negativeSize));
assert.throws(RangeError, () => set.isSupersetOf(negativeSize));
assert.throws(RangeError, () => set.symmetricDifference(negativeSize));
assert.throws(RangeError, () => set.union(negativeSize));