/*---
description: Array-like objects could cause native allocations to OOM, now throw RangeErrors.
---*/

const arrayLike = { length: 1e100 };

assert.throws(RangeError, () => Function.prototype.apply(() => {}, arrayLike));
assert.throws(RangeError, () => Reflect.apply(() => {}, undefined, arrayLike));
assert.throws(RangeError, () => Reflect.construct(Object, arrayLike));
assert.throws(RangeError, () => Reflect.ownKeys(new Proxy({}, { ownKeys: () => arrayLike })));