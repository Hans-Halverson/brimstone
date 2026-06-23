/*---
description: String.prototype.padStart.
---*/

const poisoned = { toString() { throw new Test262Error(); } };
assert.throws(Test262Error, () => 'a'.padStart(Infinity, poisoned));