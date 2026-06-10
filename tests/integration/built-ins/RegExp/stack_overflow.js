/*---
description: RangeError is thrown when backtrack stack gets too large.
---*/

assert.throws(RangeError, () => /a(?:x|){1000000000,1000000001}b/.exec("axxb"));
assert.throws(RangeError, () => /a(?=(?:x|){1000000000,1000000001})b/.exec("axxb"));