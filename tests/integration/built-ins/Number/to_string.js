/*---
description: Number.prototype.toString conversion.
---*/

// Large numbers switch to exponential notation at 1e21
assert.sameValue((1e20).toString(), '100000000000000000000');
assert.sameValue((1e21).toString(), '1e+21');
assert.sameValue((-1e21).toString(), '-1e+21');
assert.sameValue(Number.MAX_VALUE.toString(), '1.7976931348623157e+308');

// Small numbers switch to exponential notation at 1e-7
assert.sameValue((1e-6).toString(), '0.000001');
assert.sameValue((1e-7).toString(), '1e-7');
assert.sameValue((-1e-7).toString(), '-1e-7');
assert.sameValue(Number.MIN_VALUE.toString(), '5e-324');

// Explicit radix of 10 behaves the same as no radix
assert.sameValue((1e21).toString(10), '1e+21');
assert.sameValue((1e-7).toString(10), '1e-7');
