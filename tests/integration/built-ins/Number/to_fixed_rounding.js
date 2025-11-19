/*---
description: Number.prototype.toFixed rounds away from zero on .5 (not towards even)
---*/

// Rounds .5 towards zero
assert.sameValue((1.5).toFixed(0), '2');
assert.sameValue((-1.5).toFixed(0), '-2');
assert.sameValue((0.05).toFixed(1), '0.1');
assert.sameValue((-0.05).toFixed(1), '-0.1');

// Rounds .5 away from zero (not towards even)
assert.sameValue((0.5).toFixed(0), '1');
assert.sameValue((-0.5).toFixed(0), '-1');
assert.sameValue((2.25).toFixed(1), '2.3');
assert.sameValue((-2.25).toFixed(1), '-2.3');