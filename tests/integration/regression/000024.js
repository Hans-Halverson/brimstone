/*---
description: MayContainStrings was failing to track the first operand in unions or intersections.
---*/

// Union may contain strings if any operand may contain strings
assert.throws(SyntaxError, () => new RegExp("[^\\q{bc}a]", "v"));
assert.throws(SyntaxError, () => new RegExp("[^[\\q{bc}]a]", "v"));

// Intersection may contain strings if all operands may contain strings
assert.throws(SyntaxError, () => new RegExp("[^[\\q{ab}c]&&[\\q{ab}d]]","v"));

// Intersection does not throw if only one operand may contain strings
assert(new RegExp("[^a&&\\q{ab|a}]", "v").test("b"));
assert(new RegExp("[^\\q{ab|a}&&[a]]", "v").test("b"));