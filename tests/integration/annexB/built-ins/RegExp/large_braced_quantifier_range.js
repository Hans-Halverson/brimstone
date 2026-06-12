/*---
description: Braced quantifier bounds too large to represent saturate instead of erroring.
---*/

// Bounds too large to represent are valid syntax and saturated internally
new RegExp("a{99999999999999999999}");
new RegExp("a{99999999999999999999,}");
new RegExp("a{99999999999999999999,99999999999999999999}");
new RegExp("a{1,99999999999999999999}");

// A saturated upper bound executes and may match, is treated as unbounded
assert.sameValue(/a{1,99999999999999999999}/.exec("aaaa")[0], "aaaa");
assert.sameValue(/a{0,99999999999999999999}/.exec("bbb")[0], "");

// A saturated lower bound executes but fails since string is never long enough to match
assert.sameValue(/a{99999999999999999999}/.test("aaa"), false);
assert.sameValue(/a{99999999999999999999,}/.test("aaa"), false);

// Incompatible bounds are rejected even when the lower bound is too large to represent
assert.throws(SyntaxError, () => new RegExp("a{99999999999999999999,1}"));

// A quantifier with nothing to repeat is still rejected regardless of bound size
assert.throws(SyntaxError, () => new RegExp("{99999999999999999999}"));
assert.throws(SyntaxError, () => new RegExp("{99999999999999999999,}"));
assert.throws(SyntaxError, () => new RegExp("{99999999999999999999,99999999999999999999}"));
