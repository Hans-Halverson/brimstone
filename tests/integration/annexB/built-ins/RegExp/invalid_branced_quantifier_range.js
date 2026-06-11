/*---
description: Braced quantifier bounds must be in range.
---*/

// Valid quantifier position
assert.throws(SyntaxError, () => new RegExp("a{99999999999999999999}"));
assert.throws(SyntaxError, () => new RegExp("a{99999999999999999999,}"));
assert.throws(SyntaxError, () => new RegExp("a{99999999999999999999,99999999999999999999}"));
assert.throws(SyntaxError, () => new RegExp("a{1,99999999999999999999}"));

// Invalid quantifier position
assert.throws(SyntaxError, () => new RegExp("{99999999999999999999}"));
assert.throws(SyntaxError, () => new RegExp("{99999999999999999999,}"));
assert.throws(SyntaxError, () => new RegExp("{99999999999999999999,99999999999999999999}"));
assert.throws(SyntaxError, () => new RegExp("{1,99999999999999999999}"));
