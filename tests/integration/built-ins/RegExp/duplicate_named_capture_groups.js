/*---
description: >
  Duplicate named capture group may be used in separate alternatives. Detect cases where the named
  capture groups appear in nested structures and make sure they are properly detected.
---*/

// Valid cases:

new RegExp("(?<x>a)|(?<x>b)");
new RegExp("(?<x>a)|(?<x>b)|(?<x>c)");
new RegExp("(?:(?<x>a)|(?<x>b))");
new RegExp("(?<y>(?<x>a)|(?<x>b))");
new RegExp("(?:(?<x>a))|(?:(?<x>b))");
new RegExp("(?<x>a)b|c(?<x>d)");
new RegExp("((?<x>a)|(?<x>b))");
new RegExp("(?<x>a)*|(?<x>b)*");

// Invalid cases:

// Simple named capture groups in sequence
assert.throws(SyntaxError, () => new RegExp("(?<x>a)(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?<x>a).(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?<x>a)(?<x>b)(?<x>c)"));

// Nested within capture groups
assert.throws(SyntaxError, () => new RegExp("(?:(?<x>a))(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?<x>a)(?:(?<x>b))"));
assert.throws(SyntaxError, () => new RegExp("(?<y>(?<x>a))(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?:(?:(?<x>a)))(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?:(?<x>a)|(?<x>b))(?<x>c)"));

// Nested within capture groups of the same name
assert.throws(SyntaxError, () => new RegExp("(?<x>(?<x>a))"));
assert.throws(SyntaxError, () => new RegExp("(?<x>(?<x>a)|b)"));

// Named groups within lookarounds
assert.throws(SyntaxError, () => new RegExp("(?=(?<x>a))(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?<x>a)(?=(?<x>b))"));
assert.throws(SyntaxError, () => new RegExp("(?!(?<x>a))(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?<=(?<x>a))(?<x>b)"));

// Named groups within quantifiers
assert.throws(SyntaxError, () => new RegExp("(?<x>a)*(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?:(?<x>a))*(?<x>b)"));
assert.throws(SyntaxError, () => new RegExp("(?:(?<x>a)|(?<x>b))+(?<x>c)"));
