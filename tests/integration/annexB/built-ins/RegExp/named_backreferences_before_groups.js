/*---
description: Named backreferences that appear before their capture group are allowed in Annex B.
---*/

assert((new RegExp("\\k<a>(?<a>b)", "u")).test("b"));
assert((new RegExp("(?<a>\\k<a>b)", "u")).test("b"));

assert.throws(SyntaxError, () => new RegExp("\\k<a>b", "u"));