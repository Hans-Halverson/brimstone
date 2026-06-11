/*---
description: In non-unicode Annex B mode a `\u{...}` unicode escape is actually a quantifier.
---*/

assert(/a\u{2}b/.test("auub"));
assert(/a\u{1,2}b/.test("aub"));
assert.sameValue(/a\u{2}b/.test("aub"), false);
assert.sameValue(/a\u{1,2}b/.test("auuub"), false);
