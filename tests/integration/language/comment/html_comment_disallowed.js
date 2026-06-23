/*---
description: HTML comments are not allowed outside Annex B mode.
---*/

assert.throws(SyntaxError, () => eval('<!-- comment'));
assert.throws(SyntaxError, () => eval('--> comment'));
