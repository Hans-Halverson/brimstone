/*---
description: Syntax error for unterminated string literals.
---*/

// Fast paths
assert.throws(SyntaxError, () => eval('"'));
assert.throws(SyntaxError, () => eval("'"));
assert.throws(SyntaxError, () => eval('"a'));
assert.throws(SyntaxError, () => eval('"a\n"'));

// Backslash followed directly by EOF
assert.throws(SyntaxError, () => eval('"\\'));

// Slow paths
assert.throws(SyntaxError, () => eval('"a\\a'));
assert.throws(SyntaxError, () => eval('"a\\a\n"'));
assert.throws(SyntaxError, () => eval('"a\\a\\'));

