/*---
description: Syntax error for unterminated template literals.
---*/

assert.throws(SyntaxError, () => eval('`'));
assert.throws(SyntaxError, () => eval('`$'));
assert.throws(SyntaxError, () => eval('`${'));
assert.throws(SyntaxError, () => eval('`${1'));
assert.throws(SyntaxError, () => eval('`${1}'));

// Backslash followed directly by EOF
assert.throws(SyntaxError, () => eval('`a\\'));

// Slow path
assert.throws(SyntaxError, () => eval('`a\\a'));
