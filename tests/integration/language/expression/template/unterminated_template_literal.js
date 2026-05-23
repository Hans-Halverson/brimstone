/*---
description: Syntax error for unterminated template literals.
---*/

assert.throws(SyntaxError, () => eval('`'));
assert.throws(SyntaxError, () => eval('`$'));
assert.throws(SyntaxError, () => eval('`${'));
assert.throws(SyntaxError, () => eval('`${1'));
assert.throws(SyntaxError, () => eval('`${1}'));