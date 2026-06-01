/*---
description: JSON.parse does not allow trailing commas in objects or arrays.
---*/

assert.throws(SyntaxError, () => JSON.parse('{"a":1,}'));
assert.throws(SyntaxError, () => JSON.parse('{"a":1 ,}'));

assert.throws(SyntaxError, () => JSON.parse('[1,]'));
assert.throws(SyntaxError, () => JSON.parse('[1 ,]'));