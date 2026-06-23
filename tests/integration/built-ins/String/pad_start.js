/*---
description: String.prototype.padStart.
---*/

// ToString(fillString) performed before validating new string length
const poisoned = { toString() { throw new Test262Error(); } };
assert.throws(Test262Error, () => 'a'.padStart(Infinity, poisoned));

// ToString(fillString) not performed if string already has max length
assert.sameValue('abcde'.padStart(3, poisoned), 'abcde');

// Do not error for new string length if empty string is being added
assert.sameValue('abc'.padStart(Infinity, ''), 'abc');