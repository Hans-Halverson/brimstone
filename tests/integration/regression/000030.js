/*---
description: >
  Bug in BsHashMap insertion where value was inserted at first deleted entry even if a matching
  occupied entry for that key existed later in the probing sequence.
---*/

// Force HasProperty true for holes
Array.prototype[1] = undefined;
Array.prototype[100] = undefined;

var a = new Array(20000);
a.push("one");
a.unshift("two");
a.unshift("three", "four");

assert.sameValue(a[0], "three");
assert.sameValue(a[1], "four");
assert.sameValue(a[2], "two");