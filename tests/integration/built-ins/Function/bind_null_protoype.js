/*---
description: Function.prototype.bind preserves null prototype
---*/

// Create function with a null prototype
function f() {}
Object.setPrototypeOf(f, null);

// Bind should preserve the null prototype
const boundFunction = Function.prototype.bind.call(f, null);
assert.sameValue(Object.getPrototypeOf(boundFunction), null);