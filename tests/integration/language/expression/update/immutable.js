/*---
description: Update expression on an immutable bindings.
---*/

// TDZ check throws ReferenceError before valueOf or immutable TypeError
let calledValueOf = false;
assert.throws(ReferenceError, () => { c1++; });
assert.sameValue(calledValueOf, false);

// valueOf called before immutable TypeError is thrown
const c1 = { valueOf() { calledValueOf = true; return 1; } };
calledValueOf = false;
assert.throws(TypeError, () => { c1++; });
assert.sameValue(calledValueOf, true);
