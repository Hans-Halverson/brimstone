/*---
description: Assignment to immutable bindings.
---*/

// TDZ check throws ReferenceError before immutable TypeError
assert.throws(ReferenceError, () => { c1 = 1 });
const c1 = 2;