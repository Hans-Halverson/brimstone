/*---
description: >
  Assignment to a call expression is valid syntax in sloppy Annex B mode but throws a ReferenceError
  at runtime.
flags: [noStrict]
---*/

function f() {
  return 2;
}

// Assignment to a call expression does not write to the destination before throwing
function callAssign() {
  let a = 1;
  try {
    a = f() = 3;
  } catch (e) {
    assert(e instanceof ReferenceError);
  }
  return a;
}
assert.sameValue(callAssign(), 1);


// Update expression on a call expression does not write to the destination before throwing
function callUpdate() {
  let a = 1;
  try {
    a = f()++;
  } catch (e) {
    assert(e instanceof ReferenceError);
  }
  return a;
}
assert.sameValue(callUpdate(), 1);
