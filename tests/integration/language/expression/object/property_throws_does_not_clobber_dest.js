/*---
description: Throwing while evaluating a property in an object literal does not clobber the object's destination.
---*/

function throws() {
  throw new Error();
}

function test() {
  var x = 1;
  try {
    x = { prop: throws() };
  } catch {}

  // Register is not clobbered by object value
  assert.sameValue(x, 1);
}

test();