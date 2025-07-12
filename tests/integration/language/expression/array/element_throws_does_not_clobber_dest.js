/*---
description: Throwing while evaluating an element in an array literal does not clobber the array's destination.
---*/

function throws() {
  throw new Error();
}

function test() {
  var x = 1;
  try {
    x = [ throws() ];
  } catch {}

  // Register is not clobbered by array value
  assert.sameValue(x, 1);
}

test();