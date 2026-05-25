/*---
description: Await expression result can be assigned to a parameter.
flags: [async]
---*/

async function f(x) {
  x = await x;
  return x;
}

f(3)
  .then(function (value) {
    assert.sameValue(value, 3);
  })
  .then($DONE, $DONE);
