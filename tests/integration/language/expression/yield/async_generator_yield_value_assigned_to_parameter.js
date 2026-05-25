/*---
description: Async yield expression result can be assigned to a parameter.
flags: [async]
---*/

async function* g(x) {
  x = yield x;
  return x;
}

const it = g(5);
it.next()
  .then(function (first) {
    assert.sameValue(first.value, 5);
    assert.sameValue(first.done, false);
    return it.next(99);
  })
  .then(function (second) {
    assert.sameValue(second.value, 99);
    assert.sameValue(second.done, true);
  })
  .then($DONE, $DONE);
