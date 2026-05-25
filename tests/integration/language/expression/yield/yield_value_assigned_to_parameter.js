/*---
description: Yield expression result can be assigned to a parameter.
---*/

function* g(x) {
  x = yield x;
  return x;
}

const it = g(1);
assert.sameValue(it.next().value, 1);

const result = it.next(99);
assert.sameValue(result.value, 99);
assert.sameValue(result.done, true);
