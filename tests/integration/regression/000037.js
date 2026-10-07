/*---
description: >
  A computed class field key should not clobber the original binding during ToPropertyKey.
---*/

const key = { toString() { return "a"; } };

function local() {
  let k = key;
  class C { [k] = 1; }
  return k;
}
assert.sameValue(local(), key);

function staticField() {
  let k = key;
  class C { static [k] = 1; }
  return k;
}
assert.sameValue(staticField(), key);

function argument(k) {
  class C { [k] = 1; }
  return k;
}
assert.sameValue(argument(key), key);

function constant() {
  const k = key;
  class C { [k] = 1; }
  return k;
}
assert.sameValue(constant(), key);
