/*---
description: >
  Update expressions on members should correctly load the old value before updating it.
---*/

const obj = { a: 5, b: 5, c: 5, d: 5 };

assert.sameValue(++obj.a, 6);
assert.sameValue(obj.b++, 5);
assert.sameValue(++obj["c"], 6);
assert.sameValue(obj["d"]++, 5);

for (const key of ["a", "b", "c", "d"]) {
  assert.sameValue(obj[key], 6);
}

// Object expression is evaluated, then key expression, then property is read and written
{
  const log = [];
  const obj = {
    get x() {
      log.push("get");
      return 1;
    },
    set x(value) {
      log.push(`set ${value}`);
    },
  };

  assert.sameValue((log.push("object expression"), obj)[(log.push("key expression"), "x")]++, 1);
  assert.compareArray(log, ["object expression", "key expression", "get", "set 2"]);
}

// Key is converted to a property key exactly once
{
  let numConversions = 0;
  const key = {
    toString() {
      numConversions++;
      return "a";
    },
  };

  const obj = { a: 1 };

  assert.sameValue(++obj[key], 2);
  assert.sameValue(numConversions, 1);

  assert.sameValue(obj[key]++, 2);
  assert.sameValue(numConversions, 2);

  obj[key]++;
  assert.sameValue(numConversions, 3);
  assert.sameValue(obj.a, 4);
}

// Converting the key does not clobber the variable holding the key
{
  const key = { toString: () => "a" };

  function param(obj, key) {
    obj[key]++;
    return key;
  }

  function local(obj) {
    let localKey = key;
    obj[localKey]++;
    return localKey;
  }

  function numericParam(obj, key) {
    obj[key]++;
    return key;
  }

  const obj = { a: 1 };

  assert.sameValue(param(obj, key), key);
  assert.sameValue(local(obj), key);
  assert.sameValue(obj.a, 3);

  assert.sameValue(numericParam([0, 0], 1.5), 1.5);
  assert.sameValue(numericParam([0, 0], 1), 1);
}
