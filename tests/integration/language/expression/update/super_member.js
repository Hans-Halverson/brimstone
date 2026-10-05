/*---
description: >
  Update expressions on super members should correctly load the old value before updating it.
---*/

const proto = { a: 5, b: 5, c: 5, d: 5 };
const obj = {
  __proto__: proto,

  prefixNamed() {
    return ++super.a;
  },
  postfixNamed() {
    return super.b++;
  },
  prefixComputed(key) {
    return ++super[key];
  },
  postfixComputed(key) {
    return super[key]++;
  },
};

assert.sameValue(obj.prefixNamed(), 6);
assert.sameValue(obj.postfixNamed(), 5);
assert.sameValue(obj.prefixComputed("c"), 6);
assert.sameValue(obj.postfixComputed("d"), 5);

// Value is read from the home object's prototype but written to the receiver
for (const key of ["a", "b", "c", "d"]) {
  assert.sameValue(obj[key], 6);
  assert.sameValue(proto[key], 5);
}

// Key expression is evaluated, then property is read and written
{
  const log = [];
  const obj = {
    __proto__: {
      get x() {
        log.push("get");
        return 1;
      },
      set x(value) {
        log.push(`set ${value}`);
      },
    },
    update() {
      return super[(log.push("key expression"), "x")]++;
    },
    nullPrototype() {
      return super[(Object.setPrototypeOf(obj, null), "x")]++;
    },
  };

  assert.sameValue(obj.update(), 1);
  assert.compareArray(log, ["key expression", "get", "set 2"]);

  // Super base is resolved after the key expression is evaluated
  log.length = 0;
  assert.throws(TypeError, () => obj.nullPrototype());
  assert.compareArray(log, []);
}

// This value is resolved before the key expression is evaluated
{
  const log = [];

  class Base {}
  class Derived extends Base {
    constructor() {
      super[(log.push("key expression"), "x")]++;
      super();
    }
  }

  assert.throws(ReferenceError, () => new Derived());
  assert.compareArray(log, []);
}
