/*---
description: >
  Array destructuring failed to initialize the is_done register and could read a stale value from
  that register in the catch handler if evaluating a pattern threw before IteratorNext was called.
---*/

var log = [];
var iterable = {
  [Symbol.iterator]() {
    return {
      next() {
        log.push("next");
        return { done: false, value: 1 };
      },
      return() {
        log.push("return");
        return {};
      },
    };
  },
};

function throws() {
  throw new Error();
}

function f() {
  var a, b, o = {};
  [a, b] = [1];
  [o[throws()]] = iterable;
}

assert.throws(Error, f);
assert.sameValue(log.join(","), "return");
