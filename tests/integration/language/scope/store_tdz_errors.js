/*---
description: Storing to uninitialized lexical bindings must throw without clobbering the binding.
---*/

// Simple assignment
assert.throws(ReferenceError, () => { a = 5; let a; });

// Destructuring assignment
assert.throws(ReferenceError, () => { ({ x: a } = { x: 7 }); let a; });
assert.throws(ReferenceError, () => { [a] = [7]; let a; });

// Assignment inside a parameter's own default initializer
assert.throws(ReferenceError, () => (function (b = (b = 3)) { return b; })());

// Captured parameter stored and loaded from a closure inside its own default initializer
assert.throws(ReferenceError, () => (function (a = (() => a)()) {})());
assert.throws(ReferenceError, () => (function (a = (() => { a = 1; })()) { return a; })());

// Direct eval inside a parameter list accessing a parameter before it is initialized
assert.throws(ReferenceError, () => (function (a = eval("a")) {})());
assert.throws(ReferenceError, () => (function (a = eval("a = 1")) { return a; })());
assert.throws(ReferenceError, () => (function (a = (() => eval("a"))()) {})());
assert.throws(ReferenceError, () => (function (a = eval("b"), b) {})());
assert.sameValue((function (a, b = eval("a")) { return b; })(1), 1);

// Postfix update whose result is stored back to the same register
assert.throws(ReferenceError, () => { a = a++; let a; });
assert.throws(ReferenceError, () => { let a = a++; });
assert.throws(ReferenceError, () => { let a = a--; });

// The right hand side is evaluated before the TDZ error
{
  const log = [];
  try {
    a = (log.push("rhs"), 1);
    let a;
  } catch (e) {
    log.push(e.constructor.name);
  }
  assert.sameValue(log.join(","), "rhs,ReferenceError");
}
