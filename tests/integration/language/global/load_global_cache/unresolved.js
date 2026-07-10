/*---
description: >
  LoadGlobal and LoadGlobalOrUnresolved handle names that are not defined on the global object.
---*/

// An unresolved name throws a ReferenceError on every read.
(function () {
  function get() { return a; }
  assert.throws(ReferenceError, function () { get(); });
  assert.throws(ReferenceError, function () { get(); });
})();

// typeof an unresolved name is "undefined" and is not cached, so a later definition is seen.
(function () {
  function get() { return typeof b; }
  assert.sameValue(get(), "undefined");
  assert.sameValue(get(), "undefined");
  globalThis.b = 5;
  assert.sameValue(get(), "number");
  assert.sameValue(get(), "number");
})();

// typeof an unresolved name stays "undefined" after the name is defined and deleted again.
(function () {
  function get() { return typeof c; }
  assert.sameValue(get(), "undefined");
  globalThis.c = 5;
  assert.sameValue(get(), "number");
  delete globalThis.c;
  assert.sameValue(get(), "undefined");
})();

// A name that exists only as a global object property with the value undefined resolves.
(function () {
  Object.defineProperty(globalThis, "d", { value: undefined, writable: true, configurable: true });
  function get() { return d; }
  function typeofGet() { return typeof d; }
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
  assert.sameValue(typeofGet(), "undefined");
})();

// A read of a deleted name throws again after the cache is invalidated.
(function () {
  globalThis.e = 1;
  function get() { return e; }
  assert.sameValue(get(), 1);
  delete globalThis.e;
  assert.throws(ReferenceError, function () { get(); });
  assert.throws(ReferenceError, function () { get(); });
  globalThis.e = 2;
  assert.sameValue(get(), 2);
})();

// typeof a name that only exists on the global object's prototype.
(function () {
  function get() { return typeof f; }
  assert.sameValue(get(), "undefined");
  Object.prototype.f = 1;
  assert.sameValue(get(), "number");
  delete Object.prototype.f;
  assert.sameValue(get(), "undefined");
})();
