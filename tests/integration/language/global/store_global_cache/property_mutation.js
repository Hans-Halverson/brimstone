/*---
description: >
  StoreGlobal cache tracks in-place mutations of a cached global property, including when it is
  deleted, non-writable, or shadowed.
flags: [noStrict]
---*/

// A getter-only accessor silently ignores every store; the cache is kept across stores.
(function () {
  Object.defineProperty(globalThis, "a", { get: function () { return 1; }, configurable: true });
  function set(v) { a = v; }
  set(1); set(2); set(3);
  assert.sameValue(globalThis.a, 1);
})();

// Adding a setter in place to a cached getter-only accessor is observed on the next store.
(function () {
  var seen = [];
  Object.defineProperty(globalThis, "b", { get: function () { return 1; }, configurable: true });
  function set(v) { b = v; }
  set(1); set(2);
  Object.defineProperty(globalThis, "b", { set: function (v) { seen.push(v); } });
  set(3); set(4);
  assert.sameValue(seen.join(","), "3,4");
})();

// A non-writable data property silently ignores every store; the cache is kept across stores.
(function () {
  Object.defineProperty(globalThis, "c", { value: 1, writable: false, configurable: true });
  function set(v) { c = v; }
  set(2); set(3);
  assert.sameValue(globalThis.c, 1);
})();

// Making a cached non-writable data property writable in place lets the next store succeed.
(function () {
  Object.defineProperty(globalThis, "d", { value: 1, writable: false, configurable: true });
  function set(v) { d = v; }
  set(2); set(3);
  assert.sameValue(globalThis.d, 1);
  Object.defineProperty(globalThis, "d", { writable: true });
  set(4);
  assert.sameValue(globalThis.d, 4);
  set(5);
  assert.sameValue(globalThis.d, 5);
})();

// Making a cached writable data property non-writable in place stops further stores.
(function () {
  Object.defineProperty(globalThis, "e", { value: 0, writable: true, configurable: true });
  function set(v) { e = v; }
  set(1); set(2);
  assert.sameValue(globalThis.e, 2);
  Object.defineProperty(globalThis, "e", { writable: false });
  set(3);
  assert.sameValue(globalThis.e, 2);
})();

// Turning a cached writable data property into an accessor with a setter in place routes stores
// through the setter without a refill.
(function () {
  var seen = [];
  Object.defineProperty(globalThis, "f", { value: 0, writable: true, configurable: true });
  function set(v) { f = v; }
  set(1);
  Object.defineProperty(globalThis, "f", {
    set: function (v) { seen.push(v); },
    get: function () { return 0; },
    configurable: true,
  });
  set(2); set(3);
  assert.sameValue(seen.join(","), "2,3");
  // Then turning it getter-only in place stops the stores again.
  Object.defineProperty(globalThis, "f", { get: function () { return 9; }, set: undefined, configurable: true });
  set(4);
  assert.sameValue(seen.join(","), "2,3");
  assert.sameValue(globalThis.f, 9);
})();

// A directly-cached accessor setter is reused across stores and a garbage collection.
(function () {
  var seen = [];
  Object.defineProperty(globalThis, "g", {
    set: function (v) { seen.push(v); },
    get: function () { return 0; },
    configurable: true,
  });
  function set(v) { g = v; }
  set(1); set(2);
  $262.gc();
  set(3);
  assert.sameValue(seen.join(","), "1,2,3");
})();

// A cached accessor setter that deletes its own property misses and re-creates it (sloppy).
(function () {
  var seen = [];
  Object.defineProperty(globalThis, "h", {
    set: function (v) { seen.push(v); delete globalThis.h; },
    get: function () { return 0; },
    configurable: true,
  });
  function set(v) { h = v; }
  set(1);
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "h"), false);
  set(2);
  assert.sameValue(globalThis.h, 2);
  set(3);
  assert.sameValue(globalThis.h, 3);
})();

// A cached getter-only accessor that is then deleted misses and re-creates on the next store.
(function () {
  Object.defineProperty(globalThis, "i", { get: function () { return 1; }, configurable: true });
  function set(v) { i = v; }
  set(1); set(2);
  delete globalThis.i;
  set(3);
  assert.sameValue(globalThis.i, 3);
})();

// A cached non-writable data property shadowed by a later global lexical binding writes the binding.
(function () {
  Object.defineProperty(globalThis, "j", { value: 1, writable: false, configurable: true });
  function set(v) { j = v; }
  function get() { return j; }
  set(2); set(3);
  assert.sameValue(globalThis.j, 1);
  $262.evalScript("let j = 0; globalThis.getLexJ = function () { return j; };");
  set(5);
  assert.sameValue(get(), 5);
  assert.sameValue(globalThis.getLexJ(), 5);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "j").value, 1);
})();

// In strict mode a cached getter-only accessor throws on every store.
(function () {
  "use strict";
  Object.defineProperty(globalThis, "k", { get: function () { return 1; }, configurable: true });
  function set(v) { k = v; }
  assert.throws(TypeError, function () { set(1); });
  assert.throws(TypeError, function () { set(2); });
  assert.throws(TypeError, function () { set(3); });
})();

// In strict mode a cached non-writable data property throws on every store.
(function () {
  "use strict";
  Object.defineProperty(globalThis, "l", { value: 1, writable: false, configurable: true });
  function set(v) { l = v; }
  assert.throws(TypeError, function () { set(2); });
  assert.throws(TypeError, function () { set(3); });
  assert.sameValue(globalThis.l, 1);
})();

// In strict mode a cached writable data property made non-writable in place starts throwing.
(function () {
  "use strict";
  Object.defineProperty(globalThis, "m", { value: 0, writable: true, configurable: true });
  function set(v) { m = v; }
  set(1); set(2);
  assert.sameValue(globalThis.m, 2);
  Object.defineProperty(globalThis, "m", { writable: false });
  assert.throws(TypeError, function () { set(3); });
  assert.throws(TypeError, function () { set(4); });
  assert.sameValue(globalThis.m, 2);
})();

// In strict mode a cached non-writable data property made writable in place stops throwing.
(function () {
  "use strict";
  Object.defineProperty(globalThis, "n", { value: 1, writable: false, configurable: true });
  function set(v) { n = v; }
  assert.throws(TypeError, function () { set(2); });
  Object.defineProperty(globalThis, "n", { writable: true });
  set(3);
  assert.sameValue(globalThis.n, 3);
})();

// In strict mode a cached getter-only accessor that gains a setter in place stops throwing.
(function () {
  "use strict";
  var seen = [];
  Object.defineProperty(globalThis, "o", { get: function () { return 1; }, configurable: true });
  function set(v) { o = v; }
  assert.throws(TypeError, function () { set(1); });
  Object.defineProperty(globalThis, "o", { set: function (v) { seen.push(v); } });
  set(2); set(3);
  assert.sameValue(seen.join(","), "2,3");
})();

// In strict mode a store to a name that only exists as a non-writable data property on the
// prototype throws.
(function () {
  "use strict";
  Object.defineProperty(Object.prototype, "p", { value: 1, writable: false, configurable: true });
  function set(v) { p = v; }
  assert.throws(TypeError, function () { set(2); });
  assert.throws(TypeError, function () { set(3); });
  delete Object.prototype.p;
})();
