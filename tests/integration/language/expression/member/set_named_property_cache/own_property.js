/*---
description: SetNamedProperty cache handles own data and accessor properties.
flags: [noStrict]
---*/

// Own data property store at a warm callsite.
(function () {
  function set(o, v) { o.x = v; }
  var o = { x: 0 };
  set(o, 1);
  set(o, 2);
  assert.sameValue(o.x, 2);
  set(o, 3);
  assert.sameValue(o.x, 3);
})();

// Two receivers sharing a shape hit the same cache entry, stores stay separate.
(function () {
  function make() { return { m: 0, n: 0 }; }
  function set(o, v) { o.n = v; }
  var a = make();
  var b = make();
  set(a, 1);
  set(a, 2);
  set(b, 3);
  assert.sameValue(a.n, 2);
  assert.sameValue(b.n, 3);
})();

// Own setter is called with the receiver as `this` and the stored value.
(function () {
  var seenThis, seenValue, calls = 0;
  function set(o, v) { o.x = v; }
  var o = {
    set x(v) { calls++; seenThis = this; seenValue = v; },
    get x() { return 42; },
  };
  set(o, 1);
  set(o, 2);
  set(o, 3);
  assert.sameValue(calls, 3);
  assert.sameValue(seenThis, o);
  assert.sameValue(seenValue, 3);
  assert.sameValue(o.x, 42);
})();

// Setter replaced in place keeps the same shape, the cache must call the new setter.
(function () {
  var firstCalls = 0, secondCalls = 0;
  function set(o, v) { o.x = v; }
  var o = { set x(v) { firstCalls++; } };
  set(o, 1);
  set(o, 2);
  Object.defineProperty(o, "x", { set: function (v) { secondCalls++; } });
  set(o, 3);
  assert.sameValue(firstCalls, 2);
  assert.sameValue(secondCalls, 1);
})();

// Getter-only accessor: sloppy mode silently ignores the store.
(function () {
  function set(o, v) { o.x = v; }
  var o = { get x() { return 7; } };
  set(o, 1);
  set(o, 2);
  assert.sameValue(o.x, 7);
})();

// Getter-only accessor: strict mode throws on a warm cache hit.
(function () {
  "use strict";
  function set(o, v) { o.x = v; }
  var o = { get x() { return 7; } };
  assert.throws(TypeError, function () { set(o, 1); });
  assert.throws(TypeError, function () { set(o, 2); });
  assert.sameValue(o.x, 7);
})();

// Non-writable own data property is not cached: sloppy stores stay ignored.
(function () {
  function set(o, v) { o.x = v; }
  var o = {};
  Object.defineProperty(o, "x", { value: 5, writable: false, configurable: true });
  set(o, 1);
  set(o, 2);
  assert.sameValue(o.x, 5);
})();

// Non-writable own data property throws in strict mode on every store.
(function () {
  "use strict";
  function set(o, v) { o.x = v; }
  var o = {};
  Object.defineProperty(o, "x", { value: 5, writable: false, configurable: true });
  assert.throws(TypeError, function () { set(o, 1); });
  assert.throws(TypeError, function () { set(o, 2); });
})();

// Property reconfigured from non-writable to writable: stores start working.
(function () {
  function set(o, v) { o.x = v; }
  var o = {};
  Object.defineProperty(o, "x", { value: 0, writable: false, configurable: true });
  set(o, 1);
  set(o, 2);
  Object.defineProperty(o, "x", { writable: true });
  set(o, 3);
  assert.sameValue(o.x, 3);
})();

// Data property converted to an accessor after warming: the setter takes over.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var o = { x: 0 };
  set(o, 1);
  set(o, 2);
  Object.defineProperty(o, "x", { set: function (v) { calls++; }, configurable: true });
  set(o, 3);
  set(o, 4);
  assert.sameValue(calls, 2);
})();

// Freezing the receiver transitions its shape, warm stores must miss and be ignored.
(function () {
  function set(o, v) { o.x = v; }
  var o = { x: 0 };
  set(o, 1);
  set(o, 2);
  Object.freeze(o);
  set(o, 3);
  assert.sameValue(o.x, 2);
})();

// Deleting another property moves the receiver to map mode, stores stay correct.
(function () {
  function set(o, v) { o.x = v; }
  var o = { x: 0, y: 1 };
  set(o, 1);
  set(o, 2);
  delete o.y;
  set(o, 3);
  assert.sameValue(o.x, 3);
  assert.sameValue(o.hasOwnProperty("y"), false);
})();

// A frozen object of a different shape lineage at a warm callsite is left unchanged.
(function () {
  function set(o, v) { o.z = v; }
  var o = { z: 0 };
  var frozen = Object.freeze({ z: 100 });
  set(o, 1);
  set(o, 2);
  set(frozen, 3);
  assert.sameValue(frozen.z, 100);
  assert.sameValue(o.z, 2);
})();

// The stored value may be the receiver itself.
(function () {
  function set(o, v) { o.self = v; }
  var o = { x: 1 };
  set(o, o);
  set(o, o);
  assert.sameValue(o.self, o);
})();

// A cached setter that throws propagates the exception on a warm cache hit.
(function () {
  var shouldThrow = false;
  function set(o, v) { o.x = v; }
  var o = { set x(v) { if (shouldThrow) { throw new TypeError("boom"); } } };
  set(o, 1);
  set(o, 2);
  shouldThrow = true;
  assert.throws(TypeError, function () { set(o, 3); });
})();

// An own data property store stays correct across a garbage collection.
(function () {
  function set(o, v) { o.x = v; }
  var o = { x: 0 };
  set(o, 1);
  $262.gc();
  set(o, 2);
  assert.sameValue(o.x, 2);
})();

// Setter removed in place leaves a getter-only accessor: a warm strict store must
// throw through the cache, since the shape is unchanged.
(function () {
  "use strict";
  var calls = 0;
  function set(o, v) { o.x = v; }
  var o = { get x() { return 7; }, set x(v) { calls++; } };
  set(o, 1);
  set(o, 2);
  Object.defineProperty(o, "x", { set: undefined });
  assert.throws(TypeError, function () { set(o, 3); });
  assert.throws(TypeError, function () { set(o, 4); });
  assert.sameValue(calls, 2);
  assert.sameValue(o.x, 7);
})();

// Setter removed in place, sloppy mode: warm stores silently no-op through the cache,
// which must re-read the accessor from the slot since the shape is unchanged.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var o = { get x() { return 7; }, set x(v) { calls++; } };
  set(o, 1);
  set(o, 2);
  Object.defineProperty(o, "x", { set: undefined });
  set(o, 3);
  set(o, 4);
  assert.sameValue(calls, 2);
  assert.sameValue(o.x, 7);
})();
