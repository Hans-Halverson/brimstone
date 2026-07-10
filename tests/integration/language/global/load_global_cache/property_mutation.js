/*---
description: >
  LoadGlobal cache tracks in-place mutations of a cached global property, including when it is
  deleted or shadowed.
---*/

// A setter-only accessor reads as undefined on every access; the cache is kept across reads.
(function () {
  Object.defineProperty(globalThis, "a", { set: function () {}, configurable: true });
  function get() { return a; }
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
})();

// Adding a getter in place to a cached setter-only accessor is observed on the next read.
(function () {
  Object.defineProperty(globalThis, "b", { set: function () {}, configurable: true });
  function get() { return b; }
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
  Object.defineProperty(globalThis, "b", { get: function () { return 42; } });
  assert.sameValue(get(), 42);
  assert.sameValue(get(), 42);
})();

// A cached data property turned setter-only in place reads as undefined afterwards.
(function () {
  Object.defineProperty(globalThis, "c", { value: 5, writable: true, configurable: true });
  function get() { return c; }
  assert.sameValue(get(), 5);
  assert.sameValue(get(), 5);
  Object.defineProperty(globalThis, "c", { get: undefined, set: function () {}, configurable: true });
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
})();

// A cached accessor property turned into a data property in place is read directly afterwards.
(function () {
  Object.defineProperty(globalThis, "d", { get: function () { return 1; }, configurable: true });
  function get() { return d; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  Object.defineProperty(globalThis, "d", { value: 9, writable: true, configurable: true });
  assert.sameValue(get(), 9);
  globalThis.d = 10;
  assert.sameValue(get(), 10);
})();

// A cached callsite tracks a property oscillating between data and accessor in place.
(function () {
  Object.defineProperty(globalThis, "e", { value: 1, writable: true, configurable: true });
  function get() { return e; }
  assert.sameValue(get(), 1);
  for (var i = 0; i < 4; i++) {
    var captured = 100 + i;
    Object.defineProperty(globalThis, "e", {
      get: (function (n) { return function () { return n; }; })(captured),
      set: undefined,
      configurable: true,
    });
    assert.sameValue(get(), 100 + i);
    Object.defineProperty(globalThis, "e", { value: 200 + i, writable: true, configurable: true });
    assert.sameValue(get(), 200 + i);
  }
})();

// A cached getter that deletes its own property misses and re-resolves on the next read.
(function () {
  var calls = 0;
  Object.defineProperty(globalThis, "f", {
    get: function () { calls++; delete globalThis.f; return 5; },
    configurable: true,
  });
  function get() { return f; }
  assert.sameValue(get(), 5);
  assert.throws(ReferenceError, function () { get(); });
  assert.sameValue(calls, 1);
})();

// A cached setter-only accessor that is then deleted misses and re-resolves on the next read.
(function () {
  Object.defineProperty(globalThis, "g", { set: function () {}, configurable: true });
  function get() { return g; }
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
  delete globalThis.g;
  assert.throws(ReferenceError, function () { get(); });
})();

// A cached setter-only accessor shadowed by a later global lexical binding reads the binding.
(function () {
  Object.defineProperty(globalThis, "h", { set: function () {}, configurable: true });
  function get() { return h; }
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
  $262.evalScript("let h = 7;");
  assert.sameValue(get(), 7);
  assert.sameValue(get(), 7);
})();

// A setter-only accessor is kept in the cache across a garbage collection.
(function () {
  var sets = [];
  Object.defineProperty(globalThis, "i", { set: function (v) { sets.push(v); }, configurable: true });
  function get() { return i; }
  assert.sameValue(get(), undefined);
  $262.gc();
  assert.sameValue(get(), undefined);
  Object.defineProperty(globalThis, "i", { get: function () { return 3; } });
  assert.sameValue(get(), 3);
})();
