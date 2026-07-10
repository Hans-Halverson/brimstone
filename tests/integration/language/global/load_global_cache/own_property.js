/*---
description: LoadGlobal cache handles own data and accessor properties of the global object.
---*/

// A warm callsite tracks writes made through the global object.
(function () {
  Object.defineProperty(globalThis, "a", { value: 1, writable: true, configurable: true });
  function get() { return a; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  globalThis.a = 2;
  assert.sameValue(get(), 2);
})();

// A warm callsite tracks writes made through Object.defineProperty.
(function () {
  Object.defineProperty(globalThis, "b", { value: 1, writable: true, configurable: true });
  function get() { return b; }
  get(); get();
  Object.defineProperty(globalThis, "b", { value: 2 });
  assert.sameValue(get(), 2);
})();

// A warm callsite tracks writes made by another cached global store.
(function () {
  Object.defineProperty(globalThis, "c", { value: 1, writable: true, configurable: true });
  function get() { return c; }
  function set(v) { c = v; }
  get(); get();
  set(2); set(3);
  assert.sameValue(get(), 3);
})();

// Values of every kind round-trip through a warm callsite.
(function () {
  Object.defineProperty(globalThis, "d", { value: 0, writable: true, configurable: true });
  function get() { return d; }
  var sym = Symbol("d");
  var obj = {};
  get();
  globalThis.d = null;
  assert.sameValue(get(), null);
  globalThis.d = undefined;
  assert.sameValue(get(), undefined);
  globalThis.d = NaN;
  assert.sameValue(get() !== get(), true);
  globalThis.d = "str";
  assert.sameValue(get(), "str");
  globalThis.d = sym;
  assert.sameValue(get(), sym);
  globalThis.d = obj;
  assert.sameValue(get(), obj);
})();

// A non-writable data property reads correctly at a warm callsite.
(function () {
  Object.defineProperty(globalThis, "e", { value: 7, writable: false, configurable: true });
  function get() { return e; }
  assert.sameValue(get(), 7);
  assert.sameValue(get(), 7);
})();

// Built-in non-writable, non-configurable globals read correctly at a warm callsite.
(function () {
  function get() { return NaN; }
  assert.sameValue(get() !== get(), true);
  function getInfinity() { return Infinity; }
  assert.sameValue(getInfinity(), Infinity);
  assert.sameValue(getInfinity(), Infinity);
})();

// A cached read stays correct after the global properties map grows.
(function () {
  Object.defineProperty(globalThis, "f", { value: 1, writable: true, configurable: true });
  function get() { return f; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  for (var i = 0; i < 300; i++) {
    globalThis["f_filler" + i] = i;
  }
  assert.sameValue(get(), 1);
  globalThis.f = 2;
  assert.sameValue(get(), 2);
})();

// A cached read stays correct across a garbage collection.
(function () {
  Object.defineProperty(globalThis, "g", { value: 1, writable: true, configurable: true });
  function get() { return g; }
  assert.sameValue(get(), 1);
  $262.gc();
  assert.sameValue(get(), 1);
  globalThis.g = 2;
  $262.gc();
  assert.sameValue(get(), 2);
})();

// Adding and removing unrelated global properties does not disturb a warm callsite.
(function () {
  Object.defineProperty(globalThis, "h", { value: 1, writable: true, configurable: true });
  function get() { return h; }
  get(); get();
  globalThis.hOther = 2;
  delete globalThis.hOther;
  assert.sameValue(get(), 1);
})();

// The getter of an accessor property runs on every read and receives the global object as
// the receiver.
(function () {
  var calls = 0;
  var seenThis;
  Object.defineProperty(globalThis, "i", {
    get: function () { calls++; seenThis = this; return 10; },
    configurable: true,
  });
  function get() { return i; }
  assert.sameValue(get(), 10);
  assert.sameValue(get(), 10);
  assert.sameValue(calls, 2);
  assert.sameValue(seenThis, globalThis);
})();

// The getter is re-read on every access, so replacing it in place is observed.
(function () {
  Object.defineProperty(globalThis, "j", { get: function () { return 1; }, configurable: true });
  function get() { return j; }
  get(); get();
  Object.defineProperty(globalThis, "j", { get: function () { return 2; } });
  assert.sameValue(get(), 2);
})();

// Removing the getter in place leaves a setter-only accessor, which reads as undefined.
(function () {
  Object.defineProperty(globalThis, "k", {
    get: function () { return 1; },
    set: function () {},
    configurable: true,
  });
  function get() { return k; }
  assert.sameValue(get(), 1);
  Object.defineProperty(globalThis, "k", { get: undefined });
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
})();

// A setter-only accessor reads as undefined at a warm callsite.
(function () {
  Object.defineProperty(globalThis, "l", { set: function () {}, configurable: true });
  function get() { return l; }
  assert.sameValue(get(), undefined);
  assert.sameValue(get(), undefined);
})();

// A cached getter that throws propagates the exception on a warm cache hit.
(function () {
  var shouldThrow = false;
  Object.defineProperty(globalThis, "m", {
    get: function () { if (shouldThrow) { throw new TypeError("boom"); } return 1; },
    configurable: true,
  });
  function get() { return m; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  shouldThrow = true;
  assert.throws(TypeError, function () { get(); });
})();

// An accessor read stays correct across a garbage collection.
(function () {
  Object.defineProperty(globalThis, "n", { get: function () { return 7; }, configurable: true });
  function get() { return n; }
  assert.sameValue(get(), 7);
  $262.gc();
  assert.sameValue(get(), 7);
})();

// A data property redefined as an accessor switches a warm callsite to the getter.
(function () {
  Object.defineProperty(globalThis, "o", { value: 1, writable: true, configurable: true });
  function get() { return o; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  Object.defineProperty(globalThis, "o", { get: function () { return 99; }, configurable: true });
  assert.sameValue(get(), 99);
})();

// An accessor redefined as a data property switches a warm callsite to the value.
(function () {
  Object.defineProperty(globalThis, "p", { get: function () { return 1; }, configurable: true });
  function get() { return p; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  Object.defineProperty(globalThis, "p", { value: 5, writable: true, configurable: true });
  assert.sameValue(get(), 5);
  globalThis.p = 6;
  assert.sameValue(get(), 6);
})();

// A getter that deletes and recreates its own property is observed on the next read.
(function () {
  var calls = 0;
  Object.defineProperty(globalThis, "q", {
    get: function () {
      calls++;
      delete globalThis.q;
      globalThis.q = 77;
      return 1;
    },
    configurable: true,
  });
  function get() { return q; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 77);
  assert.sameValue(calls, 1);
})();
