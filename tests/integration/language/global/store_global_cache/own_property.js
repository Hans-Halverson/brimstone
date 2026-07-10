/*---
description: StoreGlobal cache handles own data and accessor properties of the global object.
flags: [noStrict]
---*/

// A warm callsite writes through to the global object property.
(function () {
  Object.defineProperty(globalThis, "a", { value: 0, writable: true, configurable: true });
  function set(v) { a = v; }
  set(1);
  assert.sameValue(globalThis.a, 1);
  set(2);
  assert.sameValue(globalThis.a, 2);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "a").value, 2);
})();

// A warm store callsite and a warm load callsite observe each other.
(function () {
  Object.defineProperty(globalThis, "b", { value: 0, writable: true, configurable: true });
  function set(v) { b = v; }
  function get() { return b; }
  set(1); set(2);
  assert.sameValue(get(), 2);
  globalThis.b = 3;
  assert.sameValue(get(), 3);
  set(4);
  assert.sameValue(get(), 4);
})();

// Property attributes are preserved by a cached store.
(function () {
  Object.defineProperty(globalThis, "c", {
    value: 0,
    writable: true,
    enumerable: false,
    configurable: true,
  });
  function set(v) { c = v; }
  set(1); set(2);
  var desc = Object.getOwnPropertyDescriptor(globalThis, "c");
  assert.sameValue(desc.value, 2);
  assert.sameValue(desc.writable, true);
  assert.sameValue(desc.enumerable, false);
  assert.sameValue(desc.configurable, true);
})();

// Values of every kind round-trip through a warm store callsite.
(function () {
  Object.defineProperty(globalThis, "d", { value: 0, writable: true, configurable: true });
  function set(v) { d = v; }
  var sym = Symbol("d");
  var obj = {};
  set(null);
  assert.sameValue(globalThis.d, null);
  set(undefined);
  assert.sameValue(globalThis.d, undefined);
  set(NaN);
  assert.sameValue(globalThis.d !== globalThis.d, true);
  set("str");
  assert.sameValue(globalThis.d, "str");
  set(sym);
  assert.sameValue(globalThis.d, sym);
  set(obj);
  assert.sameValue(globalThis.d, obj);
})();

// A non-writable data property is not written and the store is silently ignored.
(function () {
  Object.defineProperty(globalThis, "e", { value: 1, writable: false, configurable: true });
  function set(v) { e = v; }
  set(2); set(3);
  assert.sameValue(globalThis.e, 1);
})();

// A property that becomes non-writable after the cache is warm stops accepting stores.
(function () {
  Object.defineProperty(globalThis, "f", { value: 1, writable: true, configurable: true });
  function set(v) { f = v; }
  set(2); set(3);
  assert.sameValue(globalThis.f, 3);
  Object.defineProperty(globalThis, "f", { writable: false });
  set(4);
  assert.sameValue(globalThis.f, 3);
  set(5);
  assert.sameValue(globalThis.f, 3);
})();

// A property that becomes writable again after the cache failed still accepts stores.
(function () {
  Object.defineProperty(globalThis, "g", { value: 1, writable: false, configurable: true });
  function set(v) { g = v; }
  set(2);
  assert.sameValue(globalThis.g, 1);
  Object.defineProperty(globalThis, "g", { writable: true });
  set(3);
  assert.sameValue(globalThis.g, 3);
})();

// Built-in non-writable globals silently reject stores in sloppy mode.
(function () {
  function set(v) { NaN = v; }
  set(1); set(2);
  assert.sameValue(globalThis.NaN !== globalThis.NaN, true);
})();

// Stores in strict mode throw when the property is not writable.
(function () {
  "use strict";
  Object.defineProperty(globalThis, "h", { value: 1, writable: true, configurable: true });
  function set(v) { h = v; }
  set(2); set(3);
  assert.sameValue(globalThis.h, 3);
  Object.defineProperty(globalThis, "h", { writable: false });
  assert.throws(TypeError, function () { set(4); });
  assert.throws(TypeError, function () { set(5); });
  assert.sameValue(globalThis.h, 3);
})();

// A cached store stays correct after the global properties map grows.
(function () {
  Object.defineProperty(globalThis, "i", { value: 0, writable: true, configurable: true });
  function set(v) { i = v; }
  set(1); set(2);
  for (var n = 0; n < 300; n++) {
    globalThis["i_filler" + n] = n;
  }
  set(3);
  assert.sameValue(globalThis.i, 3);
})();

// A cached store stays correct across a garbage collection.
(function () {
  Object.defineProperty(globalThis, "j", { value: 0, writable: true, configurable: true });
  function set(v) { j = v; }
  function get() { return j; }
  set(1);
  $262.gc();
  set(2);
  assert.sameValue(get(), 2);
  $262.gc();
  assert.sameValue(globalThis.j, 2);
})();

// The setter of an accessor property runs on every store and receives the global object as
// the receiver.
(function () {
  var seen = [];
  var seenThis;
  Object.defineProperty(globalThis, "k", {
    set: function (v) { seenThis = this; seen.push(v); },
    get: function () { return seen.length; },
    configurable: true,
  });
  function set(v) { k = v; }
  set(1);
  set(2);
  assert.sameValue(seen.join(","), "1,2");
  assert.sameValue(seenThis, globalThis);
})();

// A data property redefined as an accessor switches a warm store callsite to the setter.
(function () {
  var sink;
  Object.defineProperty(globalThis, "l", { value: 0, writable: true, configurable: true });
  function set(v) { l = v; }
  set(1); set(2);
  assert.sameValue(globalThis.l, 2);
  Object.defineProperty(globalThis, "l", {
    set: function (v) { sink = v; },
    get: function () { return 99; },
    configurable: true,
  });
  set(3);
  assert.sameValue(sink, 3);
  set(4);
  assert.sameValue(sink, 4);
  assert.sameValue(globalThis.l, 99);
})();

// An accessor redefined as a data property switches a warm store callsite back to the value.
(function () {
  Object.defineProperty(globalThis, "m", {
    set: function () {},
    get: function () { return 1; },
    configurable: true,
  });
  function set(v) { m = v; }
  set(1); set(2);
  Object.defineProperty(globalThis, "m", { value: 0, writable: true, configurable: true });
  set(3);
  assert.sameValue(globalThis.m, 3);
  set(4);
  assert.sameValue(globalThis.m, 4);
})();

// The setter is re-read on every store, so replacing it in place is observed.
(function () {
  var first = [];
  var second = [];
  Object.defineProperty(globalThis, "n", { set: function (v) { first.push(v); }, configurable: true });
  function set(v) { n = v; }
  set(1);
  Object.defineProperty(globalThis, "n", { set: function (v) { second.push(v); } });
  set(2);
  assert.sameValue(first.join(","), "1");
  assert.sameValue(second.join(","), "2");
})();

// A getter-only accessor silently rejects stores in sloppy mode.
(function () {
  Object.defineProperty(globalThis, "o", { get: function () { return 3; }, configurable: true });
  function set(v) { o = v; }
  set(1); set(2);
  assert.sameValue(globalThis.o, 3);
})();

// A warm store callsite redefined as a getter-only accessor stops accepting stores.
(function () {
  Object.defineProperty(globalThis, "p", { value: 0, writable: true, configurable: true });
  function set(v) { p = v; }
  set(1); set(2);
  Object.defineProperty(globalThis, "p", { get: function () { return 9; }, configurable: true });
  set(3);
  assert.sameValue(globalThis.p, 9);
})();

// A getter-only accessor throws on store in strict mode.
(function () {
  "use strict";
  Object.defineProperty(globalThis, "q", { get: function () { return 3; }, configurable: true });
  function set(v) { q = v; }
  assert.throws(TypeError, function () { set(1); });
  assert.throws(TypeError, function () { set(2); });
})();

// A cached setter that throws propagates the exception on a warm cache hit.
(function () {
  var shouldThrow = false;
  Object.defineProperty(globalThis, "r", {
    set: function () { if (shouldThrow) { throw new TypeError("boom"); } },
    configurable: true,
  });
  function set(v) { r = v; }
  set(1);
  set(2);
  shouldThrow = true;
  assert.throws(TypeError, function () { set(3); });
})();

// A setter that deletes its own property invalidates the warm callsite.
(function () {
  var seen = [];
  Object.defineProperty(globalThis, "s", { value: 0, writable: true, configurable: true });
  function set(v) { s = v; }
  set(1); set(2);
  Object.defineProperty(globalThis, "s", {
    set: function (v) { seen.push(v); delete globalThis.s; },
    get: function () { return -1; },
    configurable: true,
  });
  set(3);
  assert.sameValue(seen.join(","), "3");
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "s"), false);
  // Sloppy mode recreates the deleted global property.
  set(4);
  assert.sameValue(globalThis.s, 4);
})();

// A setter that replaces its own property with a writable data property.
(function () {
  Object.defineProperty(globalThis, "t", { value: 0, writable: true, configurable: true });
  function set(v) { t = v; }
  set(1);
  Object.defineProperty(globalThis, "t", {
    set: function (v) {
      Object.defineProperty(globalThis, "t", { value: v * 10, writable: true, configurable: true });
    },
    configurable: true,
  });
  set(2);
  assert.sameValue(globalThis.t, 20);
  set(3);
  assert.sameValue(globalThis.t, 3);
})();

// An accessor store stays correct across a garbage collection.
(function () {
  var seen = [];
  Object.defineProperty(globalThis, "u", { set: function (v) { seen.push(v); }, configurable: true });
  function set(v) { u = v; }
  set(1);
  $262.gc();
  set(2);
  assert.sameValue(seen.join(","), "1,2");
})();
