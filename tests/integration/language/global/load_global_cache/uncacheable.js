/*---
description: LoadGlobal reads that cannot be cached stay correct at a warm callsite.
---*/

// A name that only exists on the global object's prototype is never cached.
(function () {
  Object.prototype.a = 11;
  function get() { return a; }
  assert.sameValue(get(), 11);
  assert.sameValue(get(), 11);
  Object.prototype.a = 12;
  assert.sameValue(get(), 12);
  delete Object.prototype.a;
})();

// A prototype accessor is consulted on every read.
(function () {
  var calls = 0;
  Object.defineProperty(Object.prototype, "b", {
    get: function () { calls++; return 5; },
    configurable: true,
  });
  function get() { return b; }
  assert.sameValue(get(), 5);
  assert.sameValue(get(), 5);
  assert.sameValue(calls, 2);
  delete Object.prototype.b;
})();

// A name shadowed by a prototype property that later becomes an own global property.
(function () {
  Object.prototype.c = 1;
  function get() { return c; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  globalThis.c = 2;
  assert.sameValue(get(), 2);
  delete Object.prototype.c;
  assert.sameValue(get(), 2);
})();

// A global lexical binding is never cached and always reads the current value.
(function () {
  $262.evalScript("let d = 1; globalThis.setD = function (v) { d = v; };");
  function get() { return d; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  globalThis.setD(2);
  assert.sameValue(get(), 2);
})();

// A global const binding is never cached and cannot be assigned.
(function () {
  $262.evalScript("const e = 1;");
  function get() { return e; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  assert.throws(TypeError, function () { e = 2; });
})();

// A Proxy prototype of the global object intercepts every unresolved read.
(function () {
  var gets = 0;
  var proxyProto = new Proxy({}, {
    has: function (t, k) { return k === "f"; },
    get: function (t, k) { if (k === "f") { gets++; return 42; } return undefined; },
  });
  var oldProto = Object.getPrototypeOf(globalThis);
  Object.setPrototypeOf(globalThis, proxyProto);
  function get() { return f; }
  var v1 = get();
  var v2 = get();
  Object.setPrototypeOf(globalThis, oldProto);
  assert.sameValue(v1, 42);
  assert.sameValue(v2, 42);
  assert.sameValue(gets, 2);
})();

// A null prototype on the global object leaves unresolved reads unresolved.
(function () {
  var oldProto = Object.getPrototypeOf(globalThis);
  Object.setPrototypeOf(globalThis, null);
  function get() { return typeof g; }
  var v1 = get();
  var v2 = get();
  Object.setPrototypeOf(globalThis, oldProto);
  assert.sameValue(v1, "undefined");
  assert.sameValue(v2, "undefined");
})();

// Each realm caches its own global properties independently.
(function () {
  var other = $262.createRealm();
  other.evalScript("var h = 'other';");
  globalThis.h = "this";
  function get() { return h; }
  assert.sameValue(get(), "this");
  assert.sameValue(get(), "this");
  assert.sameValue(other.global.h, "other");
})();
