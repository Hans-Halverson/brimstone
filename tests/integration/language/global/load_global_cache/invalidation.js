/*---
description: >
  LoadGlobal cache is invalidated when a cached global property is deleted or shadowed by a
  global lexical binding.
---*/

// Deleting the property invalidates a warm callsite, which then reports an unresolved name.
(function () {
  Object.defineProperty(globalThis, "a", { value: 1, writable: true, configurable: true });
  function get() { return a; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  delete globalThis.a;
  assert.throws(ReferenceError, function () { get(); });
})();

// Deleting and recreating the property refills the callsite with the new property.
(function () {
  Object.defineProperty(globalThis, "b", { value: 1, writable: true, configurable: true });
  function get() { return b; }
  get(); get();
  delete globalThis.b;
  globalThis.b = 42;
  assert.sameValue(get(), 42);
  globalThis.b = 43;
  assert.sameValue(get(), 43);
})();

// A global lexical binding declared by a later script permanently shadows the property.
(function () {
  Object.defineProperty(globalThis, "c", { value: 1, writable: true, configurable: true });
  function get() { return c; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  $262.evalScript("let c = 2;");
  assert.sameValue(get(), 2);
  assert.sameValue(get(), 2);
  // The shadowed global object property is still present and independent.
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "c").value, 1);
  globalThis.c = 9;
  assert.sameValue(get(), 2);
})();

// A getter that declares a shadowing lexical binding invalidates its own cache entry.
(function () {
  Object.defineProperty(globalThis, "d", {
    get: function () { $262.evalScript("let d = 3;"); return 1; },
    configurable: true,
  });
  function get() { return d; }
  // The name was resolved to the global property before the getter ran.
  assert.sameValue(get(), 1);
  // The lexical binding now wins.
  assert.sameValue(get(), 3);
})();

// A getter that deletes its own property and declares a shadowing lexical binding.
(function () {
  Object.defineProperty(globalThis, "e", {
    get: function () {
      delete globalThis.e;
      $262.evalScript("let e = 5;");
      return 1;
    },
    configurable: true,
  });
  function get() { return e; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 5);
})();

// A getter that recreates its property after a shadowing lexical binding exists.
(function () {
  Object.defineProperty(globalThis, "f", {
    get: function () {
      delete globalThis.f;
      $262.evalScript("let f = 6;");
      Object.defineProperty(globalThis, "f", { value: 7, writable: true, configurable: true });
      return 2;
    },
    configurable: true,
  });
  function get() { return f; }
  assert.sameValue(get(), 2);
  assert.sameValue(get(), 6);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "f").value, 7);
})();

// A function declaration in a later script replaces a cached global property value.
(function () {
  globalThis.g = 1;
  function get() { return g; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  $262.evalScript("function g() { return 2; }");
  assert.sameValue(typeof get(), "function");
  assert.sameValue(get()(), 2);
})();

// A var declaration in a later script replaces a cached global property value.
(function () {
  Object.defineProperty(globalThis, "h", { value: 1, writable: true, configurable: true });
  function get() { return h; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  $262.evalScript("var h = 9;");
  assert.sameValue(get(), 9);
})();

// Freezing the global object leaves cached reads correct.
(function () {
  Object.defineProperty(globalThis, "i", { value: 1, writable: true, configurable: true });
  function get() { return i; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  Object.defineProperty(globalThis, "i", { writable: false, configurable: false });
  assert.sameValue(get(), 1);
})();

// A property shadowed by a lexical binding is still reachable through globalThis (its value is
// retained), while a deleted property is not.
(function () {
  Object.defineProperty(globalThis, "j", { value: 1, writable: true, configurable: true });
  function get() { return j; }
  get(); get();
  $262.evalScript("let j = 2;");
  // Bare name reads the lexical binding, globalThis still exposes the shadowed property value.
  assert.sameValue(get(), 2);
  assert.sameValue(globalThis.j, 1);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "j").value, 1);
})();

// A shadowed accessor property is still reachable and callable through globalThis.
(function () {
  var calls = 0;
  Object.defineProperty(globalThis, "k", {
    get: function () { calls++; return 33; },
    configurable: true,
  });
  function get() { return k; }
  assert.sameValue(get(), 33);
  assert.sameValue(get(), 33);
  $262.evalScript("let k = 44;");
  assert.sameValue(get(), 44);
  // The global accessor is untouched: still present and still invokes its getter.
  assert.sameValue(globalThis.k, 33);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "k").get(), 33);
  assert.sameValue(calls, 4);
})();

// A cached accessor property that is deleted while warm re-resolves safely on the next read
// (the deleted property's stored getter is dropped).
(function () {
  Object.defineProperty(globalThis, "l", { get: function () { return 7; }, configurable: true });
  function get() { return l; }
  assert.sameValue(get(), 7);
  assert.sameValue(get(), 7);
  delete globalThis.l;
  assert.throws(ReferenceError, function () { get(); });
  globalThis.l = 8;
  assert.sameValue(get(), 8);
})();

// A warm callsite reads a ReferenceError, not a stale cached value, when the name is shadowed by a
// global lexical binding that is still in its temporal dead zone.
(function () {
  Object.defineProperty(globalThis, "m", { value: 1, writable: true, configurable: true });
  globalThis.readM = function () { return m; };
  readM(); readM();
  $262.evalScript(
    "globalThis.resM = (function () { try { return 'v:' + readM(); } catch (e) { return e.constructor.name; } })();" +
    "let m = 5;"
  );
  assert.sameValue(globalThis.resM, "ReferenceError");
  // Once the lexical binding is initialized it wins, and the shadowed property is untouched.
  assert.sameValue(readM(), 5);
  assert.sameValue(globalThis.m, 1);
})();
