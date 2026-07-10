/*---
description: >
  StoreGlobal cache is invalidated when a cached global property is deleted or shadowed by a
  global lexical binding.
flags: [noStrict]
---*/

// Deleting the property invalidates a warm callsite, which then recreates it in sloppy mode.
(function () {
  Object.defineProperty(globalThis, "a", { value: 0, writable: true, configurable: true });
  function set(v) { a = v; }
  set(1); set(2);
  assert.sameValue(globalThis.a, 2);
  delete globalThis.a;
  set(3);
  assert.sameValue(globalThis.a, 3);
  set(4);
  assert.sameValue(globalThis.a, 4);
})();

// Deleting the property invalidates a warm callsite, which then throws in strict mode.
(function () {
  "use strict";
  Object.defineProperty(globalThis, "b", { value: 0, writable: true, configurable: true });
  function set(v) { b = v; }
  set(1); set(2);
  delete globalThis.b;
  assert.throws(ReferenceError, function () { set(3); });
})();

// A global lexical binding declared by a later script permanently shadows the property.
(function () {
  Object.defineProperty(globalThis, "c", { value: 0, writable: true, configurable: true });
  function set(v) { c = v; }
  function get() { return c; }
  set(1); set(2);
  assert.sameValue(globalThis.c, 2);
  $262.evalScript("let c = 100;");
  set(3);
  // The lexical binding is written, not the global object property.
  assert.sameValue(get(), 3);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "c").value, 2);
})();

// A setter that declares a shadowing lexical binding invalidates its own cache entry.
(function () {
  var seen = [];
  Object.defineProperty(globalThis, "d", {
    set: function (v) { seen.push(v); $262.evalScript("let d = 9;"); },
    get: function () { return 1; },
    configurable: true,
  });
  function set(v) { d = v; }
  function get() { return d; }
  set(1);
  assert.sameValue(seen.join(","), "1");
  assert.sameValue(get(), 9);
  // Subsequent stores hit the lexical binding, the setter is never called again.
  set(2);
  assert.sameValue(get(), 2);
  assert.sameValue(seen.join(","), "1");
})();

// A setter that recreates its property after a shadowing lexical binding exists.
(function () {
  Object.defineProperty(globalThis, "e", {
    set: function () {
      delete globalThis.e;
      $262.evalScript("let e = 4;");
      Object.defineProperty(globalThis, "e", { value: 8, writable: true, configurable: true });
    },
    get: function () { return 0; },
    configurable: true,
  });
  function set(v) { e = v; }
  function get() { return e; }
  set(1);
  assert.sameValue(get(), 4);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "e").value, 8);
})();

// A store to a global const binding throws.
(function () {
  $262.evalScript("const f = 1;");
  function set(v) { f = v; }
  assert.throws(TypeError, function () { set(2); });
  assert.throws(TypeError, function () { set(3); });
})();

// A store to a global let binding is never cached and always writes the binding.
(function () {
  $262.evalScript("let g = 1; globalThis.getG = function () { return g; };");
  function set(v) { g = v; }
  set(2);
  assert.sameValue(globalThis.getG(), 2);
  set(3);
  assert.sameValue(globalThis.getG(), 3);
})();

// Freezing a cached property makes further stores silent no-ops.
(function () {
  Object.defineProperty(globalThis, "h", { value: 0, writable: true, configurable: true });
  function set(v) { h = v; }
  set(1); set(2);
  Object.defineProperty(globalThis, "h", { writable: false, configurable: false });
  set(3);
  assert.sameValue(globalThis.h, 2);
})();

// A warm callsite throws a ReferenceError and writes nothing when the name is shadowed by a global
// lexical binding that is still in its temporal dead zone.
(function () {
  Object.defineProperty(globalThis, "i", { value: 1, writable: true, configurable: true });
  globalThis.writeI = function (v) { i = v; };
  writeI(2); writeI(3);
  $262.evalScript(
    "globalThis.resI = (function () { try { writeI(9); return 'ok'; } catch (e) { return e.constructor.name; } })();" +
    "let i = 5;"
  );
  assert.sameValue(globalThis.resI, "ReferenceError");
  // The store during the dead zone wrote nothing: the shadowed property keeps its last value.
  assert.sameValue(globalThis.i, 3);
})();
