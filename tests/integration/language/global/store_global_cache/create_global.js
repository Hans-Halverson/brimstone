/*---
description: StoreGlobal creates a new global property for an unresolved name in sloppy mode.
flags: [noStrict]
---*/

// An unresolved name becomes a writable, enumerable, configurable global property.
(function () {
  function set(v) { a = v; }
  set(1);
  var desc = Object.getOwnPropertyDescriptor(globalThis, "a");
  assert.sameValue(desc.value, 1);
  assert.sameValue(desc.writable, true);
  assert.sameValue(desc.enumerable, true);
  assert.sameValue(desc.configurable, true);
})();

// The callsite caches the newly created property and reuses it for later stores.
(function () {
  function set(v) { b = v; }
  function get() { return b; }
  set(1);
  assert.sameValue(get(), 1);
  set(2);
  assert.sameValue(get(), 2);
  assert.sameValue(globalThis.b, 2);
})();

// A store to a name that only exists as a writable data property on the prototype
// creates a shadowing own property on the global object.
(function () {
  Object.prototype.c = 1;
  function set(v) { c = v; }
  set(5);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "c").value, 5);
  assert.sameValue(Object.prototype.c, 1);
  set(6);
  assert.sameValue(globalThis.c, 6);
  delete Object.prototype.c;
})();

// A store to a name that only exists as a non-writable data property on the prototype is
// silently ignored in sloppy mode and throws in strict mode.
(function () {
  Object.defineProperty(Object.prototype, "d", { value: 1, writable: false, configurable: true });
  function set(v) { d = v; }
  set(5); set(6);
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "d"), false);
  assert.sameValue(Object.prototype.d, 1);
  assert.throws(TypeError, function () { "use strict"; d = 7; });
  delete Object.prototype.d;
})();

// A store to a name that only exists as an accessor on the prototype calls the setter
// with the global object as the receiver, and creates no own property.
(function () {
  var seen = [];
  var seenThis;
  Object.defineProperty(Object.prototype, "e", {
    set: function (v) { seenThis = this; seen.push(v); },
    get: function () { return 0; },
    configurable: true,
  });
  function set(v) { e = v; }
  set(1); set(2);
  assert.sameValue(seen.join(","), "1,2");
  assert.sameValue(seenThis, globalThis);
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "e"), false);
  delete Object.prototype.e;
})();

// An unresolved name in strict mode throws instead of creating a global property.
(function () {
  "use strict";
  function set(v) { f = v; }
  assert.throws(ReferenceError, function () { set(1); });
  assert.throws(ReferenceError, function () { set(2); });
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "f"), false);
})();

// A created global property can be deleted and recreated at the same callsite.
(function () {
  function set(v) { g = v; }
  set(1);
  assert.sameValue(globalThis.g, 1);
  delete globalThis.g;
  set(2);
  assert.sameValue(globalThis.g, 2);
})();

// A created global property survives a garbage collection.
(function () {
  function set(v) { h = v; }
  set(1);
  $262.gc();
  set(2);
  assert.sameValue(globalThis.h, 2);
})();
