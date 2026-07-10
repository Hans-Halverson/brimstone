/*---
description: >
  The global object stores named properties in exotic storage and indexed properties in ordinary
  storage.
---*/

// Named data properties round-trip through the exotic property handlers.
(function () {
  Object.defineProperty(globalThis, "a", {
    value: 1,
    writable: false,
    enumerable: false,
    configurable: true,
  });
  var desc = Object.getOwnPropertyDescriptor(globalThis, "a");
  assert.sameValue(desc.value, 1);
  assert.sameValue(desc.writable, false);
  assert.sameValue(desc.enumerable, false);
  assert.sameValue(desc.configurable, true);
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "a"), true);
})();

// Named accessor properties round-trip through the exotic property handlers.
(function () {
  function getter() { return 1; }
  function setter() {}
  Object.defineProperty(globalThis, "b", {
    get: getter,
    set: setter,
    enumerable: true,
    configurable: true,
  });
  var desc = Object.getOwnPropertyDescriptor(globalThis, "b");
  assert.sameValue(desc.get, getter);
  assert.sameValue(desc.set, setter);
  assert.sameValue(desc.enumerable, true);
  assert.sameValue(desc.configurable, true);
})();

// A named property redefined in place keeps its identity for cached access.
(function () {
  Object.defineProperty(globalThis, "c", { value: 1, writable: true, configurable: true });
  Object.defineProperty(globalThis, "c", { value: 2 });
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "c").value, 2);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "c").writable, true);
})();

// Symbol-keyed properties are stored in the exotic named property storage.
(function () {
  var sym = Symbol("d");
  globalThis[sym] = 5;
  assert.sameValue(globalThis[sym], 5);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, sym).value, 5);
  assert.sameValue(delete globalThis[sym], true);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, sym), undefined);
})();

// Array index keys are stored in the ordinary indexed property storage.
(function () {
  globalThis[0] = "zero";
  Object.defineProperty(globalThis, "1", { value: "one", enumerable: true, configurable: true });
  assert.sameValue(globalThis[0], "zero");
  assert.sameValue(globalThis[1], "one");
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "0").value, "zero");
  assert.sameValue(delete globalThis[0], true);
  assert.sameValue(globalThis[0], undefined);
  assert.sameValue(delete globalThis[1], true);
})();

// Deleting a configurable named property succeeds, a non-configurable one fails.
(function () {
  Object.defineProperty(globalThis, "e", { value: 1, configurable: true });
  assert.sameValue(delete globalThis.e, true);
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "e"), false);

  Object.defineProperty(globalThis, "f", { value: 1, configurable: false });
  assert.sameValue(Reflect.deleteProperty(globalThis, "f"), false);
  assert.sameValue(globalThis.f, 1);
  assert.throws(TypeError, function () { "use strict"; delete globalThis.f; });
})();

// A top level var declaration creates a non-configurable global property.
(function () {
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "topLevelVar").configurable, false);
  assert.sameValue(Reflect.deleteProperty(globalThis, "topLevelVar"), false);
})();

// Deleting a name that was never defined succeeds.
(function () {
  assert.sameValue(Reflect.deleteProperty(globalThis, "neverDefined"), true);
  assert.sameValue(Reflect.deleteProperty(globalThis, "12345"), true);
})();

// Redefining a non-configurable property throws, and the value is unchanged.
(function () {
  Object.defineProperty(globalThis, "g", { value: 1, writable: false, configurable: false });
  assert.throws(TypeError, function () {
    Object.defineProperty(globalThis, "g", { value: 2 });
  });
  assert.sameValue(globalThis.g, 1);
  // Redefining with an identical descriptor is allowed.
  Object.defineProperty(globalThis, "g", { value: 1 });
  assert.sameValue(globalThis.g, 1);
})();

// The global object's prototype chain is consulted for inherited properties.
(function () {
  Object.prototype.h = 1;
  assert.sameValue(globalThis.h, 1);
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "h"), false);
  assert.sameValue("h" in globalThis, true);
  delete Object.prototype.h;
})();

// Reads and writes through globalThis agree with bare identifier access.
(function () {
  Object.defineProperty(globalThis, "i", { value: 1, writable: true, configurable: true });
  function get() { return i; }
  function set(v) { globalThis.i = v; }
  assert.sameValue(get(), 1);
  set(2);
  assert.sameValue(get(), 2);
  assert.sameValue(globalThis.i, 2);
})();

// Private properties are readable but are not exposed as own property keys.
(function () {
  class Base {
    constructor() { return globalThis; }
  }

  class WithField extends Base {
    #field = 1;
    static has(o) { return #field in o; }
    static get(o) { return o.#field; }
    static set(o, v) { o.#field = v; }
  }

  class WithMethod extends Base {
    #method() { return 2; }
    static has(o) { return #method in o; }
    static call(o) { return o.#method(); }
  }

  var symbolsBefore = Object.getOwnPropertySymbols(globalThis).length;
  var namesBefore = Object.getOwnPropertyNames(globalThis).length;

  assert.sameValue(WithField.has(globalThis), false);
  new WithField();
  assert.sameValue(WithField.has(globalThis), true);
  assert.sameValue(WithField.get(globalThis), 1);
  WithField.set(globalThis, 5);
  assert.sameValue(WithField.get(globalThis), 5);

  new WithMethod();
  assert.sameValue(WithMethod.has(globalThis), true);
  assert.sameValue(WithMethod.call(globalThis), 2);

  assert.sameValue(Object.getOwnPropertySymbols(globalThis).length, symbolsBefore);
  assert.sameValue(Object.getOwnPropertyNames(globalThis).length, namesBefore);
  assert.sameValue(Reflect.ownKeys(globalThis).length, symbolsBefore + namesBefore);

  // A private property survives a garbage collection.
  $262.gc();
  assert.sameValue(WithField.get(globalThis), 5);
  assert.sameValue(WithField.has(globalThis), true);

  // Adding the same private field twice throws.
  assert.throws(TypeError, function () { new WithField(); });

  // A private name is not found on an object that never received it.
  assert.sameValue(WithField.has({}), false);
})();

var topLevelVar = 1;
