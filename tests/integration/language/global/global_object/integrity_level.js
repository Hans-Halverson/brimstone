/*---
description: >
  Integrity levels applied to the global object keep cached global property access correct.
flags: [noStrict]
---*/

Object.defineProperty(globalThis, "sealed", { value: 1, writable: true, configurable: true });
Object.defineProperty(globalThis, "frozen", { value: 1, writable: true, configurable: true });
Object.defineProperty(globalThis, "readonly", { value: 1, writable: false, configurable: true });
Object.defineProperty(globalThis, "accessor", {
  get: function () { return 2; },
  configurable: true,
});

function readSealed() { return sealed; }
function writeSealed(v) { sealed = v; }
function readFrozen() { return frozen; }
function writeFrozen(v) { frozen = v; }
function readAccessor() { return accessor; }
function writeReadonly(v) { readonly = v; }
function createNew() { brandNew = 1; }

// Warm every callsite before changing the integrity level.
writeSealed(2);
assert.sameValue(readSealed(), 2);
writeFrozen(2);
assert.sameValue(readFrozen(), 2);
assert.sameValue(readAccessor(), 2);
writeReadonly(2);
assert.sameValue(globalThis.readonly, 1);

Object.preventExtensions(globalThis);

assert.sameValue(Object.isExtensible(globalThis), false);

// Existing writable properties still accept cached stores.
writeSealed(3);
assert.sameValue(readSealed(), 3);

// Existing non-writable properties still reject stores.
writeReadonly(3);
assert.sameValue(globalThis.readonly, 1);

// A new global property cannot be created, and the store is silently ignored in sloppy mode.
createNew();
assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "brandNew"), false);
createNew();
assert.sameValue(typeof brandNew, "undefined");

// The same store throws in strict mode.
(function () {
  "use strict";
  assert.throws(TypeError, function () { globalThis.brandNew2 = 1; });
  assert.sameValue(Object.prototype.hasOwnProperty.call(globalThis, "brandNew2"), false);
})();

// Defining a new global property throws.
assert.throws(TypeError, function () {
  Object.defineProperty(globalThis, "brandNew3", { value: 1 });
});

Object.seal(globalThis);

assert.sameValue(Object.isSealed(globalThis), true);

// Sealing keeps data properties writable.
writeSealed(4);
assert.sameValue(readSealed(), 4);
assert.sameValue(globalThis.sealed, 4);

// Sealing makes properties non-configurable.
assert.sameValue(delete globalThis.sealed, false);
assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "sealed").configurable, false);

// The accessor still runs after sealing.
assert.sameValue(readAccessor(), 2);

Object.freeze(globalThis);

assert.sameValue(Object.isFrozen(globalThis), true);

// Freezing makes data properties non-writable, so cached stores become no-ops.
writeFrozen(5);
assert.sameValue(readFrozen(), 2);
writeFrozen(6);
assert.sameValue(readFrozen(), 2);
assert.sameValue(globalThis.frozen, 2);

// Cached reads of frozen data properties still return the frozen value.
assert.sameValue(readSealed(), 4);

// A frozen accessor property still calls its getter.
assert.sameValue(readAccessor(), 2);

// Reads and stores stay correct across a garbage collection.
$262.gc();
assert.sameValue(readFrozen(), 2);
assert.sameValue(readSealed(), 4);
assert.sameValue(readAccessor(), 2);
