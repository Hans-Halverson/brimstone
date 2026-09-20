/*---
description: ArraySpeciesCreate fast path for arrays with the default species behavior.
includes: [compareArray.js]
---*/

var constructorDesc = Object.getOwnPropertyDescriptor(Array.prototype, "constructor");
var speciesDesc = Object.getOwnPropertyDescriptor(Array, Symbol.species);

function Custom(length) {
  this.createdWithLength = length;
}

var customConstructor = { [Symbol.species]: Custom };

function assertOrdinary(result, expected, message) {
  assert(Array.isArray(result), message + ": is an array");
  assert.sameValue(Object.getPrototypeOf(result), Array.prototype, message + ": prototype");
  assert.compareArray(result, expected, message);
}

function assertCustom(result, expectedLength, message) {
  assert(result instanceof Custom, message + ": created by the species constructor");
  assert.sameValue(result.createdWithLength, expectedLength, message + ": length argument");
}

function restoreConstructor() {
  Object.defineProperty(Array.prototype, "constructor", constructorDesc);
}

function restoreSpecies() {
  Object.defineProperty(Array, Symbol.species, speciesDesc);
}

// An array with enough named properties to use map mode storage
function mapModeArray() {
  var array = [1, 2, 3];
  for (var i = 0; i < 70; i++) {
    array["p" + i] = i;
  }
  return array;
}

// Default behavior, repeated to use both the computed and the cached state
assertOrdinary([1, 2, 3].slice(1), [2, 3], "slice");
assertOrdinary([1, 2, 3].slice(1), [2, 3], "slice again");
assertOrdinary([1, 2, 3].splice(0, 1), [1], "splice");

// Receivers that never take the fast path
assertOrdinary(Array.prototype.slice.call({ length: 2, 0: "a", 1: "b" }), ["a", "b"],
  "array-like receiver");

class MyArray extends Array {}
assert(MyArray.from([1, 2, 3]).slice(1) instanceof MyArray, "subclass uses its own species");

// An own constructor shadows the prototype's, and is only looked up if the array has own names
var withProperty = [1, 2, 3];
withProperty.unrelated = 1;
assertOrdinary(withProperty.slice(1), [2, 3], "own unrelated property");
withProperty.constructor = customConstructor;
assertCustom(withProperty.slice(1), 2, "own constructor");

// Same, in map mode storage
var mapMode = mapModeArray();
assertOrdinary(mapMode.slice(1), [2, 3], "map mode array");
mapMode.constructor = customConstructor;
assertCustom(mapMode.slice(1), 2, "map mode array, own constructor");

// Array.prototype.constructor variations; each mutation invalidates the cache
Array.prototype.constructor = customConstructor;
assertCustom([1, 2, 3].slice(1), 2, "replaced constructor");
assertCustom([1, 2, 3].slice(1), 2, "replaced constructor, cached");
restoreConstructor();
assertOrdinary([1, 2, 3].slice(1), [2, 3], "restored constructor");

Array.prototype.constructor = 5;
assert.throws(TypeError, function () { [1, 2, 3].slice(1); }, "primitive constructor");
restoreConstructor();

delete Array.prototype.constructor;
assertOrdinary([1, 2, 3].slice(1), [2, 3], "deleted constructor");
restoreConstructor();

// An accessor constructor must have its getter called
var constructorGets = 0;
Object.defineProperty(Array.prototype, "constructor", {
  get: function () { constructorGets++; return Array; },
  configurable: true,
});
assertOrdinary([1, 2, 3].slice(1), [2, 3], "accessor constructor");
assert.sameValue(constructorGets, 1, "accessor constructor was called");
restoreConstructor();

// A write to Object.prototype invalidates the cache through the prototype chain
Object.prototype.unrelated = 1;
assertOrdinary([1, 2, 3].slice(1), [2, 3], "after writing to Object.prototype");
delete Object.prototype.unrelated;

// Array[Symbol.species] variations, while Array still uses array mode storage
Object.defineProperty(Array, Symbol.species, { value: Custom, configurable: true });
assertCustom([1, 2, 3].slice(1), 2, "species data property");
restoreSpecies();

// `get` must be named explicitly: redefining an accessor without it keeps the old getter
Object.defineProperty(Array, Symbol.species, {
  get: undefined,
  set: function () {},
  configurable: true,
});
assertOrdinary([1, 2, 3].slice(1), [2, 3], "species accessor with no getter");
restoreSpecies();

Object.defineProperty(Array, Symbol.species, {
  get: function () { return Custom; },
  configurable: true,
});
assertCustom([1, 2, 3].slice(1), 2, "species getter returning a constructor");
restoreSpecies();

// Any getter that returns its receiver is the default behavior, not just Array's own
Object.defineProperty(Array, Symbol.species, Object.getOwnPropertyDescriptor(Map, Symbol.species));
assertOrdinary([1, 2, 3].slice(1), [2, 3], "species getter borrowed from Map");
restoreSpecies();

// Deleting a property moves Array into map mode permanently, so these run last
delete Array[Symbol.species];
assertOrdinary([1, 2, 3].slice(1), [2, 3], "deleted species");
restoreSpecies();
assertOrdinary([1, 2, 3].slice(1), [2, 3], "species restored, map mode");

Object.defineProperty(Array, Symbol.species, { value: Custom, configurable: true });
assertCustom([1, 2, 3].slice(1), 2, "species data property, map mode");
restoreSpecies();
