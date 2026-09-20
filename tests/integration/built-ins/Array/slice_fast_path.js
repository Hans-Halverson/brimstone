/*---
description: Array.prototype.slice fast path for dense arrays.
includes: [compareArray.js]
---*/

function ownKeys(array) {
  return Object.keys(array).join(",");
}

// Dense arrays are copied directly out of their storage
assert.compareArray([0, 1, 2, 3].slice(1, 3), [1, 2], "middle of the array");
assert.compareArray([0, 1, 2].slice(), [0, 1, 2], "no arguments");
assert.compareArray([0, 1, 2, 3].slice(-3, -1), [1, 2], "relative to the end");
assert.compareArray([0, 1, 2].slice(1, 10), [1, 2], "end past the length");
assert.compareArray([0, 1, 2].slice(5), [], "start past the length");
assert.compareArray([0, 1, 2].slice(2, 1), [], "end before the start");

// Holes are not copied into the result, so they stay holes
var holes = [0, , 2];
var holesResult = holes.slice(0, 3);
assert.sameValue(holesResult.length, 3, "holes: length");
assert.sameValue(ownKeys(holesResult), "0,2", "holes: keys");

// A subclass uses the fast path too, with its species creating the result
class MyArray extends Array {}
var subclass = MyArray.from([0, 1, 2]);
var subclassResult = subclass.slice(1);
assert(subclassResult instanceof MyArray, "subclass: result is a subclass instance");
assert.compareArray(Array.from(subclassResult), [1, 2], "subclass: contents");

// Receivers without dense array storage
var arrayLike = { length: 3, 0: "a", 2: "c" };
var arrayLikeResult = Array.prototype.slice.call(arrayLike, 0, 3);
assert.sameValue(arrayLikeResult.length, 3, "array-like: length");
assert.sameValue(ownKeys(arrayLikeResult), "0,2", "array-like: missing property stays missing");
assert.compareArray(Array.prototype.slice.call(arrayLike, 3, 1), [], "array-like: empty range");

assert.compareArray(Array.prototype.slice.call("abc", 1), ["b", "c"], "string receiver");

// Sliced near the end so the slow path only walks a couple of indices
var sparse = [];
sparse[0] = 0;
sparse[2000] = 1;
var sparseResult = sparse.slice(1999);
assert.sameValue(sparseResult.length, 2, "sparse: length");
assert.sameValue(ownKeys(sparseResult), "1", "sparse: keys");
assert.sameValue(sparseResult[1], 1, "sparse: copied element");

// The length changing while the arguments are converted
var resized = [0, 1, 2, 3];
var resizedResult = resized.slice(0, { valueOf: function () { resized.length = 2; return 4; } });
assert.sameValue(resizedResult.length, 4, "resized while converting: length");
assert.sameValue(ownKeys(resizedResult), "0,1", "resized while converting: keys");

// Species constructors returning a result the fast path cannot write into
function withSpecies(species) {
  var array = [0, 1, 2, 3];
  array.constructor = { [Symbol.species]: species };
  return array;
}

var wrongLength = withSpecies(function () { return []; });
assert.compareArray(wrongLength.slice(1, 3), [1, 2], "species with the wrong length");

var notAnArray = withSpecies(function () { return { marker: true }; });
var notAnArrayResult = notAnArray.slice(1, 3);
assert.sameValue(notAnArrayResult.marker, true, "non-array species: result object");
assert.sameValue(notAnArrayResult.length, 2, "non-array species: result length");
assert.sameValue(notAnArrayResult[0], 1, "non-array species: copied element");

// The result must differ from the array being sliced. Slicing the whole array makes the result
// the length the fast path expects, so only the identity check rejects it.
var sameArray = [0, 1];
sameArray.constructor = { [Symbol.species]: function () { return sameArray; } };
assert.sameValue(sameArray.slice(0, 2), sameArray, "species returning the sliced array");
assert.compareArray(sameArray, [0, 1], "species returning the sliced array: contents");

// Runs last: an indexed property on Array.prototype keeps its indexed storage non-empty even
// after being deleted, which disables the fast path for every array in the realm
Array.prototype[1] = "from prototype";
var inherited = [0, , 2];
assert.compareArray(inherited.slice(0, 3), [0, "from prototype", 2], "prototype property");
delete Array.prototype[1];
