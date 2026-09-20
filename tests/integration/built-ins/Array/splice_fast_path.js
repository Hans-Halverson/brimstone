/*---
description: Array.prototype.splice fast path for dense arrays.
includes: [compareArray.js]
---*/

function ownKeys(array) {
  return Object.keys(array).join(",");
}

// Splice an array, checking both the removed elements and what is left behind
function assertSplice(array, args, removed, remaining, message) {
  var result = Array.prototype.splice.apply(array, args);
  assert(Array.isArray(result), message + ": result is an array");
  assert.compareArray(result, removed, message + ": removed");
  assert.compareArray(array, remaining, message + ": remaining");
}

// Elements are removed and inserted in place
assertSplice([0, 1, 2, 3], [1, 2, "a", "b"], [1, 2], [0, "a", "b", 3], "equal counts");
assertSplice([0, 1, 2, 3], [1, 1, "a", "b"], [1], [0, "a", "b", 2, 3], "more inserted than deleted");
assertSplice([0, 1, 2, 3], [1, 2], [1, 2], [0, 3], "fewer inserted than deleted");
assertSplice([0, 1], [1, 0, "a"], [], [0, "a", 1], "inserted without deleting");
assertSplice([0, 1, 2], [1], [1, 2], [0], "deleted through the end");
assertSplice([0, 1, 2], [3, 0, "a"], [], [0, 1, 2, "a"], "inserted at the end");
assertSplice([0, 1, 2], [], [], [0, 1, 2], "no arguments");

// Holes are not copied into the result, and are preserved when elements move
var deletedHole = [0, , 2, 3];
var deletedHoleRemoved = deletedHole.splice(1, 2);
assert.sameValue(deletedHoleRemoved.length, 2, "deleted hole: removed length");
assert.sameValue(ownKeys(deletedHoleRemoved), "1", "deleted hole: removed keys");
assert.compareArray(deletedHole, [0, 3], "deleted hole: remaining");

var movedHole = [0, 1, , 3];
movedHole.splice(1, 1);
assert.sameValue(movedHole.length, 3, "moved hole: length");
assert.sameValue(ownKeys(movedHole), "0,2", "moved hole: keys");

// Arrays large enough that growing and shrinking reallocates their storage
var large = [];
for (var i = 0; i < 100; i++) {
  large.push(i);
}
large.splice(50, 0, "a", "b", "c");
assert.sameValue(large.length, 103, "grown: length");
assert.compareArray(large.slice(49, 54), [49, "a", "b", "c", 50], "grown: around the insert");
large.splice(5, 90);
assert.sameValue(large.length, 13, "shrunk: length");
assert.compareArray(large.slice(0, 7), [0, 1, 2, 3, 4, 92, 93], "shrunk: around the delete");

// A subclass uses the fast path too, with its species creating the result
class MyArray extends Array {}
var subclass = MyArray.from([0, 1, 2]);
var subclassRemoved = subclass.splice(1, 1);
assert(subclassRemoved instanceof MyArray, "subclass: result is a subclass instance");
assert.compareArray(Array.from(subclassRemoved), [1], "subclass: removed");
assert.compareArray(Array.from(subclass), [0, 2], "subclass: remaining");

// Receivers without dense array storage
var arrayLike = { length: 3, 0: "a", 1: "b", 2: "c" };
assert.compareArray(Array.prototype.splice.call(arrayLike, 1, 1), ["b"], "array-like: removed");
assert.sameValue(arrayLike.length, 2, "array-like: length");
assert.sameValue(arrayLike[1], "c", "array-like: moved");

// The slow path grows and replaces too, moving around properties that are missing
var grownLike = { length: 3, 0: "a", 2: "c" };
assert.compareArray(Array.prototype.splice.call(grownLike, 0, 0, "x"), [], "array-like grow: removed");
assert.sameValue(grownLike.length, 4, "array-like grow: length");
assert.sameValue(grownLike[0], "x", "array-like grow: inserted");
assert.sameValue(grownLike[1], "a", "array-like grow: moved up");
assert.sameValue(2 in grownLike, false, "array-like grow: missing property stays missing");
assert.sameValue(grownLike[3], "c", "array-like grow: moved to the end");

var replacedLike = { length: 2, 0: "a", 1: "b" };
assert.compareArray(Array.prototype.splice.call(replacedLike, 0, 1, "z"), ["a"],
  "array-like replace: removed");
assert.sameValue(replacedLike.length, 2, "array-like replace: length");
assert.sameValue(replacedLike[0], "z", "array-like replace: inserted");
assert.sameValue(replacedLike[1], "b", "array-like replace: unchanged");

// Spliced near the end so the slow path only walks a couple of indices
var sparse = [];
sparse[0] = 0;
sparse[2000] = 1;
var sparseRemoved = sparse.splice(1998, 1);
assert.sameValue(sparseRemoved.length, 1, "sparse: removed length");
assert.sameValue(ownKeys(sparseRemoved), "", "sparse: removed a hole");
assert.sameValue(sparse.length, 2000, "sparse: length");
assert.sameValue(sparse[1999], 1, "sparse: moved");

// Arrays that cannot have properties added or their length changed
var nonExtensible = Object.preventExtensions([0, 1, 2]);
assert.compareArray(nonExtensible.splice(0, 1), [0], "non-extensible: removed");
assert.compareArray(nonExtensible, [1, 2], "non-extensible: remaining");
assert.throws(TypeError, function () { Object.preventExtensions([0, 1]).splice(0, 0, "a"); },
  "non-extensible: growing throws");

var fixedLength = [0, 1, 2];
Object.defineProperty(fixedLength, "length", { writable: false });
assert.throws(TypeError, function () { fixedLength.splice(0, 1); }, "non-writable length throws");

// A prototype that can intercept indexed access
var proxyProto = [0, 1, 2];
Object.setPrototypeOf(proxyProto, new Proxy({}, {}));
assert.compareArray(Array.prototype.splice.call(proxyProto, 0, 1), [0], "proxy prototype: removed");
assert.sameValue(proxyProto.length, 2, "proxy prototype: length");

// The length changing while the arguments are converted
var resized = [0, 1, 2, 3];
var resizedRemoved = resized.splice({ valueOf: function () { resized.length = 2; return 0; } }, 3);
assert.sameValue(resizedRemoved.length, 3, "resized while converting: removed length");
assert.sameValue(ownKeys(resizedRemoved), "0,1", "resized while converting: removed keys");
assert.sameValue(resized.length, 1, "resized while converting: remaining length");

// Species constructors returning a result the fast path cannot write into
function withSpecies(species) {
  var array = [0, 1, 2, 3];
  array.constructor = { [Symbol.species]: species };
  return array;
}

var wrongLength = withSpecies(function () { return []; });
assert.compareArray(wrongLength.splice(1, 2), [1, 2], "species with the wrong length: removed");
assert.compareArray(wrongLength, [0, 3], "species with the wrong length: remaining");

var notAnArray = withSpecies(function () { return { marker: true }; });
var notAnArrayRemoved = notAnArray.splice(1, 2);
assert.sameValue(notAnArrayRemoved.marker, true, "non-array species: result object");
assert.sameValue(notAnArrayRemoved.length, 2, "non-array species: result length");
assert.compareArray(notAnArray, [0, 3], "non-array species: remaining");

// The result must differ from the array being spliced. Deleting the whole array makes the
// result the length the fast path expects, so only the identity check rejects it.
var sameArray = [0, 1];
sameArray.constructor = { [Symbol.species]: function () { return sameArray; } };
assert.sameValue(sameArray.splice(0, 2), sameArray, "species returning the spliced array");
assert.sameValue(sameArray.length, 0, "species returning the spliced array: length");

// Runs last: an indexed property on Array.prototype keeps its indexed storage non-empty even
// after being deleted, which disables the fast path for every array in the realm
Array.prototype[1] = "from prototype";
var protoHole = [0, , 2];
var protoHoleRemoved = protoHole.splice(0, 2);
assert.compareArray(protoHoleRemoved, [0, "from prototype"], "prototype property: removed");
assert.sameValue(ownKeys(protoHoleRemoved), "0,1", "prototype property: removed keys");
delete Array.prototype[1];
