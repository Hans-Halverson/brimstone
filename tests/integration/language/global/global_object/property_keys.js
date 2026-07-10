/*---
description: >
  Own property keys of the global object are indexed keys, then string keys in creation order, then
  symbol keys.
---*/

var sym1 = Symbol("sym1");
var sym2 = Symbol("sym2");

globalThis[3] = "three";
globalThis[1] = "one";
globalThis[sym1] = 1;
globalThis.strA = 1;
globalThis.strB = 2;
globalThis[sym2] = 2;

var keys = Reflect.ownKeys(globalThis);

// Integer indices come first, in ascending numeric order.
assert.sameValue(keys[0], "1");
assert.sameValue(keys[1], "3");

// String keys follow, in creation order.
var indexA = keys.indexOf("strA");
var indexB = keys.indexOf("strB");
assert.sameValue(indexA > 1, true);
assert.sameValue(indexA < indexB, true);

// Symbol keys come last, in creation order.
var indexSym1 = keys.indexOf(sym1);
var indexSym2 = keys.indexOf(sym2);
assert.sameValue(indexSym1 > indexB, true);
assert.sameValue(indexSym1 < indexSym2, true);

// Deleting and recreating a string key moves it to the end of the string keys.
delete globalThis.strA;
globalThis.strA = 3;
var names = Object.getOwnPropertyNames(globalThis);
assert.sameValue(names.indexOf("strB") < names.indexOf("strA"), true);

// getOwnPropertyNames returns only string keys, getOwnPropertySymbols only symbol keys.
assert.sameValue(Object.getOwnPropertyNames(globalThis).indexOf(sym1), -1);
assert.sameValue(Object.getOwnPropertySymbols(globalThis).indexOf("strA"), -1);
assert.sameValue(Object.getOwnPropertySymbols(globalThis).indexOf(sym1) >= 0, true);

// Deleted keys are not enumerated.
delete globalThis.strB;
assert.sameValue(Object.getOwnPropertyNames(globalThis).indexOf("strB"), -1);
delete globalThis[sym1];
assert.sameValue(Object.getOwnPropertySymbols(globalThis).indexOf(sym1), -1);
delete globalThis[1];
assert.sameValue(Object.getOwnPropertyNames(globalThis).indexOf("1"), -1);

// Object.keys only returns enumerable string keys.
Object.defineProperty(globalThis, "hidden", { value: 1, enumerable: false, configurable: true });
assert.sameValue(Object.keys(globalThis).indexOf("hidden"), -1);
assert.sameValue(Object.getOwnPropertyNames(globalThis).indexOf("hidden") >= 0, true);

// Enumerating the global object with for-in visits its enumerable string keys.
globalThis.enumerated = 1;
var sawEnumerated = false;
var sawHidden = false;
for (var key in globalThis) {
  if (key === "enumerated") { sawEnumerated = true; }
  if (key === "hidden") { sawHidden = true; }
}
assert.sameValue(sawEnumerated, true);
assert.sameValue(sawHidden, false);

// A large number of global properties enumerates in creation order.
for (var i = 0; i < 200; i++) {
  globalThis["many" + i] = i;
}
var collected = Object.getOwnPropertyNames(globalThis).filter(function (name) {
  return name.indexOf("many") === 0;
});
assert.sameValue(collected.length, 200);
assert.sameValue(collected[0], "many0");
assert.sameValue(collected[199], "many199");

// Enumeration order survives a garbage collection.
$262.gc();
var afterGc = Object.getOwnPropertyNames(globalThis).filter(function (name) {
  return name.indexOf("many") === 0;
});
assert.sameValue(afterGc.join(","), collected.join(","));
