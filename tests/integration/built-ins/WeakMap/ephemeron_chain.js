/*---
description: Weak map kv pairs being removed may cause another set of kv pairs to now be dead and should be collected.
---*/

// Set up chain of three kv pairs that are only reachable through the first key.
var key1 = {};
var key2 = {};
var key3 = {};
var value3 = {};

// Weak refs to detect when the keys are collected.
var weakRefKey2 = (() => new WeakRef(key2))();
var weakRefKey3 = (() => new WeakRef(key3))();

var weakMap1 = newWeakMap1();
var weakMap2 = newWeakMap2();

function newWeakMap1() {
  var weakMap = new WeakMap();
  weakMap.set(key1, key2);
  weakMap.set(key2, key3);
  return weakMap;
}

function newWeakMap2() {
  var weakMap = new WeakMap();
  weakMap.set(key3, value3);
  return weakMap;
}

// Before nulling out the only reference to the key the kv pair chain survives.
(() => {
  $262.gc();
  assert(weakMap1.has(key1));
  assert.sameValue(weakMap1.get(key1), key2);
  assert(weakMap1.has(key2));
  assert.sameValue(weakMap1.get(key2), key3);
  assert(weakMap2.has(key3));
  assert.sameValue(weakMap2.get(key3), value3);
})();

// Entire chain survives since the first key is still reachable.
(() => {
  key2 = null;
  key3 = null;
  $262.gc();
  assert(weakMap1.has(key1));
  assert.sameValue(weakMap1.get(key1), weakRefKey2.deref());
  assert(weakMap1.has(weakRefKey2.deref()));
  assert.sameValue(weakMap1.get(weakRefKey2.deref()), weakRefKey3.deref());
  assert(weakMap2.has(weakRefKey3.deref()));
  assert.sameValue(weakMap2.get(weakRefKey3.deref()), value3);
})();

// After nulling out the only reference to the key the kv pair chain is removed.
(() => {
  key1 = null;
  $262.gc();
  assert(!weakMap1.has(key1));
  assert.sameValue(weakMap1.get(key1), undefined);
  assert(!weakMap1.has(key2));
  assert.sameValue(weakMap1.get(key2), undefined);
  assert(!weakMap2.has(key3));
  assert.sameValue(weakMap2.get(key3), undefined);
})();