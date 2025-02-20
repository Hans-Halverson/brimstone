/*---
description: Weak map keys and values are GC'd when key is unreferenced, object target.
---*/

var objectKey = {};
var objectValue = {};
var weakMap = newWeakMap();

function newWeakMap() {
  var weakMap = new WeakMap();
  weakMap.set(objectKey, objectValue);
  return weakMap;
}

// Before nulling out the only reference to the key the weak map kv pair survives.
(() => {
  $262.gc();
  assert(weakMap.has(objectKey));
  assert.sameValue(weakMap.get(objectKey), objectValue);
})();

// After nulling out the only reference to the key the weak map kv pair is removed.
(() => {
  objectKey = null;
  $262.gc();
  assert(!weakMap.has(objectKey));
  assert.sameValue(weakMap.get(objectKey), undefined);
});