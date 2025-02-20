/*---
description: Weak map keys and values are removed when key is unreferenced, symbol target.
---*/

var symbolKey = Symbol('key');
var symbolValue = Symbol('value');
var weakMap = newWeakMap();

function newWeakMap() {
  var weakMap = new WeakMap();
  weakMap.set(symbolKey, symbolValue);
  return weakMap;
}

// Before nulling out the only reference to the key the weak map kv pair survives.
(() => {
  $262.gc();
  assert(weakMap.has(symbolKey));
  assert.sameValue(weakMap.get(symbolKey), symbolValue);
})();

// After nulling out the only reference to the key the weak map kv pair is removed.
(() => {
  symbolKey = null;
  $262.gc();
  assert(!weakMap.has(symbolKey));
  assert.sameValue(weakMap.get(symbolKey), undefined);
})();