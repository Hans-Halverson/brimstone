/*---
description: Weak set removes dead element after GC, symbol target.
---*/

var symbol = Symbol();
var weakSet = newWeakSet();

function newWeakSet() {
  var set = new WeakSet();
  set.add(symbol);
  return set;
}

// Before nulling out the only reference to the element, the element survives GCs.
(() => {
  $262.gc();
  assert(weakSet.has(symbol));
})();

// After nulling out the only reference to the element, the element is removed with undefined.
(() => {
  symbol = null;
  $262.gc();
  assert(!weakSet.has(symbol));
})();