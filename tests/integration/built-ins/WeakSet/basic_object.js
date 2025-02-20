/*---
description: Weak set removes dead element after GC, object target.
---*/

var object = {};
var weakSet = newWeakSet();

function newWeakSet() {
  var set = new WeakSet();
  set.add(object);
  return set;
}

// Before nulling out the only reference to the element, the element survives GCs.
(() => {
  $262.gc();
  assert(weakSet.has(object));
})();

// After nulling out the only reference to the element, the element is removed with undefined.
(() => {
  object = null;
  $262.gc();
  assert(!weakSet.has(object));
})();