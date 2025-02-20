/*---
description: Weak map returns undefined after GC, object target.
---*/

var object = {};
var weakRef = newWeakRef();

function newWeakRef() {
  return new WeakRef(object);
}

// Before nulling out the only reference to the target, the target survives GCs.
(() => {
  $262.gc();
  assert.sameValue(weakRef.deref(), object);
})();

// After nulling out the only reference to the target, the target is replaced with undefined.
(() => {
  object = null;
  $262.gc();
  assert.sameValue(weakRef.deref(), undefined);
})();
