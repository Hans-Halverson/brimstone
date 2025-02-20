/*---
description: Weak map returns undefined after GC, symbol target.
---*/

var symbol = Symbol();
var weakRef = newWeakRef();

function newWeakRef() {
  return new WeakRef(symbol);
}

// Before nulling out the only reference to the target, the target survives GCs.
(() => {
  $262.gc();
  assert.sameValue(weakRef.deref(), symbol);
})();

// After nulling out the only reference to the target, the target is replaced with undefined.
(() => {
  symbol = null;
  $262.gc();
  assert.sameValue(weakRef.deref(), undefined);
})();