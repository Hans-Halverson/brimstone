/*---
description: Instruction caches hold weak references to cached heap items.
---*/

var proto = { f: 1 };
var weakRef = new WeakRef(proto);

function load(o) {
  return o.f;
}

(() => {
  var obj = Object.create(proto);
  for (var i = 0; i < 10; i++) {
    assert.sameValue(load(obj), 1);
  }
})();

proto = null;
$262.gc();
assert.sameValue(weakRef.deref(), undefined);
