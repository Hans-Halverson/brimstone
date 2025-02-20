/*---
description: FinalizationRegistry calls finalizer callback when key is GC'd.
flags: [async]
---*/

var called = [];

var registry = new FinalizationRegistry((heldValue) => called.push(heldValue));

var key1 = {};
var key2 = {};
var token2 = {};
var keyRef1 = (() => new WeakRef(key1))();
var keyRef2 = (() => new WeakRef(key2))();

(() => {
  registry.register(key1, "heldValue1");
  registry.register(key2, "heldValue2", token2);
})();

// Keys have not been collected yet as they are still referenced.
(() => {
  $262.gc();
  assert.notSameValue(keyRef1.deref(), undefined);
  assert.notSameValue(keyRef2.deref(), undefined);
})();

// Keys are collected, but only the still-registered finalizer callback is run.
(() => {
  key1 = null;
  key2 = null;

  registry.unregister(token2);
  $262.gc();

  assert.sameValue(keyRef1.deref(), undefined);
  assert.sameValue(keyRef2.deref(), undefined);
})();

// Verify the finalizer callback was run.
Promise.resolve().then(() => {
  assert.sameValue(called.length, 1);
  assert.sameValue(called[0], "heldValue1");
}).then($DONE, $DONE);