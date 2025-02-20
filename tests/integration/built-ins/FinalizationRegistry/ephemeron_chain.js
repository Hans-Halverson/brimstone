/*---
description: FinalizationRegistry is weak map from target value to unregister token. Collecting a token may cause another registered token to now be dead and should be collected.
---*/

// Set up chain of three kv pairs that are only reachable through the first key.
var key1 = {};
var key2 = {};
var key3 = {};
var token3 = {};

// Weak refs to detect when the keys are collected.
var key1Ref = (() => new WeakRef(key1))();
var key2Ref = (() => new WeakRef(key2))();
var key3Ref = (() => new WeakRef(key3))();
var token3Ref = (() => new WeakRef(token3))();

var registry1 = newRegistry1();
var registry2 = newRegistry2();

function newRegistry1() {
  var registry = new FinalizationRegistry(() => {});
  registry.register(key1, "", key2);
  registry.register(key2, "", key3);
  return registry;
}

function newRegistry2() {
  var registry = new FinalizationRegistry(() => {});
  registry.register(key3, "", token3);
  return registry;
}

// Before nulling out the only reference to the key the kv pair chain survives.
(() => {
  $262.gc();
  assert.notSameValue(key1Ref.deref(), undefined);
  assert.notSameValue(key2Ref.deref(), undefined);
  assert.notSameValue(key3Ref.deref(), undefined);
  assert.notSameValue(token3Ref.deref(), undefined);
});

// Entire chain survives since the first key is still reachable.
(() => {
  key2 = null;
  key3 = null;
  token3 = null;
  $262.gc();
  assert.notSameValue(key1Ref.deref(), undefined);
  assert.notSameValue(key2Ref.deref(), undefined);
  assert.notSameValue(key3Ref.deref(), undefined);
  assert.notSameValue(token3Ref.deref(), undefined);
})();

// After nulling out the only reference to the key the kv pair chain is removed.
(() => {
  key1 = null;
  $262.gc();
  assert.sameValue(key1Ref.deref(), undefined);
  assert.sameValue(key2Ref.deref(), undefined);
  assert.sameValue(key3Ref.deref(), undefined);
  assert.sameValue(token3Ref.deref(), undefined);
})();
