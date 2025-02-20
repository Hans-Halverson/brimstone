/*---
description: FinalizationRegistry is weak map from target value to unregister token, verify token is GC'd when appropriate.
---*/

var key = {};
var value = {};
var token = {};
var valueRef = (() => new WeakRef(value))();
var tokenRef = (() => new WeakRef(token))();
var registry = newRegistry();

function newRegistry() {
  var registry = new FinalizationRegistry(() => {});
  registry.register(key, value, token);
  return registry;
}

// Before nulling out the only reference to the key the value and token survives.
(() => {
  value = null;
  token = null;
  $262.gc();
  assert.notSameValue(valueRef.deref(), undefined);
  assert.notSameValue(tokenRef.deref(), undefined);
})();

// After nulling out the only reference to the key the token is collected but the value survives.
(() => {
  key = null;
  $262.gc();
  assert.notSameValue(valueRef.deref(), undefined);
  assert.sameValue(tokenRef.deref(), undefined);
})();