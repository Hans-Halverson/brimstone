/*---
description: GetNamedProperty cache on exotic objects.
---*/

// Array length - a warm callsite must see live updates, never a stale cached length.
(function () {
  function len(a) { return a.length; }
  var a = [1, 2];
  assert.sameValue(len(a), 2);
  assert.sameValue(len(a), 2);
  a.push(3);
  assert.sameValue(len(a), 3);
  a.length = 1;
  assert.sameValue(len(a), 1);
})();

// A non-length property on an array is ordinary and may be cached.
(function () {
  function get(a) { return a.foo; }
  var a = [];
  a.foo = 7;
  assert.sameValue(get(a), 7);
  assert.sameValue(get(a), 7);
})();

// A proxy in the chain must run its trap on every access, so it is never cached.
(function () {
  var count = 0;
  function get(o) { return o.x; }
  var proto = new Proxy({}, { get: function () { return ++count; } });
  var o = Object.create(proto);
  assert.sameValue(get(o), 1);
  assert.sameValue(get(o), 2);
  assert.sameValue(get(o), 3);
})();

// String object indexing - named access stays correct across different instances at one callsite.
(function () {
  function len(s) { return s.length; }
  assert.sameValue(len(new String("ab")), 2);
  assert.sameValue(len(new String("cdef")), 4);
})();

// Typed arrays intercept canonical numeric keys, so named access is never cached but stays correct.
(function () {
  function len(a) { return a.length; }
  function byteLen(a) { return a.byteLength; }
  var ta = new Uint8Array(4);
  assert.sameValue(len(ta), 4);
  assert.sameValue(len(ta), 4);
  assert.sameValue(byteLen(ta), 4);
})();

// An ordinary named property on a typed array reads correctly across a warm callsite.
(function () {
  function get(a) { return a.foo; }
  var ta = new Uint8Array(2);
  ta.foo = 7;
  assert.sameValue(get(ta), 7);
  assert.sameValue(get(ta), 7);
})();
