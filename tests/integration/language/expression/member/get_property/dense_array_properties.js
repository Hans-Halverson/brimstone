/*---
description: GetProperty fast path for dense arrays.
---*/

// In-range loads of every value type.
(function () {
  function get(a, i) { return a[i]; }
  var obj = {};
  var a = [1, 2.5, "s", true, null, undefined, obj];
  for (var round = 0; round < 3; round++) {
    assert.sameValue(get(a, 0), 1);
    assert.sameValue(get(a, 1), 2.5);
    assert.sameValue(get(a, 2), "s");
    assert.sameValue(get(a, 3), true);
    assert.sameValue(get(a, 4), null);
    assert.sameValue(get(a, 5), undefined);
    assert.sameValue(get(a, 6), obj);
  }
})();

// Out of range loads are undefined and do not throw.
(function () {
  function get(a, i) { return a[i]; }
  var a = [1, 2, 3];
  get(a, 0); get(a, 0);
  assert.sameValue(get(a, 3), undefined);
  assert.sameValue(get(a, 1000), undefined);
})();

// Shrinking the array via length makes old indices read as undefined.
(function () {
  function get(a, i) { return a[i]; }
  var a = [1, 2, 3, 4];
  assert.sameValue(get(a, 3), 4);
  a.length = 2;
  assert.sameValue(get(a, 3), undefined);
  assert.sameValue(get(a, 1), 2);
})();

// Growing the array via length exposes holes, not stale values.
(function () {
  function get(a, i) { return a[i]; }
  var a = [1, 2, 3, 4];
  assert.sameValue(get(a, 3), 4);
  a.length = 2;
  a.length = 4;
  assert.sameValue(get(a, 3), undefined);
})();

// An accessor element converts the array to sparse storage, loads still work.
(function () {
  function get(a, i) { return a[i]; }
  var a = [1, 2, 3];
  assert.sameValue(get(a, 1), 2);
  Object.defineProperty(a, "0", { get: function () { return "got"; } });
  assert.sameValue(get(a, 0), "got");
  assert.sameValue(get(a, 1), 2);
})();

// A warm callsite handles array and non-array receivers interchangeably.
(function () {
  function get(o, i) { return o[i]; }
  var a = [1, 2, 3];
  var o = { 0: "a", 1: "b" };
  var s = "xy";
  assert.sameValue(get(a, 1), 2);
  assert.sameValue(get(o, 1), "b");
  assert.sameValue(get(s, 1), "y");
  assert.sameValue(get(a, 2), 3);
})();

// Array subclass indexed properties load like plain array indexed properties.
(function () {
  function get(a, i) { return a[i]; }
  class Sub extends Array {}
  var s = Sub.of(10, 20);
  assert.sameValue(get(s, 0), 10);
  assert.sameValue(get(s, 1), 20);
  assert.sameValue(get(s, 2), undefined);
})();

// Dense indexed property loads stay correct across a garbage collection.
(function () {
  function get(a, i) { return a[i]; }
  var a = [{ tag: 1 }, { tag: 2 }];
  assert.sameValue(get(a, 0).tag, 1);
  $262.gc();
  assert.sameValue(get(a, 0).tag, 1);
  assert.sameValue(get(a, 1).tag, 2);
})();

// A hole load finds a data property on Array.prototype, present indexed properties shadow it.
(function () {
  function get(a, i) { return a[i]; }
  Array.prototype[1] = "proto";
  try {
    var a = [0, , 2];
    get(a, 0); get(a, 0);
    assert.sameValue(get(a, 1), "proto");
    assert.sameValue(get(a, 0), 0);
    assert.sameValue(get(a, 2), 2);
  } finally {
    delete Array.prototype[1];
  }
})();

// A hole load calls a getter on Array.prototype with the array as receiver.
(function () {
  function get(a, i) { return a[i]; }
  var seenThis;
  Object.defineProperty(Array.prototype, "1", {
    get: function () { seenThis = this; return "got"; },
    configurable: true,
  });
  try {
    var a = [0, , 2];
    assert.sameValue(get(a, 1), "got");
    assert.sameValue(seenThis, a);
    assert.sameValue(get(a, 0), 0);
  } finally {
    delete Array.prototype[1];
  }
})();

// Deleting an indexed property re-introduces a hole that reads through the prototype again.
(function () {
  function get(a, i) { return a[i]; }
  Array.prototype[0] = "proto";
  try {
    var a = [1, 2];
    assert.sameValue(get(a, 0), 1);
    assert.sameValue(get(a, 0), 1);
    delete a[0];
    assert.sameValue(get(a, 0), "proto");
  } finally {
    delete Array.prototype[0];
  }
})();

// Integral float and negative zero keys canonicalize to the same indexed property.
(function () {
  function get(a, k) { return a[k]; }
  var a = [1, 2, 3];
  assert.sameValue(get(a, -0), 1);
})();

// Fractional and negative keys are not indexed properties.
(function () {
  function get(a, k) { return a[k]; }
  var a = [1, 2, 3];
  assert.sameValue(get(a, 1.5), undefined);
  assert.sameValue(get(a, -1), undefined);
})();
