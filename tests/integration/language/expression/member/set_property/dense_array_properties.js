/*---
description: SetProperty fast path for dense arrays.
---*/

// In-range overwrites of every value type.
(function () {
  function set(a, i, v) { a[i] = v; }
  var obj = {};
  var a = [0, 0, 0, 0, 0, 0, 0];
  var values = [1, 2.5, "s", true, null, undefined, obj];
  for (var round = 0; round < 3; round++) {
    for (var i = 0; i < values.length; i++) {
      set(a, i, values[i]);
    }
  }
  for (var i = 0; i < values.length; i++) {
    assert.sameValue(a[i], values[i]);
  }
})();

// In-range stores do not change the length.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = [1, 2, 3];
  set(a, 1, "y");
  set(a, 1, "z");
  assert.sameValue(a[1], "z");
  assert.sameValue(a.length, 3);
})();

// Stores past the end extend the array and update the length.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = [1];
  set(a, 0, 2); set(a, 0, 3);
  set(a, 3, "x");
  assert.sameValue(a.length, 4);
  assert.sameValue(a[3], "x");
  assert.sameValue(a[1], undefined);
})();

// Frozen arrays throw on store in strict mode.
(function () {
  "use strict";
  var a = Object.freeze([1, 2, 3]);
  function set(x, i, v) { x[i] = v; }
  assert.throws(TypeError, function () { set(a, 0, 99); });
  assert.throws(TypeError, function () { set(a, 0, 99); });
  assert.sameValue(a[0], 1);
})();

// A warm callsite handles array and non-array receivers interchangeably.
(function () {
  function set(o, i, v) { o[i] = v; }
  var a = [1, 2, 3];
  var o = {};
  set(a, 0, "a1");
  set(a, 0, "a2");
  set(o, 0, "o1");
  assert.sameValue(a[0], "a2");
  assert.sameValue(o[0], "o1");
})();

// Array subclass indexed properties store like plain array indexed properties.
(function () {
  function set(a, i, v) { a[i] = v; }
  class Sub extends Array {}
  var s = Sub.of(1, 2);
  set(s, 0, 10);
  set(s, 0, 11);
  assert.sameValue(s[0], 11);
  assert.sameValue(s.length, 2);
})();

// Stored heap values survive a garbage collection.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = [null, null];
  set(a, 0, { tag: 1 });
  set(a, 1, { tag: 2 });
  $262.gc();
  assert.sameValue(a[0].tag, 1);
  assert.sameValue(a[1].tag, 2);
})();

// A hole store is intercepted by a setter on Array.prototype, no own indexed property is created.
(function () {
  function set(a, i, v) { a[i] = v; }
  var calls = 0, seenThis, last;
  Object.defineProperty(Array.prototype, "1", {
    set: function (v) { calls++; seenThis = this; last = v; },
    configurable: true,
  });
  try {
    var a = [0, , 2];
    set(a, 0, 10);
    set(a, 0, 11);
    set(a, 1, "trapped");
    assert.sameValue(calls, 1);
    assert.sameValue(seenThis, a);
    assert.sameValue(last, "trapped");
    assert.sameValue(a.hasOwnProperty("1"), false);
    // Present indexed properties shadow the prototype setter.
    set(a, 2, 20);
    assert.sameValue(calls, 1);
    assert.sameValue(a[2], 20);
  } finally {
    delete Array.prototype[1];
  }
})();

// Without prototype interference, a hole store creates an own indexed property that later
// stores treat as present.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = [0, , 2];
  assert.sameValue(a[1], undefined);
  set(a, 1, "own");
  assert.sameValue(a[1], "own");
  set(a, 1, "own2");
  assert.sameValue(a[1], "own2");
})();

// Integral float and negative zero keys canonicalize to the same indexed property.
(function () {
  function set(a, k, v) { a[k] = v; }
  var a = [1, 2, 3];
  set(a, -0, "zero");
  assert.sameValue(a[0], "zero");
  assert.sameValue(a.length, 3);
})();

// Fractional and negative keys do not touch indexed properties or the length.
(function () {
  function set(a, k, v) { a[k] = v; }
  var a = [1, 2, 3];
  set(a, 1.5, "frac");
  set(a, -1, "neg");
  assert.sameValue(a[1], 2);
  assert.sameValue(a.length, 3);
})();
