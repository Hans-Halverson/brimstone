/*---
description: SetProperty fast path for dense arrays in non-strict mode.
flags: [noStrict]
---*/

// Frozen arrays silently ignore stores in sloppy mode, even at a warm callsite.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = [1, 2, 3];
  set(a, 0, 10);
  set(a, 0, 20);
  Object.freeze(a);
  set(a, 0, 99);
  set(a, 1, 99);
  assert.sameValue(a[0], 20);
  assert.sameValue(a[1], 2);
})();

// A single non-writable indexed property rejects stores while its neighbors accept them.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = [1, 2, 3];
  set(a, 1, 20);
  Object.defineProperty(a, "1", { writable: false });
  set(a, 0, 10);
  set(a, 1, 99);
  set(a, 2, 30);
  assert.sameValue(a[0], 10);
  assert.sameValue(a[1], 20);
  assert.sameValue(a[2], 30);
})();

// A hole store with a getter-only accessor on Array.prototype is silently ignored in
// sloppy mode and throws in strict mode.
(function () {
  Object.defineProperty(Array.prototype, "1", {
    get: function () { return "got"; },
    configurable: true,
  });
  try {
    var a = [0, , 2];
    a[1] = "dropped";
    assert.sameValue(a.hasOwnProperty("1"), false);
    assert.sameValue(a[1], "got");

    (function () {
      "use strict";
      function set(x, i, v) { x[i] = v; }
      assert.throws(TypeError, function () { set(a, 1, "boom"); });
    })();
  } finally {
    delete Array.prototype[1];
  }
})();
