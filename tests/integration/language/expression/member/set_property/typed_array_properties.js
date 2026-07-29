/*---
description: SetProperty fast path for typed arrays.
---*/

// In-range stores roundtrip for every typed array kind.
(function () {
  function set(a, i, v) { a[i] = v; }
  var kinds = [Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array,
               Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array];
  for (var k = 0; k < kinds.length; k++) {
    var a = new kinds[k](4);
    set(a, 2, 42);
    set(a, 2, 43);
    assert.sameValue(a[2], 43, kinds[k].name);
    assert.sameValue(a[0], 0, kinds[k].name);
  }
})();

// Out-of-range integer stores wrap modularly, for both smi and double values.
(function () {
  function set(a, i, v) { a[i] = v; }
  var i8 = new Int8Array(4);
  set(i8, 0, 300);
  set(i8, 1, -129);
  set(i8, 2, 130.7);
  assert.sameValue(i8[0], 44);
  assert.sameValue(i8[1], 127);
  assert.sameValue(i8[2], -126);

  var u8 = new Uint8Array(2);
  set(u8, 0, 256);
  set(u8, 1, -1);
  assert.sameValue(u8[0], 0);
  assert.sameValue(u8[1], 255);

  var i16 = new Int16Array(1);
  set(i16, 0, 32768);
  assert.sameValue(i16[0], -32768);

  var u16 = new Uint16Array(1);
  set(u16, 0, 65536);
  assert.sameValue(u16[0], 0);

  var i32 = new Int32Array(2);
  set(i32, 0, 2147483648.5);
  set(i32, 1, -2147483649.5);
  assert.sameValue(i32[0], -2147483648);
  assert.sameValue(i32[1], 2147483647);

  var u32 = new Uint32Array(3);
  set(u32, 0, -1);
  set(u32, 1, 4294967297.5);
  set(u32, 2, -1.5);
  assert.sameValue(u32[0], 4294967295);
  assert.sameValue(u32[1], 1);
  assert.sameValue(u32[2], 4294967295);
})();

// Non-finite doubles store as zero in integer arrays.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = new Int32Array(3);
  set(a, 0, NaN);
  set(a, 1, Infinity);
  set(a, 2, -Infinity);
  assert.sameValue(a[0], 0);
  assert.sameValue(a[1], 0);
  assert.sameValue(a[2], 0);
})();

// Uint8Clamped stores clamp and round instead of wrapping.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = new Uint8ClampedArray(8);
  set(a, 0, 300);
  set(a, 1, -5);
  set(a, 2, 1.5);
  set(a, 3, 2.5);
  set(a, 4, 254.5);
  set(a, 5, NaN);
  set(a, 6, Infinity);
  set(a, 7, -Infinity);
  assert.sameValue(a[0], 255);
  assert.sameValue(a[1], 0);
  assert.sameValue(a[2], 2);
  assert.sameValue(a[3], 2);
  assert.sameValue(a[4], 254);
  assert.sameValue(a[5], 0);
  assert.sameValue(a[6], 255);
  assert.sameValue(a[7], 0);
})();

// Float16 stores round to the nearest half-precision value.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = new Float16Array(6);
  set(a, 0, 0.1);
  set(a, 1, 65505);
  set(a, 2, 100000);
  set(a, 3, NaN);
  set(a, 4, -0);
  set(a, 5, 1.5);
  assert.sameValue(a[0], 0.0999755859375);
  assert.sameValue(a[0], Math.f16round(0.1));
  assert.sameValue(a[1], 65504);
  assert.sameValue(a[2], Infinity);
  assert.sameValue(Number.isNaN(a[3]), true);
  assert.sameValue(a[4], -0);
  assert.sameValue(a[5], 1.5);
})();

// Negative zero stores preserve the sign in float arrays.
(function () {
  function set(a, i, v) { a[i] = v; }
  var f64 = new Float64Array(1);
  set(f64, 0, -0);
  assert.sameValue(f64[0], -0);
  var f32 = new Float32Array(1);
  set(f32, 0, -0);
  assert.sameValue(f32[0], -0);
})();

// Double stores into float arrays, including Float32 rounding and NaN.
(function () {
  function set(a, i, v) { a[i] = v; }
  var f64 = new Float64Array(2);
  set(f64, 0, 0.1);
  set(f64, 1, NaN);
  assert.sameValue(f64[0], 0.1);
  assert.sameValue(Number.isNaN(f64[1]), true);

  var f32 = new Float32Array(1);
  set(f32, 0, 0.1);
  assert.sameValue(f32[0], Math.fround(0.1));
})();

// Non-number values are converted with ToNumber.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = new Int32Array(4);
  set(a, 0, { valueOf: function () { return 7; } });
  set(a, 1, "5");
  set(a, 2, true);
  set(a, 3, null);
  assert.sameValue(a[0], 7);
  assert.sameValue(a[1], 5);
  assert.sameValue(a[2], 1);
  assert.sameValue(a[3], 0);
})();

// Invalid indices are silent no-ops in both sloppy and strict mode, no own indexed
// property is created and the prototype chain is never consulted.
(function () {
  "use strict";
  function set(a, i, v) { a[i] = v; }
  var a = new Int32Array([1, 2]);
  set(a, 0, 10);
  set(a, 2, 99);
  set(a, -1, 99);
  // Not a smi key, so this one is dropped by the slow path
  set(a, 0.5, 99);
  assert.sameValue(a[0], 10);
  assert.sameValue(a[1], 2);
  assert.sameValue(a.hasOwnProperty("2"), false);
  assert.sameValue(a.hasOwnProperty("-1"), false);
  assert.sameValue(a.length, 2);
})();

// Dropped stores never reach a setter inherited for the same index, including at the
// negative and huge indices that the fast path folds into a huge usize.
(function () {
  "use strict";
  function set(a, i, v) { a[i] = v; }
  var calls = 0;
  var descriptor = {
    set: function () { calls++; },
    get: function () { return "proto"; },
    configurable: true,
  };
  Object.defineProperty(Int32Array.prototype, "4", descriptor);
  Object.defineProperty(Int32Array.prototype, "-1", descriptor);
  Object.defineProperty(Int32Array.prototype, "2147483647", descriptor);
  try {
    var a = new Int32Array([1, 2]);
    set(a, 4, 99);
    set(a, -1, 99);
    set(a, -2147483648, 99);
    set(a, 2147483647, 99);
    assert.sameValue(calls, 0);
    assert.sameValue(a.hasOwnProperty("4"), false);
  } finally {
    delete Int32Array.prototype["4"];
    delete Int32Array.prototype["-1"];
    delete Int32Array.prototype["2147483647"];
  }
})();

// Integral float keys are smis, so `1.0` takes the same fast path as `1`. Negative zero
// is a double key and is only handled by the slow path, but must agree with the fast path.
(function () {
  function set(a, k, v) { a[k] = v; }
  var a = new Int32Array(2);
  // Smi key, fast path
  set(a, 1.0, 20);
  // Double key, slow path
  set(a, -0, 10);
  assert.sameValue(a[1], 20);
  assert.sameValue(a[0], 10);
})();

// Stores through views at byte offsets land in the right part of the buffer.
(function () {
  function set(a, i, v) { a[i] = v; }
  var buffer = new ArrayBuffer(16);
  var lo = new Int32Array(buffer, 0, 2);
  var hi = new Int32Array(buffer, 8, 2);
  set(lo, 0, 1);
  set(hi, 0, 3);
  assert.sameValue(lo[0], 1);
  assert.sameValue(lo[1], 0);
  assert.sameValue(hi[0], 3);
})();

// Shrinking a resizable buffer puts a fixed-length view out of bounds, stores are
// silently dropped. Regrowing exposes zeroed memory, not the dropped values.
(function () {
  function set(a, i, v) { a[i] = v; }
  var buffer = new ArrayBuffer(16, { maxByteLength: 16 });
  var a = new Int32Array(buffer, 0, 4);
  set(a, 0, 11);
  set(a, 3, 44);
  buffer.resize(8);
  for (var i = 0; i < 4; i++) {
    set(a, i, 99);
  }
  buffer.resize(16);
  assert.sameValue(a[0], 11);
  assert.sameValue(a[2], 0);
  assert.sameValue(a[3], 0);
})();

// Length-tracking views accept stores to indices exposed by a grow.
(function () {
  function set(a, i, v) { a[i] = v; }
  var buffer = new ArrayBuffer(8, { maxByteLength: 16 });
  var a = new Int32Array(buffer);
  set(a, 1, 5);
  set(a, 2, 99);
  assert.sameValue(a[1], 5);
  buffer.resize(16);
  set(a, 3, 7);
  assert.sameValue(a[3], 7);
  assert.sameValue(a[2], 0);
})();

// Detaching the buffer makes stores silent no-ops, even in strict mode.
(function () {
  "use strict";
  function set(a, i, v) { a[i] = v; }
  var buffer = new ArrayBuffer(8);
  var a = new Int32Array(buffer);
  set(a, 0, 5);
  $262.detachArrayBuffer(buffer);
  set(a, 0, 9);
  assert.sameValue(a[0], undefined);
})();

// BigInt typed arrays accept bigint stores and reject number stores.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = new BigInt64Array(2);
  set(a, 0, 5n);
  assert.sameValue(a[0], 5n);
  assert.throws(TypeError, function () { set(a, 1, 5); });

  var u = new BigUint64Array(2);
  set(u, 0, 18446744073709551615n);
  set(u, 1, -1n);
  assert.sameValue(u[0], 18446744073709551615n);
  assert.sameValue(u[1], 18446744073709551615n);
  assert.throws(TypeError, function () { set(u, 0, 5); });
})();

// Shrinking below a length-tracking view's byte offset makes stores silent no-ops.
(function () {
  function set(a, i, v) { a[i] = v; }
  var buffer = new ArrayBuffer(16, { maxByteLength: 24 });
  var a = new Float64Array(buffer, 8);
  set(a, 0, 1.5);
  assert.sameValue(a[0], 1.5);
  buffer.resize(4);
  set(a, 0, 99);
  assert.sameValue(a.length, 0);
  buffer.resize(24);
  assert.sameValue(a[0], 0);
  assert.sameValue(a[1], 0);
})();

// A ToNumber conversion that resizes the buffer runs before the bounds check and
// reallocates the backing data, so the store must be rechecked against the new bounds.
(function () {
  function set(a, i, v) { a[i] = v; }

  // Growing exposes the index, so the store lands in the reallocated data
  var grown = new ArrayBuffer(4, { maxByteLength: 64 });
  var growing = new Int32Array(grown);
  set(growing, 0, 111);
  set(growing, 3, { valueOf: function () { grown.resize(64); return 999; } });
  assert.sameValue(growing.length, 16);
  assert.sameValue(growing[0], 111);
  assert.sameValue(growing[3], 999);

  // The same conversion on a fixed length view, which stays in bounds after the grow
  var fixedBuffer = new ArrayBuffer(4, { maxByteLength: 64 });
  var fixed = new Int32Array(fixedBuffer, 0, 1);
  set(fixed, 0, { valueOf: function () { fixedBuffer.resize(64); return 42; } });
  assert.sameValue(fixed[0], 42);

  // Shrinking takes the index out of bounds, so the store is dropped
  var shrunk = new ArrayBuffer(64, { maxByteLength: 64 });
  var shrinking = new Int32Array(shrunk);
  set(shrinking, 15, { valueOf: function () { shrunk.resize(4); return 5; } });
  assert.sameValue(shrinking.length, 1);
  assert.sameValue(shrinking[15], undefined);

  // Detaching drops the store without throwing
  var detached = new ArrayBuffer(8);
  var detaching = new Int32Array(detached);
  var converted = 0;
  set(detaching, 0, {
    valueOf: function () {
      converted++;
      $262.detachArrayBuffer(detached);
      return 5;
    },
  });
  assert.sameValue(converted, 1);
  assert.sameValue(detaching[0], undefined);
})();

// A ToNumber conversion runs even when the index is already out of bounds, so its side
// effects are observable and it can still throw.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = new Int32Array(2);
  var converted = 0;
  var counter = { valueOf: function () { converted++; return 1; } };
  set(a, 99, counter);
  set(a, -1, counter);
  assert.sameValue(converted, 2);
  assert.throws(RangeError, function () {
    set(a, 99, { valueOf: function () { throw new RangeError("boom"); } });
  });
  // ToBigInt still rejects a number store on a BigInt array at an invalid index
  var big = new BigInt64Array(1);
  assert.throws(TypeError, function () { set(big, 99, 5); });
})();

// The fast path is guarded on any pointer value, not just objects, so smi keyed stores
// on other kinds of heap values fall back to the slow path unchanged.
(function () {
  function set(a, i, v) { a[i] = v; }
  var object = {};
  set(object, 0, "zero");
  assert.sameValue(object[0], "zero");

  var array = [1, 2];
  set(array, 1, 22);
  assert.sameValue(array[1], 22);

  // Indices past a string wrapper's length are ordinary extensible properties
  var boxedString = new String("xy");
  set(boxedString, 5, "five");
  assert.sameValue(boxedString[5], "five");
  assert.sameValue(boxedString[0], "x");

  var boxedNumber = new Number(1);
  set(boxedNumber, 0, "zero");
  assert.sameValue(boxedNumber[0], "zero");

  var map = new Map();
  set(map, 0, "zero");
  assert.sameValue(map[0], "zero");
  assert.sameValue(map.size, 0);
})();

// A proxy wrapping a typed array is a different kind of object, so its traps still run.
(function () {
  function set(a, i, v) { a[i] = v; }
  var target = new Int32Array(2);
  var keys = [];
  var proxy = new Proxy(target, {
    set: function (t, k, v, r) {
      keys.push(String(k));
      return Reflect.set(t, k, v, r);
    },
  });
  set(proxy, 0, 10);
  set(proxy, 5, 99);
  assert.sameValue(keys.join(","), "0,5");
  assert.sameValue(target[0], 10);
  assert.sameValue(target.hasOwnProperty("5"), false);
})();

// Subclass instances keep their typed array kind, so they use the same fast path and
// still ignore an indexed setter inherited from the subclass prototype.
(function () {
  function set(a, i, v) { a[i] = v; }
  class Subclass extends Int32Array {}
  var calls = 0;
  Object.defineProperty(Subclass.prototype, "0", {
    set: function () { calls++; },
    configurable: true,
  });
  var a = new Subclass(2);
  set(a, 0, 7);
  assert.sameValue(calls, 0);
  assert.sameValue(a[0], 7);
})();

// Named properties on a typed array change its shape but not its indexed elements.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = new Int32Array(2);
  for (var i = 0; i < 200; i++) a["p" + i] = i;
  set(a, 0, 5);
  set(a, 1, 6);
  assert.sameValue(a[0], 5);
  assert.sameValue(a[1], 6);
  assert.sameValue(a.p199, 199);
})();

// Stores stay correct across a garbage collection.
(function () {
  function set(a, i, v) { a[i] = v; }
  var a = new Int32Array(2);
  set(a, 0, 1);
  $262.gc();
  set(a, 1, 2);
  assert.sameValue(a[0], 1);
  assert.sameValue(a[1], 2);
})();
