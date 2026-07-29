/*---
description: GetProperty fast path for typed arrays.
---*/

// In-range loads for every typed array kind, covering each kind's value range.
(function () {
  function get(a, i) { return a[i]; }
  var i8 = new Int8Array([1, -128, 127]);
  assert.sameValue(get(i8, 0), 1);
  assert.sameValue(get(i8, 1), -128);
  assert.sameValue(get(i8, 2), 127);

  var u8 = new Uint8Array([0, 255]);
  assert.sameValue(get(u8, 1), 255);

  var u8c = new Uint8ClampedArray([0, 255]);
  assert.sameValue(get(u8c, 1), 255);

  var i16 = new Int16Array([-32768, 32767]);
  assert.sameValue(get(i16, 0), -32768);
  assert.sameValue(get(i16, 1), 32767);

  var u16 = new Uint16Array([65535]);
  assert.sameValue(get(u16, 0), 65535);

  var i32 = new Int32Array([-2147483648, 2147483647]);
  assert.sameValue(get(i32, 0), -2147483648);
  assert.sameValue(get(i32, 1), 2147483647);

  // Uint32 values above the smi range read back as doubles.
  var u32 = new Uint32Array([2147483648, 4294967295]);
  assert.sameValue(get(u32, 0), 2147483648);
  assert.sameValue(get(u32, 1), 4294967295);

  var f32 = new Float32Array([1.5]);
  assert.sameValue(get(f32, 0), 1.5);

  var f64 = new Float64Array([0.1, NaN]);
  assert.sameValue(get(f64, 0), 0.1);
  assert.sameValue(Number.isNaN(get(f64, 1)), true);

  var f16 = new Float16Array([1.5, 0.1, 65504, Infinity, NaN]);
  assert.sameValue(get(f16, 0), 1.5);
  assert.sameValue(get(f16, 1), 0.0999755859375);
  assert.sameValue(get(f16, 2), 65504);
  assert.sameValue(get(f16, 3), Infinity);
  assert.sameValue(Number.isNaN(get(f16, 4)), true);
})();

// Negative zero loads as negative zero, not positive zero.
(function () {
  function get(a, i) { return a[i]; }
  var f64 = new Float64Array([-0]);
  assert.sameValue(get(f64, 0), -0);
  var f32 = new Float32Array([-0]);
  assert.sameValue(get(f32, 0), -0);
  var f16 = new Float16Array([-0]);
  assert.sameValue(get(f16, 0), -0);
})();

// Arbitrary NaN bit patterns written through an aliasing view load as NaN numbers. The
// payload must be canonicalized and can never be misread as another value type.
(function () {
  function get(a, i) { return a[i]; }
  var buffer = new ArrayBuffer(16);
  var f64 = new Float64Array(buffer);
  var u8 = new Uint8Array(buffer);
  // All-ones bit pattern: a quiet NaN with every payload bit set
  for (var i = 0; i < 8; i++) u8[i] = 0xff;
  // Signaling NaN bit pattern: exponent all ones, high mantissa bit clear (little-endian)
  u8[8] = 0x01; u8[14] = 0xf4; u8[15] = 0x7f;
  for (var i = 0; i < 2; i++) {
    var value = get(f64, i);
    assert.sameValue(typeof value, "number", "index " + i);
    assert.sameValue(Number.isNaN(value), true, "index " + i);
  }

  var f32buffer = new ArrayBuffer(4);
  var f32 = new Float32Array(f32buffer);
  var f32u8 = new Uint8Array(f32buffer);
  for (var i = 0; i < 4; i++) f32u8[i] = 0xff;
  assert.sameValue(Number.isNaN(get(f32, 0)), true);

  // Half precision widens to a double before boxing, so it must canonicalize too
  var f16buffer = new ArrayBuffer(4);
  var f16 = new Float16Array(f16buffer);
  var f16u8 = new Uint8Array(f16buffer);
  // All-ones bit pattern: a quiet NaN with every payload bit set
  f16u8[0] = 0xff; f16u8[1] = 0xff;
  // Signaling NaN bit pattern: exponent all ones, high mantissa bit clear (little-endian)
  f16u8[2] = 0x01; f16u8[3] = 0x7c;
  for (var i = 0; i < 2; i++) {
    var f16Value = get(f16, i);
    assert.sameValue(typeof f16Value, "number", "f16 index " + i);
    assert.sameValue(Number.isNaN(f16Value), true, "f16 index " + i);
  }
})();

// Invalid indices read as undefined and never consult the prototype chain.
(function () {
  function get(a, i) { return a[i]; }
  Int32Array.prototype[4] = "proto";
  try {
    var a = new Int32Array([1, 2]);
    get(a, 0); get(a, 0);
    assert.sameValue(get(a, 2), undefined);
    assert.sameValue(get(a, 4), undefined);
    assert.sameValue(get(a, -1), undefined);
    // Not a smi key, so this one is checked by the slow path
    assert.sameValue(get(a, 0.5), undefined);
  } finally {
    delete Int32Array.prototype[4];
  }
})();

// Invalid indices still read as undefined when the prototype has an accessor at a
// negative or huge index, which are the indices the fast path folds into a huge usize.
(function () {
  function get(a, i) { return a[i]; }
  var descriptor = { get: function () { return "proto"; }, configurable: true };
  Object.defineProperty(Int32Array.prototype, "-1", descriptor);
  Object.defineProperty(Int32Array.prototype, "2147483647", descriptor);
  try {
    var a = new Int32Array([1, 2]);
    assert.sameValue(get(a, -1), undefined);
    assert.sameValue(get(a, -2147483648), undefined);
    assert.sameValue(get(a, 2147483647), undefined);
  } finally {
    delete Int32Array.prototype["-1"];
    delete Int32Array.prototype["2147483647"];
  }
})();

// Integral float keys are smis, so `1.0` takes the same fast path as `1`. Negative zero
// is a double key and is only handled by the slow path, but must agree with the fast path.
(function () {
  function get(a, k) { return a[k]; }
  var a = new Int32Array([10, 20]);
  // Smi key, fast path
  assert.sameValue(get(a, 1.0), 20);
  assert.sameValue(get(a, 1), 20);
  // Double keys, slow path
  assert.sameValue(get(a, -0), 10);
  assert.sameValue(get(a, 1.5), undefined);
})();

// A large in-bounds smi index is handled by the fast path.
(function () {
  function get(a, i) { return a[i]; }
  var a = new Int8Array(3000);
  a[2999] = 7;
  assert.sameValue(get(a, 2999), 7);
  assert.sameValue(get(a, 3000), undefined);
})();

// Views at byte offsets into the same buffer are disjoint.
(function () {
  function get(a, i) { return a[i]; }
  var buffer = new ArrayBuffer(16);
  var lo = new Int32Array(buffer, 0, 2);
  var hi = new Int32Array(buffer, 8, 2);
  lo[0] = 1; lo[1] = 2; hi[0] = 3; hi[1] = 4;
  assert.sameValue(get(lo, 0), 1);
  assert.sameValue(get(lo, 1), 2);
  assert.sameValue(get(hi, 0), 3);
  assert.sameValue(get(hi, 1), 4);
})();

// Shrinking a resizable buffer puts a fixed-length view out of bounds, every index
// reads as undefined. Regrowing exposes zeroed memory, not stale values.
(function () {
  function get(a, i) { return a[i]; }
  var buffer = new ArrayBuffer(16, { maxByteLength: 16 });
  var a = new Int32Array(buffer, 0, 4);
  a[0] = 11; a[1] = 22; a[2] = 33; a[3] = 44;
  assert.sameValue(get(a, 3), 44);
  buffer.resize(8);
  for (var i = 0; i < 4; i++) {
    assert.sameValue(get(a, i), undefined);
  }
  buffer.resize(16);
  assert.sameValue(get(a, 0), 11);
  assert.sameValue(get(a, 1), 22);
  assert.sameValue(get(a, 2), 0);
  assert.sameValue(get(a, 3), 0);
})();

// Length-tracking views follow buffer resizes.
(function () {
  function get(a, i) { return a[i]; }
  var buffer = new ArrayBuffer(8, { maxByteLength: 16 });
  var a = new Int32Array(buffer);
  a[1] = 5;
  assert.sameValue(get(a, 1), 5);
  assert.sameValue(get(a, 2), undefined);
  buffer.resize(16);
  a[3] = 7;
  assert.sameValue(get(a, 3), 7);
  buffer.resize(4);
  assert.sameValue(get(a, 1), undefined);
})();

// A partial trailing property exposed by a resize is excluded from a length-tracking view.
(function () {
  function get(a, i) { return a[i]; }
  var buffer = new ArrayBuffer(8, { maxByteLength: 16 });
  var a = new Int32Array(buffer);
  a[0] = 1; a[1] = 2;
  buffer.resize(10);
  assert.sameValue(a.length, 2);
  assert.sameValue(get(a, 1), 2);
  assert.sameValue(get(a, 2), undefined);
})();

// Shrinking below a length-tracking view's byte offset makes every index read as undefined.
(function () {
  function get(a, i) { return a[i]; }
  var buffer = new ArrayBuffer(16, { maxByteLength: 24 });
  var a = new Float64Array(buffer, 8);
  a[0] = 1.5;
  assert.sameValue(get(a, 0), 1.5);
  buffer.resize(4);
  assert.sameValue(a.length, 0);
  assert.sameValue(get(a, 0), undefined);
  buffer.resize(24);
  assert.sameValue(a.length, 2);
  assert.sameValue(get(a, 0), 0);
  assert.sameValue(get(a, 1), 0);
})();

// Detaching the buffer makes every index read as undefined.
(function () {
  function get(a, i) { return a[i]; }
  var buffer = new ArrayBuffer(8);
  var a = new Int32Array(buffer);
  a[0] = 5;
  assert.sameValue(get(a, 0), 5);
  $262.detachArrayBuffer(buffer);
  assert.sameValue(get(a, 0), undefined);
})();

// Indexed property loads stay correct across a garbage collection.
(function () {
  function get(a, i) { return a[i]; }
  var a = new Int32Array([1, 2]);
  assert.sameValue(get(a, 0), 1);
  $262.gc();
  assert.sameValue(get(a, 0), 1);
  assert.sameValue(get(a, 1), 2);
})();

// BigInt typed array loads return bigints.
(function () {
  function get(a, i) { return a[i]; }
  var a = new BigInt64Array([1n, -2n]);
  assert.sameValue(get(a, 0), 1n);
  assert.sameValue(get(a, 1), -2n);
  assert.sameValue(get(a, 2), undefined);

  var u = new BigUint64Array([0n, 18446744073709551615n]);
  assert.sameValue(get(u, 0), 0n);
  assert.sameValue(get(u, 1), 18446744073709551615n);
  assert.sameValue(get(u, 2), undefined);
})();

// The fast path is guarded on any pointer value, not just objects, so smi keyed loads on
// strings, symbols and bigints reach the kind check before falling back to the slow path.
(function () {
  function get(a, i) { return a[i]; }
  var string = "abc";
  assert.sameValue(get(string, 0), "a");
  assert.sameValue(get(string, 2), "c");
  assert.sameValue(get(string, 3), undefined);
  assert.sameValue(get(string, -1), undefined);

  assert.sameValue(get(new String("xy"), 1), "y");
  assert.sameValue(get(Symbol("desc"), 0), undefined);
  assert.sameValue(get(1234n, 0), undefined);
  assert.sameValue(get(new Map([[0, "zero"]]), 0), undefined);
  assert.sameValue(get({ 0: "own" }, 0), "own");
})();

// A proxy wrapping a typed array is a different kind of object, so its traps still run.
(function () {
  function get(a, i) { return a[i]; }
  var target = new Int32Array([10, 20]);
  var keys = [];
  var proxy = new Proxy(target, {
    get: function (t, k, r) {
      keys.push(String(k));
      return Reflect.get(t, k, r);
    },
  });
  assert.sameValue(get(proxy, 0), 10);
  assert.sameValue(get(proxy, 1), 20);
  assert.sameValue(get(proxy, 5), undefined);
  assert.sameValue(keys.join(","), "0,1,5");
})();

// Named properties on a typed array change its shape but not its indexed elements.
(function () {
  function get(a, i) { return a[i]; }
  var a = new Int32Array([1, 2]);
  for (var i = 0; i < 200; i++) a["p" + i] = i;
  assert.sameValue(get(a, 0), 1);
  assert.sameValue(get(a, 1), 2);
  assert.sameValue(get(a, 2), undefined);
  assert.sameValue(a.p199, 199);
})();

// Subclass instances keep their typed array kind, so they use the same fast path and
// still ignore an indexed accessor inherited from the subclass prototype.
(function () {
  function get(a, i) { return a[i]; }
  class Subclass extends Int32Array {}
  Object.defineProperty(Subclass.prototype, "0", {
    get: function () { return "proto"; },
    configurable: true,
  });
  var a = new Subclass([1, 2]);
  assert.sameValue(get(a, 0), 1);
  assert.sameValue(get(a, 1), 2);
  assert.sameValue(get(a, 2), undefined);
})();

// A single callsite handles different typed array kinds and regular arrays interchangeably.
(function () {
  function get(a, i) { return a[i]; }
  var i8 = new Int8Array([1]);
  var f64 = new Float64Array([2.5]);
  var arr = [3];
  assert.sameValue(get(i8, 0), 1);
  assert.sameValue(get(f64, 0), 2.5);
  assert.sameValue(get(arr, 0), 3);
  assert.sameValue(get(i8, 0), 1);
})();
