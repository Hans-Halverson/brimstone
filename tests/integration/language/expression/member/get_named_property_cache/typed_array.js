/*---
description: ^
  GetNamedProperty cache on typed array receivers. Non-numeric keys are ordinary and may be cached,
  canonical numeric keys must keep their exotic behavior.
---*/

// Typed array length is a prototype getter, a warm callsite must see live updates.
(function () {
  function len(a) { return a.length; }
  var buffer = new ArrayBuffer(4, { maxByteLength: 16 });
  var ta = new Uint8Array(buffer);
  assert.sameValue(len(ta), 4);
  assert.sameValue(len(ta), 4);
  buffer.resize(8);
  assert.sameValue(len(ta), 8);
  buffer.resize(0);
  assert.sameValue(len(ta), 0);
  buffer.resize(2);
  assert.sameValue(len(ta), 2);
  buffer.transfer();
  assert.sameValue(len(ta), 0);
})();

// Typed array length at one callsite across different instances, kinds, and lengths.
(function () {
  function len(a) { return a.length; }
  assert.sameValue(len(new Uint8Array(4)), 4);
  assert.sameValue(len(new Uint8Array(7)), 7);
  assert.sameValue(len(new Float64Array(3)), 3);
  assert.sameValue(len(new Int16Array(0)), 0);
  assert.sameValue(len(new BigInt64Array(2)), 2);
  assert.sameValue(len(new Uint8Array(4)), 4);
})();

// Other prototype getters and methods on typed arrays read correctly at a warm callsite.
(function () {
  function byteLen(a) { return a.byteLength; }
  function byteOff(a) { return a.byteOffset; }
  function sub(a) { return a.subarray; }
  var buffer = new ArrayBuffer(8);
  var ta1 = new Uint16Array(buffer, 2, 2);
  var ta2 = new Uint16Array(buffer, 4, 1);
  assert.sameValue(byteLen(ta1), 4);
  assert.sameValue(byteLen(ta1), 4);
  assert.sameValue(byteLen(ta2), 2);
  assert.sameValue(byteOff(ta1), 2);
  assert.sameValue(byteOff(ta2), 4);
  assert.sameValue(sub(ta1), Uint16Array.prototype.subarray);
  assert.sameValue(sub(ta2), Uint16Array.prototype.subarray);
})();

// Named properties added directly to a typed array read correctly at a warm callsite.
(function () {
  function get(a) { return a.foo; }
  var ta1 = new Uint8Array(2);
  var ta2 = new Uint8Array(2);
  ta1.foo = 1;
  ta2.foo = 2;
  assert.sameValue(get(ta1), 1);
  assert.sameValue(get(ta1), 1);
  assert.sameValue(get(ta2), 2);
  ta1.foo = 3;
  assert.sameValue(get(ta1), 3);
})();

// An absent property on a typed array is undefined and stays correct once defined.
(function () {
  function get(a) { return a.missing; }
  var ta = new Uint8Array(2);
  assert.sameValue(get(ta), undefined);
  assert.sameValue(get(ta), undefined);
  Uint8Array.prototype.missing = 5;
  assert.sameValue(get(ta), 5);
  delete Uint8Array.prototype.missing;
  assert.sameValue(get(ta), undefined);
})();

// "NaN" and "Infinity" are canonical numeric index strings, so a typed array returns undefined
// for them without consulting the prototype chain, even at a warm callsite and even when the
// same callsite also sees ordinary objects with those properties.
//
// These are the only canonical numeric index strings that are valid identifiers. Every other one
// ("-0", "-1", "1.5", "1e+21", ...) can only be written as a computed key, which compiles to a
// keyed access and never reaches the named property cache.
(function () {
  function getNaN(o) { return o.NaN; }
  function getInfinity(o) { return o.Infinity; }
  Object.defineProperty(Uint8Array.prototype, "NaN", { get: function () { return 1; }, configurable: true });
  Object.defineProperty(Uint8Array.prototype, "Infinity", { get: function () { return 2; }, configurable: true });
  var ta = new Uint8Array(2);
  var ordinary = { NaN: 3, Infinity: 4 };
  for (var i = 0; i < 3; i++) {
    assert.sameValue(getNaN(ta), undefined);
    assert.sameValue(getInfinity(ta), undefined);
    assert.sameValue(getNaN(ordinary), 3);
    assert.sameValue(getInfinity(ordinary), 4);
  }
  delete Uint8Array.prototype.NaN;
  delete Uint8Array.prototype.Infinity;
})();

// Stores of "NaN" and "Infinity" on a typed array are silently ignored and do not create own
// properties.
(function () {
  function setNaN(o, v) { o.NaN = v; }
  function setInfinity(o, v) { o.Infinity = v; }
  var ta = new Uint8Array(2);
  var ordinary = {};
  for (var i = 0; i < 3; i++) {
    setNaN(ta, i);
    setInfinity(ta, i);
    setNaN(ordinary, i);
    setInfinity(ordinary, i);
  }
  assert.sameValue(ta.hasOwnProperty("NaN"), false);
  assert.sameValue(ta.hasOwnProperty("Infinity"), false);
  assert.sameValue(ta.NaN, undefined);
  assert.sameValue(ta.Infinity, undefined);
  assert.sameValue(ordinary.NaN, 2);
  assert.sameValue(ordinary.Infinity, 2);
})();

// Stores of ordinary named properties on a typed array are cached and stay correct.
(function () {
  function set(o, v) { o.foo = v; }
  var ta1 = new Int8Array(4);
  var ta2 = new Int8Array(4);
  set(ta1, 1);
  set(ta1, 2);
  set(ta2, 3);
  assert.sameValue(ta1.foo, 2);
  assert.sameValue(ta2.foo, 3);
  assert.sameValue(ta1[0], 0);
  assert.sameValue(ta1.length, 4);
})();

// A typed array in the prototype chain is ordinary for non-numeric keys.
(function () {
  function get(o) { return o.foo; }
  var proto = new Uint8Array(2);
  proto.foo = 7;
  var o = Object.create(proto);
  assert.sameValue(get(o), 7);
  assert.sameValue(get(o), 7);
  proto.foo = 8;
  assert.sameValue(get(o), 8);
})();
