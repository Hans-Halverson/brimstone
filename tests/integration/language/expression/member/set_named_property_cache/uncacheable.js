/*---
description: SetNamedProperty stores stay correct for receivers and callsites that cannot be cached.
flags: [noStrict]
---*/

// A polymorphic callsite (two different shapes) stays correct after caching stops.
(function () {
  function set(o, v) { o.a = v; }
  var x = { a: 0 };
  var y = { b: 0, a: 0 };
  for (var i = 0; i < 6; i++) {
    var o = i % 2 === 0 ? x : y;
    set(o, i);
    assert.sameValue(o.a, i);
  }
  assert.sameValue(x.a, 4);
  assert.sameValue(y.a, 5);
})();

// A megamorphic transition callsite (fresh shape every call) stays correct.
(function () {
  function set(o, v) { o.y = v; }
  for (var i = 0; i < 6; i++) {
    var o = {};
    o["u" + i] = 1;
    set(o, i);
    assert.sameValue(o.y, i);
  }
})();

// Named properties on arrays are stored correctly.
(function () {
  function set(o, v) { o.foo = v; }
  var arr = [1, 2, 3];
  set(arr, 1);
  set(arr, 2);
  assert.sameValue(arr.foo, 2);
  assert.sameValue(arr.length, 3);
})();

// Array length stores are never cached and keep their exotic behavior.
(function () {
  function setLength(o, v) { o.length = v; }
  var arr = [1, 2, 3, 4, 5];
  setLength(arr, 4);
  assert.sameValue(arr.length, 4);
  setLength(arr, 2);
  assert.sameValue(arr.length, 2);
  assert.sameValue(arr[2], undefined);
})();

// A map mode receiver (many properties) stores correctly at a warm callsite.
(function () {
  function setExisting(o, v) { o.k3 = v; }
  function setFresh(o, v) { o.fresh = v; }
  var o = {};
  for (var i = 0; i < 70; i++) { o["k" + i] = i; }
  setExisting(o, 1);
  setExisting(o, 2);
  assert.sameValue(o.k3, 2);
  setFresh(o, 1);
  setFresh(o, 2);
  assert.sameValue(o.fresh, 2);
})();

// Primitive receivers in sloppy mode are silent no-ops at a warm callsite.
(function () {
  function set(o, v) { o.foo = v; }
  set({ x: 1 }, 1);
  set({ x: 1 }, 2);
  set(5, 3);
  set("s", 4);
  set(true, 5);
  assert.sameValue((5).foo, undefined);
  assert.sameValue("s".foo, undefined);
})();

// Primitive receivers throw in strict mode.
(function () {
  "use strict";
  function set(o, v) { o.foo = v; }
  set({ x: 1 }, 1);
  set({ x: 1 }, 2);
  assert.throws(TypeError, function () { set(5, 3); });
  assert.throws(TypeError, function () { set("s", 4); });
})();

// A receiver that becomes a prototype changes shape when guarded, stores stay correct.
(function () {
  function set(o, v) { o.a = v; }
  function get(o) { return o.a; }
  var o = { a: 0 };
  set(o, 1);
  set(o, 2);
  var child = Object.create(o);
  // Warm a get callsite through the child to request a guard on o's chain.
  get(child);
  get(child);
  set(o, 3);
  assert.sameValue(o.a, 3);
  assert.sameValue(child.a, 3);
})();

// Stores of every value kind round-trip through a warm callsite.
(function () {
  function set(o, v) { o.v = v; }
  var o = { v: 0 };
  var sym = Symbol("s");
  var obj = {};
  set(o, null);
  assert.sameValue(o.v, null);
  set(o, undefined);
  assert.sameValue(o.v, undefined);
  set(o, NaN);
  assert.sameValue(o.v !== o.v, true);
  set(o, "str");
  assert.sameValue(o.v, "str");
  set(o, sym);
  assert.sameValue(o.v, sym);
  set(o, obj);
  assert.sameValue(o.v, obj);
})();

// A Proxy in the prototype chain intercepts stores with its set trap on every call.
(function () {
  var traps = 0;
  function set(o, v) { o.x = v; }
  var proxyProto = new Proxy({}, {
    set: function (target, key, value, receiver) { traps++; return true; },
  });
  var o = Object.create(proxyProto);
  Object.defineProperty(o, "pad", { value: 1, writable: true, enumerable: true, configurable: true });
  set(o, 1);
  set(o, 2);
  set(o, 3);
  assert.sameValue(traps, 3);
  assert.sameValue(o.hasOwnProperty("x"), false);
})();

// A Proxy deeper in the chain, beyond a shadowing writable data property, is never
// consulted, and the shadowing store stays correct at a warm callsite.
(function () {
  var traps = 0;
  function set(o, v) { o.y = v; }
  var proxyTop = new Proxy({}, {
    set: function () { traps++; return true; },
  });
  var mid = Object.create(proxyTop);
  Object.defineProperty(mid, "y", { value: 100, writable: true, enumerable: true, configurable: true });
  for (var i = 0; i < 3; i++) {
    var o = Object.create(mid);
    Object.defineProperty(o, "pad", { value: 1, writable: true, enumerable: true, configurable: true });
    set(o, i);
    assert.sameValue(o.y, i);
    assert.sameValue(o.hasOwnProperty("y"), true);
  }
  assert.sameValue(traps, 0);
  assert.sameValue(mid.y, 100);
})();

// A String object in the prototype chain is exotic, stores through it stay correct.
(function () {
  function set(o, v) { o.x = v; }
  var o = Object.create(new String("ab"));
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  assert.sameValue(o.x, 2);
  assert.sameValue(o.hasOwnProperty("x"), true);
})();

// A Proxy receiver calls its set trap on every store at a warm callsite.
(function () {
  var traps = 0;
  function set(o, v) { o.x = v; }
  var target = {};
  var p = new Proxy(target, {
    set: function (t, key, value) { traps++; t[key] = value; return true; },
  });
  set(p, 1);
  set(p, 2);
  set(p, 3);
  assert.sameValue(traps, 3);
  assert.sameValue(target.x, 3);
})();

// A String object receiver stores expando named properties correctly.
(function () {
  function set(o, v) { o.foo = v; }
  var s = new String("ab");
  set(s, 1);
  set(s, 2);
  assert.sameValue(s.foo, 2);
  assert.sameValue(s.length, 2);
})();

// A typed array receiver stores non-canonical named properties correctly.
(function () {
  function set(o, v) { o.foo = v; }
  var ta = new Int8Array(4);
  set(ta, 1);
  set(ta, 2);
  assert.sameValue(ta.foo, 2);
  assert.sameValue(ta[0], 0);
})();

// A mapped arguments object receiver stores named properties correctly.
(function () {
  function set(o, v) { o.foo = v; }
  function f(a) {
    set(arguments, 1);
    set(arguments, 2);
    assert.sameValue(arguments.foo, 2);
    assert.sameValue(arguments[0], a);
  }
  f(10);
  f(20);
})();
