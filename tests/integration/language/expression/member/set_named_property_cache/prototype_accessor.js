/*---
description: SetNamedProperty cache handles accessor properties from the prototype chain.
flags: [noStrict]
---*/

// Prototype setter is called with the actual receiver as `this`, no own property is created.
(function () {
  var seenThis, calls = 0;
  function set(o, v) { o.x = v; }
  var proto = { set x(v) { calls++; seenThis = this; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  set(o, 3);
  assert.sameValue(calls, 3);
  assert.sameValue(seenThis, o);
  assert.sameValue(o.hasOwnProperty("x"), false);
})();

// A cached prototype setter binds `this` to the actual receiver, not the one that filled the cache.
(function () {
  function set(o, v) { o.x = v; }
  var proto = { set x(v) { this.tag = v; } };
  var a = Object.create(proto); a.pad = 1;
  var b = Object.create(proto); b.pad = 1;
  set(a, "a");
  set(a, "a");
  set(b, "b");
  assert.sameValue(a.tag, "a");
  assert.sameValue(b.tag, "b");
})();

// Deleting the prototype accessor invalidates the guard, later stores create own properties.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var proto = { set x(v) { calls++; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  delete proto.x;
  set(o, 3);
  assert.sameValue(calls, 2);
  assert.sameValue(o.x, 3);
  assert.sameValue(o.hasOwnProperty("x"), true);
})();

// Setter-only accessor on the prototype: sloppy stores call it, reads are undefined.
(function () {
  var last;
  function set(o, v) { o.x = v; }
  var proto = { set x(v) { last = v; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  assert.sameValue(last, 2);
  assert.sameValue(o.x, undefined);
})();

// Getter-only accessor on the prototype: sloppy silently ignores, no own property is created.
(function () {
  function set(o, v) { o.x = v; }
  var proto = { get x() { return 3; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  assert.sameValue(o.x, 3);
  assert.sameValue(o.hasOwnProperty("x"), false);
})();

// Getter-only accessor on the prototype throws in strict mode on a warm cache hit.
(function () {
  "use strict";
  function set(o, v) { o.x = v; }
  var proto = { get x() { return 3; } };
  var o = Object.create(proto);
  o.pad = 1;
  assert.throws(TypeError, function () { set(o, 1); });
  assert.throws(TypeError, function () { set(o, 2); });
})();

// The setter may mutate the receiver, later stores must still reach the setter.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var proto = { set x(v) { calls++; this["p" + v] = v; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  set(o, 3);
  assert.sameValue(calls, 3);
  assert.sameValue(o.p1, 1);
  assert.sameValue(o.p3, 3);
  assert.sameValue(o.hasOwnProperty("x"), false);
})();

// A setter that deletes itself: only the first store reaches it, the rest create own properties.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var proto = {};
  Object.defineProperty(proto, "x", {
    set: function (v) { calls++; delete proto.x; },
    configurable: true,
  });
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  set(o, 3);
  assert.sameValue(calls, 1);
  assert.sameValue(o.x, 3);
})();

// Accessor deep in the chain, then a middle link gains a shadowing writable data property.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var top = { set x(v) { calls++; } };
  var mid = Object.create(top);
  var proto = Object.create(mid);
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  Object.defineProperty(mid, "x", { value: 0, writable: true, enumerable: true, configurable: true });
  set(o, 3);
  assert.sameValue(calls, 2);
  assert.sameValue(o.x, 3);
  assert.sameValue(o.hasOwnProperty("x"), true);
})();

// Changing the receiver's prototype transitions its shape, the old setter stops being called.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var protoA = { set x(v) { calls++; } };
  var protoB = {};
  var o = Object.create(protoA);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  Object.setPrototypeOf(o, protoB);
  set(o, 3);
  assert.sameValue(calls, 2);
  assert.sameValue(o.x, 3);
})();

// Cutting a middle link's prototype removes the accessor from the chain, guard must invalidate.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var top = { set x(v) { calls++; } };
  var mid = Object.create(top);
  var o = Object.create(mid);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  Object.setPrototypeOf(mid, null);
  set(o, 3);
  assert.sameValue(calls, 2);
  assert.sameValue(o.x, 3);
})();

// An unrelated property added to the prototype invalidates the guard, but the setter is still found.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var proto = { set x(v) { calls++; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  proto.unrelated = 1;
  set(o, 3);
  assert.sameValue(calls, 3);
})();

// Class setters, including one inherited through a superclass prototype.
(function () {
  var baseCalls = 0, derivedCalls = 0;
  class Base { set p(v) { baseCalls++; this._p = v; } }
  class Derived extends Base { set q(v) { derivedCalls++; this._q = v; } }
  function setP(o, v) { o.p = v; }
  function setQ(o, v) { o.q = v; }
  var d = new Derived();
  setP(d, 1);
  setP(d, 2);
  setQ(d, 3);
  setQ(d, 4);
  assert.sameValue(baseCalls, 2);
  assert.sameValue(derivedCalls, 2);
  assert.sameValue(d._p, 2);
  assert.sameValue(d._q, 4);
})();

// `__proto__` stores go through the Object.prototype accessor at a warm callsite.
(function () {
  function set(o, v) { o.__proto__ = v; }
  var protoA = { tag: "a" };
  var protoB = { tag: "b" };
  var o = {};
  set(o, protoA);
  assert.sameValue(Object.getPrototypeOf(o), protoA);
  set(o, protoB);
  assert.sameValue(Object.getPrototypeOf(o), protoB);
})();

// A prototype setter store stays correct across a garbage collection.
(function () {
  var last;
  function set(o, v) { o.x = v; }
  var proto = { set x(v) { last = v; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  $262.gc();
  set(o, 2);
  assert.sameValue(last, 2);
})();

// Prototype setter removed in place leaves a getter-only accessor. The in-place
// define invalidates the chain guard, so warm strict stores must miss and throw.
(function () {
  "use strict";
  var calls = 0;
  function set(o, v) { o.x = v; }
  var proto = { get x() { return 7; }, set x(v) { calls++; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  Object.defineProperty(proto, "x", { set: undefined });
  assert.throws(TypeError, function () { set(o, 3); });
  assert.throws(TypeError, function () { set(o, 4); });
  assert.sameValue(calls, 2);
  assert.sameValue(o.hasOwnProperty("x"), false);
})();

// Prototype setter removed in place, sloppy mode: the invalidated guard forces a refill
// which caches the now setter-less accessor, and further stores silently no-op through it.
(function () {
  var calls = 0;
  function set(o, v) { o.x = v; }
  var proto = { get x() { return 7; }, set x(v) { calls++; } };
  var o = Object.create(proto);
  o.pad = 1;
  set(o, 1);
  set(o, 2);
  Object.defineProperty(proto, "x", { set: undefined });
  set(o, 3);
  set(o, 4);
  set(o, 5);
  assert.sameValue(calls, 2);
  assert.sameValue(o.x, 7);
  assert.sameValue(o.hasOwnProperty("x"), false);
})();
