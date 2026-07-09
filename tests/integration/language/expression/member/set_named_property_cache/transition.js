/*---
description: SetNamedProperty cache handles stores which add a new property via a shape transition.
flags: [noStrict]
---*/

// Adding a new property to fresh objects of the same shape at a warm callsite.
(function () {
  function set(o, v) { o.y = v; }
  for (var i = 0; i < 5; i++) {
    var o = { x: i };
    set(o, i * 2);
    assert.sameValue(o.y, i * 2);
    assert.sameValue(Object.keys(o).join(), "x,y");
  }
})();

// The transitioned property has the attributes of a plain store.
(function () {
  function set(o, v) { o.y = v; }
  var o = { x: 1 };
  set(o, 1);
  var o2 = { x: 2 };
  set(o2, 2);
  var desc = Object.getOwnPropertyDescriptor(o2, "y");
  assert.sameValue(desc.writable, true);
  assert.sameValue(desc.enumerable, true);
  assert.sameValue(desc.configurable, true);
})();

// A setter added to the prototype after warming must intercept later transition stores.
(function () {
  var calls = 0;
  function make(proto) { var o = Object.create(proto); o.x = 1; return o; }
  function set(o, v) { o.y = v; }
  var proto = {};
  var a = make(proto);
  set(a, 1);
  var b = make(proto);
  set(b, 2);
  Object.defineProperty(proto, "y", { set: function (v) { calls++; }, configurable: true });
  var c = make(proto);
  set(c, 3);
  assert.sameValue(calls, 1);
  assert.sameValue(a.hasOwnProperty("y"), true);
  assert.sameValue(c.hasOwnProperty("y"), false);
})();

// A non-writable data property added to the prototype after warming blocks later stores.
(function () {
  function make(proto) { var o = Object.create(proto); o.x = 1; return o; }
  function set(o, v) { o.y = v; }
  var proto = {};
  var a = make(proto);
  set(a, 1);
  set(make(proto), 2);
  Object.defineProperty(proto, "y", { value: 99, writable: false, configurable: true });
  var c = make(proto);
  set(c, 3);
  assert.sameValue(a.y, 1);
  assert.sameValue(c.hasOwnProperty("y"), false);
  assert.sameValue(c.y, 99);
})();

// The blocked store above throws in strict mode.
(function () {
  "use strict";
  function make(proto) { var o = Object.create(proto); o.x = 1; return o; }
  function set(o, v) { o.y = v; }
  var proto = {};
  set(make(proto), 1);
  set(make(proto), 2);
  Object.defineProperty(proto, "y", { value: 99, writable: false, configurable: true });
  var c = make(proto);
  assert.throws(TypeError, function () { set(c, 3); });
})();

// A non-extensible receiver has a different shape, warm transition stores must miss.
(function () {
  function make() { return { x: 1 }; }
  function set(o, v) { o.y = v; }
  set(make(), 1);
  set(make(), 2);
  var sealed = make();
  Object.preventExtensions(sealed);
  set(sealed, 3);
  assert.sameValue(sealed.hasOwnProperty("y"), false);
})();

// The non-extensible receiver throws in strict mode.
(function () {
  "use strict";
  function make() { return { x: 1 }; }
  function set(o, v) { o.y = v; }
  set(make(), 1);
  set(make(), 2);
  var sealed = make();
  Object.preventExtensions(sealed);
  assert.throws(TypeError, function () { set(sealed, 3); });
})();

// A writable data property on the prototype is shadowed by an own property, and stays untouched.
(function () {
  function set(o, v) { o.y = v; }
  var proto = { y: 100 };
  for (var i = 0; i < 3; i++) {
    var o = Object.create(proto);
    o.x = 1;
    set(o, i);
    assert.sameValue(o.y, i);
    assert.sameValue(o.hasOwnProperty("y"), true);
  }
  assert.sameValue(proto.y, 100);
})();

// A null-prototype receiver transitions with no guard needed.
(function () {
  function set(o, v) { o.y = v; }
  for (var i = 0; i < 3; i++) {
    var o = Object.create(null);
    o.x = 1;
    set(o, i);
    assert.sameValue(o.y, i);
  }
})();

// A map mode prototype without the key allows caching; adding the key there must be seen.
(function () {
  var calls = 0;
  var proto = {};
  for (var i = 0; i < 70; i++) { proto["k" + i] = i; }
  function make() { var o = Object.create(proto); o.x = 1; return o; }
  function set(o, v) { o.y = v; }
  var a = make();
  set(a, 1);
  set(make(), 2);
  Object.defineProperty(proto, "y", { set: function (v) { calls++; }, configurable: true });
  var c = make();
  set(c, 3);
  assert.sameValue(calls, 1);
  assert.sameValue(a.hasOwnProperty("y"), true);
  assert.sameValue(c.hasOwnProperty("y"), false);
})();

// A writable data property on a map mode prototype is shadowed correctly.
(function () {
  var proto = {};
  for (var i = 0; i < 70; i++) { proto["k" + i] = i; }
  proto.y = 100;
  function set(o, v) { o.y = v; }
  for (var j = 0; j < 3; j++) {
    var o = Object.create(proto);
    o.x = 1;
    set(o, j);
    assert.sameValue(o.y, j);
    assert.sameValue(o.hasOwnProperty("y"), true);
  }
  assert.sameValue(proto.y, 100);
})();

// Transition stores around the array mode property limit stay correct.
(function () {
  function makeWide() {
    var o = {};
    for (var i = 0; i < 63; i++) { o["w" + i] = i; }
    return o;
  }
  function setLast(o, v) { o.last = v; }
  function setOverflow(o, v) { o.overflow = v; }
  for (var j = 0; j < 3; j++) {
    var o = makeWide();
    setLast(o, j);
    assert.sameValue(o.last, j);
    setOverflow(o, j);
    assert.sameValue(o.overflow, j);
  }
})();

// Strict mode transition stores succeed without a bogus throw.
(function () {
  "use strict";
  function set(o, v) { o.y = v; }
  for (var i = 0; i < 3; i++) {
    var o = { x: i };
    set(o, i);
    assert.sameValue(o.y, i);
  }
})();

// A transition store stays correct across a garbage collection.
(function () {
  function set(o, v) { o.y = v; }
  set({ x: 1 }, 1);
  $262.gc();
  var o = { x: 2 };
  set(o, 2);
  assert.sameValue(o.y, 2);
})();

// A self-deleting prototype setter that appends TWO own properties must not be cached as a
// single-property transition. The stored key lands at the append frontier, but the shape gains
// more than one property, so replaying the transition with a single append would leave the value
// storage shorter than the new shape claims (a heap out-of-bounds without the parent-edge check).
(function () {
  var proto = {};
  Object.defineProperty(proto, "c", {
    configurable: true,
    set: function (v) {
      delete proto.c;
      Object.defineProperty(this, "c", { value: v, writable: true, enumerable: true, configurable: true });
      this.d = 999;
    },
  });
  function set(o, v) { o.c = v; }
  function make() { var o = Object.create(proto); o.a = 1; o.b = 2; return o; }

  var o1 = make(); set(o1, 5);
  assert.sameValue(o1.c, 5);
  assert.sameValue(o1.d, 999);

  // Second object of the same [a,b] shape must not inherit a bogus multi-property transition.
  var o2 = make(); set(o2, 7);
  assert.sameValue(o2.c, 7);
  assert.sameValue(o2.d, undefined);
  assert.sameValue(Object.keys(o2).join(), "a,b,c");
})();

// A self-deleting prototype setter that reconfigures an EARLIER own property before appending must
// not be cached. The new shape has the same property count as a clean append but a divergent
// prefix, so replaying the transition would silently rewrite the earlier property's attributes.
(function () {
  var proto = {};
  Object.defineProperty(proto, "c", {
    configurable: true,
    set: function (v) {
      delete proto.c;
      Object.defineProperty(this, "a", { enumerable: false });
      Object.defineProperty(this, "c", { value: v, writable: true, enumerable: true, configurable: true });
    },
  });
  function set(o, v) { o.c = v; }
  function make() { var o = Object.create(proto); o.a = 1; o.b = 2; return o; }

  var o1 = make(); set(o1, 5);
  assert.sameValue(Object.getOwnPropertyDescriptor(o1, "a").enumerable, false);

  // A fresh receiver's own `a` must stay enumerable - the divergent-prefix transition must not
  // have been cached and replayed onto it.
  var o2 = make(); set(o2, 7);
  assert.sameValue(Object.getOwnPropertyDescriptor(o2, "a").enumerable, true);
  assert.sameValue(Object.keys(o2).join(), "a,b,c");
  assert.sameValue(o2.c, 7);
})();

// A self-deleting prototype setter that appends a DIFFERENT property than the stored key must not
// be cached: the transition adds `other`, not `c`, so a later store of `c` must still create its
// own writable `c`.
(function () {
  var proto = {};
  Object.defineProperty(proto, "c", {
    configurable: true,
    set: function (v) { delete proto.c; this.other = v; },
  });
  function set(o, v) { o.c = v; }
  function make() { var o = Object.create(proto); o.a = 1; o.b = 2; return o; }

  var o1 = make(); set(o1, 5);
  assert.sameValue(o1.other, 5);
  assert.sameValue(o1.hasOwnProperty("c"), false);

  var o2 = make(); set(o2, 7);
  assert.sameValue(o2.c, 7);
  assert.sameValue(Object.getOwnPropertyDescriptor(o2, "c").writable, true);
})();

// A self-deleting prototype setter that appends the key as a NON-writable property must not be
// cached: a later plain store creates a writable `c`, so caching the non-writable shape would
// corrupt the property's attributes.
(function () {
  var proto = {};
  Object.defineProperty(proto, "c", {
    configurable: true,
    set: function (v) {
      delete proto.c;
      Object.defineProperty(this, "c", { value: v, writable: false, enumerable: true, configurable: true });
    },
  });
  function set(o, v) { o.c = v; }
  function make() { var o = Object.create(proto); o.a = 1; o.b = 2; return o; }

  var o1 = make(); set(o1, 5);
  assert.sameValue(Object.getOwnPropertyDescriptor(o1, "c").writable, false);

  var o2 = make(); set(o2, 7);
  assert.sameValue(Object.getOwnPropertyDescriptor(o2, "c").writable, true);
  o2.c = 99;
  assert.sameValue(o2.c, 99);
})();

// A self-deleting prototype setter that appends the key as a NON-enumerable property must not be
// cached: a plain store creates an enumerable property, so caching the non-enumerable shape would
// silently drop the property from enumeration (Object.keys / for-in / JSON.stringify).
(function () {
  var proto = {};
  Object.defineProperty(proto, "c", {
    configurable: true,
    set: function (v) {
      delete proto.c;
      Object.defineProperty(this, "c", { value: v, writable: true, enumerable: false, configurable: true });
    },
  });
  function set(o, v) { o.c = v; }
  function make() { var o = Object.create(proto); o.a = 1; o.b = 2; return o; }

  var o1 = make(); set(o1, 5);
  assert.sameValue(Object.getOwnPropertyDescriptor(o1, "c").enumerable, false);

  var o2 = make(); set(o2, 7);
  assert.sameValue(o2.c, 7);
  assert.sameValue(Object.getOwnPropertyDescriptor(o2, "c").enumerable, true);
  assert.sameValue(Object.keys(o2).join(), "a,b,c");
})();

// A self-deleting prototype setter that appends the key as a NON-configurable property must not be
// cached: a plain store creates a configurable property.
(function () {
  var proto = {};
  Object.defineProperty(proto, "c", {
    configurable: true,
    set: function (v) {
      delete proto.c;
      Object.defineProperty(this, "c", { value: v, writable: true, enumerable: true, configurable: false });
    },
  });
  function set(o, v) { o.c = v; }
  function make() { var o = Object.create(proto); o.a = 1; o.b = 2; return o; }

  var o1 = make(); set(o1, 5);
  assert.sameValue(Object.getOwnPropertyDescriptor(o1, "c").configurable, false);

  var o2 = make(); set(o2, 7);
  assert.sameValue(o2.c, 7);
  assert.sameValue(Object.getOwnPropertyDescriptor(o2, "c").configurable, true);
})();
