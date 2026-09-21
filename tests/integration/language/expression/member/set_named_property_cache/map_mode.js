/*---
description: SetNamedProperty caches stores to existing properties of map mode objects and stays correct across map mutations, prototype changes, and preventExtensions.
flags: [noStrict]
---*/

function makeBig(prefix) {
  var o = {};
  for (var i = 0; i < 100; i++) {
    o[prefix + i] = i;
  }
  return o;
}

// Stores to an existing writable property on a map mode receiver hit the cache.
(function () {
  var o = makeBig("p");
  o.x = 0;
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 5; i++) {
    set(o, i);
    assert.sameValue(o.x, i);
  }

  // Growing the map moves entries; the cached location must not be reused
  for (var i = 0; i < 100; i++) {
    o["q" + i] = i;
  }
  set(o, "after grow");
  assert.sameValue(o.x, "after grow");
  assert.sameValue(o.q99, 99);
  assert.sameValue(o.p0, 0);

  // Delete then store re-adds the property
  delete o.x;
  set(o, "readded");
  assert.sameValue(o.x, "readded");
  assert.sameValue(Object.keys(o).indexOf("x"), Object.keys(o).length - 1);
})();

// Making a cached entry non-writable stops the cached store.
(function () {
  var o = makeBig("p");
  o.x = 0;
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 5; i++) {
    set(o, i);
  }
  Object.defineProperty(o, "x", { writable: false });
  set(o, 99);
  assert.sameValue(o.x, 4);
})();

// Turning a cached data entry into an accessor calls the setter.
(function () {
  var o = makeBig("p");
  o.x = 0;
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 5; i++) {
    set(o, i);
  }
  var observed;
  Object.defineProperty(o, "x", { set: function (v) { observed = v; }, configurable: true });
  set(o, "setter");
  assert.sameValue(observed, "setter");
})();

// Two map mode objects do not share cached entry locations.
(function () {
  var a = makeBig("p");
  var b = makeBig("p");
  b.shift = 1;
  a.x = 0;
  b.x = 0;
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 5; i++) {
    set(a, "a" + i);
    set(b, "b" + i);
  }
  assert.sameValue(a.x, "a4");
  assert.sameValue(b.x, "b4");
  assert.sameValue(b.shift, 1);
})();

// Adding a new property to a map mode receiver at a warm callsite is not cached as a transition.
(function () {
  function set(o, v) { o.fresh = v; }
  for (var i = 0; i < 5; i++) {
    var o = makeBig("p");
    set(o, i);
    assert.sameValue(o.fresh, i);
    assert.sameValue(o.p99, 99);
  }
})();

// A store that reaches a setter on the prototype observes a prototype change.
(function () {
  var log = [];
  var o = makeBig("p");
  Object.setPrototypeOf(o, { set x(v) { log.push("p1:" + v); } });
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 3; i++) {
    set(o, i);
  }
  Object.setPrototypeOf(o, { set x(v) { log.push("p2:" + v); } });
  set(o, "after");
  assert.sameValue(log.join(","), "p1:0,p1:1,p1:2,p2:after");
  assert.sameValue(Object.prototype.hasOwnProperty.call(o, "x"), false);
})();

// A cached store to an own property ignores a setter on a new prototype.
(function () {
  var called = false;
  var o = makeBig("p");
  o.x = 0;
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 5; i++) {
    set(o, i);
  }
  Object.setPrototypeOf(o, { set x(v) { called = true; } });
  set(o, "own");
  assert.sameValue(o.x, "own");
  assert.sameValue(called, false);
})();

// A store blocked by a non-writable property on the prototype is allowed once the prototype
// changes.
(function () {
  var o = makeBig("p");
  function set(o, v) { o.y = v; }
  Object.setPrototypeOf(o, Object.freeze({ y: "readonly" }));
  for (var i = 0; i < 5; i++) {
    set(o, i);
    assert.sameValue(o.y, "readonly");
  }
  Object.setPrototypeOf(o, {});
  set(o, "writable now");
  assert.sameValue(o.y, "writable now");
})();

// After preventExtensions a cached store to an existing property keeps working while stores of
// new properties are rejected, and after freeze the cached store is ignored too.
(function () {
  var o = makeBig("p");
  o.x = 0;
  function set(o, v) { o.x = v; }
  function add(o, v) { o.fresh = v; }
  function addStrict(o, v) { "use strict"; o.fresh = v; }
  for (var i = 0; i < 5; i++) {
    set(o, i);
  }

  Object.preventExtensions(o);
  assert.sameValue(Object.isExtensible(o), false);
  set(o, "still writable");
  assert.sameValue(o.x, "still writable");

  add(o, 1);
  assert.sameValue(Object.prototype.hasOwnProperty.call(o, "fresh"), false);
  assert.throws(TypeError, function () { addStrict(o, 1); });

  Object.freeze(o);
  set(o, "ignored");
  assert.sameValue(o.x, "still writable");
})();

// A callsite that adds a property, warmed on extensible map mode receivers, rejects a
// non-extensible one without affecting the others.
(function () {
  function add(o, v) { o.fresh = v; }
  var extensible = makeBig("p");
  for (var i = 0; i < 5; i++) {
    add(extensible, i);
  }
  var locked = Object.preventExtensions(makeBig("p"));
  add(locked, "no");
  assert.sameValue(locked.fresh, undefined);

  add(extensible, "yes");
  assert.sameValue(extensible.fresh, "yes");
  assert.sameValue(Object.isExtensible(extensible), true);
})();

// preventExtensions can itself move an object into map mode once its shape has run out of
// transitions. The object must end up non-extensible without affecting its siblings.
(function () {
  var root = {};
  for (var i = 0; i < 300; i++) {
    Object.create(root)["k" + i] = i;
  }
  function add(o, v) { o.fresh = v; }

  var o = Object.preventExtensions(Object.create(root));
  assert.sameValue(Object.isExtensible(o), false);
  add(o, 1);
  assert.sameValue(o.fresh, undefined);

  var sibling = Object.create(root);
  add(sibling, 2);
  assert.sameValue(sibling.fresh, 2);
  assert.sameValue(Object.isExtensible(sibling), true);
})();

// A map mode receiver that is mutated between stores abandons its old shape each time. The dead
// cache entry is replaced in place so the callsite keeps storing correctly.
(function () {
  var o = makeBig("p");
  o.x = -1;
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 10; i++) {
    o["added" + i] = i;
    set(o, i);
    assert.sameValue(o.x, i);
    set(o, i + 100);
    assert.sameValue(o.x, i + 100);
  }

  // A store through a replaced entry must respect a later attribute change
  Object.defineProperty(o, "x", { value: "readonly", writable: false });
  set(o, "ignored");
  assert.sameValue(o.x, "readonly");
})();

// A repeatedly mutated map mode receiver only uses a single entry of a polymorphic cache, so
// array mode receivers at the same callsite stay cached and correct. Mutates more times than there
// are polymorphic cache entries.
(function () {
  var a = { x: 0 };
  var b = { other: 0, x: 0 };
  var o = makeBig("p");
  o.x = 0;
  function set(o, v) { o.x = v; }
  set(a, -1);
  set(b, -1);
  for (var i = 0; i < 10; i++) {
    o["added" + i] = i;
    set(o, i);
    set(o, i);
    set(a, i);
    set(b, i);
    assert.sameValue(o.x, i);
    assert.sameValue(a.x, i);
    assert.sameValue(b.x, i);
  }
  assert.sameValue(o.added9, 9);
  assert.sameValue(o.p0, 0);
})();

// A callsite whose first store adds the property to a map mode receiver keeps working, and caches,
// once later stores overwrite that property. The same callsite then handles other receivers.
(function () {
  var o = makeBig("p");
  function set(o, v) { o.added = v; }
  for (var i = 0; i < 5; i++) {
    set(o, i);
    assert.sameValue(o.added, i);
  }
  assert.sameValue(Object.keys(o).length, 101);

  // Deleting the property means the next store adds it again at a new location
  delete o.added;
  o.other = "other";
  set(o, "readded");
  assert.sameValue(o.added, "readded");
  assert.sameValue(o.other, "other");
  set(o, "overwritten");
  assert.sameValue(o.added, "overwritten");

  // A cached overwrite must respect the property becoming non-writable
  Object.defineProperty(o, "added", { writable: false });
  set(o, "ignored");
  assert.sameValue(o.added, "overwritten");

  // Array mode receivers at the same callsite, both adding and overwriting
  for (var i = 0; i < 5; i++) {
    var small = { a: 1 };
    set(small, i);
    assert.sameValue(small.added, i);
    set(small, "again");
    assert.sameValue(small.added, "again");
  }
})();

// The same for a store that moves the receiver from array mode into map mode.
(function () {
  function set(o, v) { o.added = v; }
  for (var j = 0; j < 3; j++) {
    var o = {};
    var i = 0;
    // Add properties through the callsite's own receiver until it enters map mode
    while (i < 200) {
      o["p" + i] = i;
      i++;
    }
    delete o.p0;
    set(o, "first");
    assert.sameValue(o.added, "first");
    set(o, "second");
    assert.sameValue(o.added, "second");
    assert.sameValue(o.p199, 199);
  }
})();

// A setter on a map mode prototype is called for array mode receivers and observes changes to the
// prototype.
(function () {
  var log = [];
  var proto = makeBig("p");
  Object.defineProperty(proto, "x", {
    set: function (v) { log.push("s1:" + v); },
    configurable: true,
  });
  function set(o, v) { o.x = v; }

  var o = Object.create(proto);
  for (var i = 0; i < 5; i++) {
    set(o, i);
  }
  assert.sameValue(log.join(","), "s1:0,s1:1,s1:2,s1:3,s1:4");
  assert.sameValue(Object.prototype.hasOwnProperty.call(o, "x"), false);

  // Unrelated mutation of the prototype may move or keep the setter's entry
  log = [];
  for (var i = 0; i < 100; i++) {
    proto["q" + i] = i;
  }
  delete proto.p0;
  set(o, "moved");
  assert.sameValue(log.join(","), "s1:moved");

  // Replacing the setter
  log = [];
  Object.defineProperty(proto, "x", { set: function (v) { log.push("s2:" + v); } });
  set(o, "replaced");
  assert.sameValue(log.join(","), "s2:replaced");

  // Replacing the setter with a writable data property means stores create an own property
  log = [];
  Object.defineProperty(proto, "x", { value: "data", writable: true });
  set(o, "own");
  assert.sameValue(log.length, 0);
  assert.sameValue(o.x, "own");
  assert.sameValue(proto.x, "data");
})();

// The same with a map mode receiver as well as a map mode prototype.
(function () {
  var log = [];
  var proto = makeBig("p");
  Object.defineProperty(proto, "x", {
    set: function (v) { log.push(v); },
    configurable: true,
  });
  var o = makeBig("r");
  Object.setPrototypeOf(o, proto);
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 5; i++) {
    set(o, i);
  }
  assert.sameValue(log.join(","), "0,1,2,3,4");

  delete proto.x;
  set(o, "own");
  assert.sameValue(log.join(","), "0,1,2,3,4");
  assert.sameValue(o.x, "own");
})();

// A writable data property on a map mode prototype is shadowed by stores, which are cached as
// adding an own property. Making the prototype's property non-writable must then block the store.
(function () {
  var proto = makeBig("p");
  proto.x = "proto";
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 5; i++) {
    var o = Object.create(proto);
    set(o, i);
    assert.sameValue(o.x, i);
    assert.sameValue(Object.prototype.hasOwnProperty.call(o, "x"), true);
  }
  assert.sameValue(proto.x, "proto");

  Object.defineProperty(proto, "x", { writable: false });
  var blocked = Object.create(proto);
  set(blocked, "blocked");
  assert.sameValue(blocked.x, "proto");
  assert.sameValue(Object.prototype.hasOwnProperty.call(blocked, "x"), false);

  // And a setter on the prototype must be called instead of adding an own property
  var log = [];
  Object.defineProperty(proto, "x", { set: function (v) { log.push(v); } });
  var viaSetter = Object.create(proto);
  set(viaSetter, "setter");
  assert.sameValue(log.join(","), "setter");
  assert.sameValue(Object.prototype.hasOwnProperty.call(viaSetter, "x"), false);
})();

// A non-writable data property on a map mode prototype blocks stores, in strict mode by throwing,
// until it is made writable.
(function () {
  "use strict";
  var proto = makeBig("p");
  Object.defineProperty(proto, "x", { value: "readonly", writable: false, configurable: true });
  function set(o, v) { o.x = v; }
  for (var i = 0; i < 5; i++) {
    var o = Object.create(proto);
    assert.throws(TypeError, function () { set(o, i); });
    assert.sameValue(o.x, "readonly");
  }

  Object.defineProperty(proto, "x", { writable: true });
  var o = Object.create(proto);
  set(o, "own");
  assert.sameValue(o.x, "own");
  assert.sameValue(proto.x, "readonly");
})();

// A setter on a map mode prototype at an offset too large to be represented in a cache is still
// called, instead of being skipped for a property higher on the prototype chain.
(function () {
  var log = [];
  var higher = { set last(v) { log.push("higher:" + v); } };
  var proto = Object.create(higher);
  for (var i = 0; i < 2100; i++) {
    proto["p" + i] = i;
  }
  Object.defineProperty(proto, "last", { set: function (v) { log.push("proto:" + v); } });

  var o = Object.create(proto);
  function set(o, v) { o.last = v; }
  for (var i = 0; i < 3; i++) {
    set(o, i);
  }
  assert.sameValue(log.join(","), "proto:0,proto:1,proto:2");
  assert.sameValue(Object.prototype.hasOwnProperty.call(o, "last"), false);
})();
