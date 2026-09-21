/*---
description: GetNamedProperty caches own and prototype properties of map mode objects (objects with more properties than array mode allows) and stays correct across map mutations and prototype changes.
---*/

// Build an object with enough properties to leave array mode.
function makeBig(prefix) {
  var o = {};
  for (var i = 0; i < 100; i++) {
    o[prefix + i] = i;
  }
  return o;
}

// Own data properties on a map mode receiver hit the cache at a warm callsite.
(function () {
  var o = makeBig("p");
  o.x = 1;
  function get(o) { return o.x; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o), 1);
  }

  // Overwriting the value keeps the entry location valid
  o.x = 2;
  assert.sameValue(get(o), 2);

  // Adding a property (possibly growing the map) must not return a stale entry
  for (var i = 0; i < 100; i++) {
    o["q" + i] = i;
  }
  o.x = 3;
  assert.sameValue(get(o), 3);

  // Deleting the property must be observed
  delete o.x;
  assert.sameValue(get(o), undefined);

  // Re-adding after delete lands in a new entry
  o.x = 4;
  assert.sameValue(get(o), 4);
})();

// Attribute changes on a cached entry are observed.
(function () {
  var o = makeBig("p");
  o.x = 1;
  function get(o) { return o.x; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o), 1);
  }

  Object.defineProperty(o, "x", { get: function () { return 42; }, configurable: true });
  assert.sameValue(get(o), 42);

  Object.defineProperty(o, "x", { value: 7, writable: true, configurable: true });
  assert.sameValue(get(o), 7);
})();

// Two map mode objects never share a cache entry, even with the same insertion order.
(function () {
  var a = makeBig("p");
  var b = makeBig("p");
  a.x = "a";
  b.y = "shift";
  b.x = "b";
  function get(o) { return o.x; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(a), "a");
    assert.sameValue(get(b), "b");
  }
})();

// Properties found on a map mode prototype are cached and invalidated on prototype mutation.
(function () {
  var proto = makeBig("m");
  proto.method = function () { return "proto"; };
  var o = Object.create(proto);
  function get(o) { return o.method; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o)(), "proto");
  }

  // Overwriting on the prototype
  proto.method = function () { return "proto2"; };
  assert.sameValue(get(o)(), "proto2");

  // Growing the prototype's map
  for (var i = 0; i < 100; i++) {
    proto["n" + i] = i;
  }
  assert.sameValue(get(o)(), "proto2");

  // Shadowing on the receiver
  o.method = function () { return "own"; };
  assert.sameValue(get(o)(), "own");
  delete o.method;
  assert.sameValue(get(o)(), "proto2");

  // Deleting from the prototype
  delete proto.method;
  assert.sameValue(get(o), undefined);
})();

// A map mode receiver with the property on its (array mode) prototype, then shadowed.
(function () {
  function Ctor() {}
  Ctor.prototype.v = "proto";
  var o = new Ctor();
  for (var i = 0; i < 100; i++) {
    o["p" + i] = i;
  }
  function get(o) { return o.v; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o), "proto");
  }
  o.v = "own";
  assert.sameValue(get(o), "own");
})();

// Missing properties on a map mode receiver are observed once added.
(function () {
  var o = makeBig("p");
  function get(o) { return o.later; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o), undefined);
  }
  o.later = "now";
  assert.sameValue(get(o), "now");
})();

// A cached prototype property hit observes every way of changing the receiver's prototype.
(function () {
  var p1 = { m: "p1" };
  var p2 = { m: "p2" };
  var p3 = { m: "p3" };
  var o = makeBig("p");
  Object.setPrototypeOf(o, p1);
  function get(o) { return o.m; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o), "p1");
  }

  Object.setPrototypeOf(o, p2);
  assert.sameValue(get(o), "p2");

  o.__proto__ = p3;
  assert.sameValue(get(o), "p3");

  assert.sameValue(Reflect.setPrototypeOf(o, p1), true);
  assert.sameValue(get(o), "p1");

  Object.setPrototypeOf(o, null);
  assert.sameValue(get(o), undefined);
  assert.sameValue(Object.getPrototypeOf(o), null);

  // Own properties survive the prototype changes
  assert.sameValue(o.p0, 0);
  assert.sameValue(o.p99, 99);
})();

// A cached absent property observes a new prototype that has the property.
(function () {
  var o = makeBig("p");
  function get(o) { return o.later; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o), undefined);
  }
  Object.setPrototypeOf(o, { later: "found" });
  assert.sameValue(get(o), "found");
})();

// Two map mode objects share a callsite and only one has its prototype changed.
(function () {
  var p1 = { m: "p1" };
  var a = makeBig("p");
  var b = makeBig("p");
  Object.setPrototypeOf(a, p1);
  Object.setPrototypeOf(b, p1);
  function get(o) { return o.m; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(a), "p1");
    assert.sameValue(get(b), "p1");
  }
  Object.setPrototypeOf(a, { m: "p2" });
  assert.sameValue(get(a), "p2");
  assert.sameValue(get(b), "p1");
})();

// Setting the prototype can itself move an object into map mode once its shape has run out of
// transitions. Each object must end up with its own prototype and observe later changes.
(function () {
  var root = {};
  var protos = [];
  var objects = [];
  for (var i = 0; i < 300; i++) {
    protos.push({ m: i });
    objects.push(Object.setPrototypeOf(Object.create(root), protos[i]));
  }
  for (var i = 0; i < 300; i++) {
    assert.sameValue(objects[i].m, i);
    assert.sameValue(Object.getPrototypeOf(objects[i]), protos[i]);
  }

  var last = objects[299];
  function get(o) { return o.m; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(last), 299);
  }
  Object.setPrototypeOf(last, protos[0]);
  assert.sameValue(get(last), 0);
})();

// Overwriting an existing property through a generic (uncached) store is not a shape-changing
// mutation, so it must not move the entries that warm caches point at. This matters when the
// properties map is exactly full, where an insertion would replace the map. Sweep every size
// across several map capacities, with a fresh callsite per size so that it stays cached.
//
// A single object is grown one property at a time instead of building a new object for each size.
// The overwrites leave the map untouched so the object at each size is in the same state as one
// built from scratch.
(function () {
  var o = {};
  for (var i = 0; i < 64; i++) {
    o["p" + i] = i;
  }

  for (var size = 65; size <= 300; size++) {
    var lastKey = "p" + (size - 1);
    o[lastKey] = size - 1;

    var get = new Function("o", "return o.p3;");
    for (var i = 0; i < 5; i++) {
      assert.sameValue(get(o), 3);
    }

    o[lastKey] = "overwritten";
    assert.sameValue(get(o), 3);
    assert.sameValue(o[lastKey], "overwritten");
  }
})();

// The same when the full map is mostly deleted entries, where an insertion would compact the
// map instead of growing it. The cached property sits after the deleted entries so that a
// compaction would move it. Sweep the number of properties added back after the deletes, again
// adding them one at a time to a single object.
(function () {
  var o = makeBig("p");
  for (var i = 0; i < 80; i++) {
    delete o["p" + i];
  }

  for (var added = 0; added <= 80; added++) {
    if (added > 0) {
      o["q" + (added - 1)] = added - 1;
    }

    var get = new Function("o", "return o.p99;");
    for (var i = 0; i < 5; i++) {
      assert.sameValue(get(o), 99);
    }

    o["p80"] = "overwritten" + added;
    assert.sameValue(get(o), 99);
    assert.sameValue(o.p80, "overwritten" + added);
  }
})();

// An object with enough properties that the last ones are stored at an offset too large to be
// represented in a cache. Shared between the tests below since it is expensive to build, so each
// test uses its own key. Keys p2095 to p2099 are all at uncacheable offsets, and only the odd ones
// are also present higher on the prototype chain.
var huge = Object.create({ p2095: "higher", p2097: "higher", p2099: "higher" });
for (var i = 0; i < 2100; i++) {
  huge["p" + i] = i;
}

// An own property at an uncacheable offset is not mistaken for an absent property, so is not
// shadowed by a cached property of the same name on the prototype.
(function () {
  function get(o) { return o.p2099; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(huge), 2099);
  }

  huge.p2099 = "overwritten";
  assert.sameValue(get(huge), "overwritten");

  // Once the own property is deleted the prototype's property is visible
  delete huge.p2099;
  assert.sameValue(get(huge), "higher");
})();

// The same when the property is not on the prototype chain, where absence would be cached.
(function () {
  function get(o) { return o.p2098; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(huge), 2098);
  }

  // Properties at cacheable offsets in the same object are still found
  function getFirst(o) { return o.p0; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(getFirst(huge), 0);
  }
})();

// A property at an uncacheable offset on a map mode prototype is not skipped in favor of a
// property of the same name higher on the prototype chain.
(function () {
  var o = Object.create(huge);
  function get(o) { return o.p2097; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o), 2097);
  }

  huge.p2097 = "overwritten";
  assert.sameValue(get(o), "overwritten");

  delete huge.p2097;
  assert.sameValue(get(o), "higher");
})();

// The same when the property is not higher on the prototype chain, where absence would be cached.
(function () {
  var o = Object.create(huge);
  function get(o) { return o.p2096; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o), 2096);
  }
})();

// The same for a primitive receiver, which looks up the prototype chain of its wrapper's prototype.
(function () {
  var original = Object.getPrototypeOf(String.prototype);
  Object.setPrototypeOf(String.prototype, huge);
  try {
    function get(s) { return s.p2095; }
    for (var i = 0; i < 5; i++) {
      assert.sameValue(get("str"), 2095);
    }
  } finally {
    Object.setPrototypeOf(String.prototype, original);
  }
})();

// A map mode receiver that is mutated between accesses abandons its old shape each time. The dead
// cache entry is replaced in place so the callsite keeps returning the correct value.
(function () {
  var o = makeBig("p");
  o.x = 0;
  function get(o) { return o.x; }
  for (var i = 0; i < 10; i++) {
    o["added" + i] = i;
    o.x = i;
    assert.sameValue(get(o), i);
    assert.sameValue(get(o), i);
  }

  // Deleting, redefining attributes, changing the prototype, and preventing extensions all abandon
  // the old shape too
  delete o.added0;
  assert.sameValue(get(o), 9);
  Object.defineProperty(o, "x", { value: "readonly", writable: false });
  assert.sameValue(get(o), "readonly");
  Object.setPrototypeOf(o, { y: 1 });
  assert.sameValue(get(o), "readonly");
  Object.preventExtensions(o);
  assert.sameValue(get(o), "readonly");
  delete o.x;
  assert.sameValue(get(o), undefined);
})();

// A repeatedly mutated map mode receiver only uses a single entry of a polymorphic cache, so
// array mode receivers at the same callsite stay cached and correct. Mutates more times than there
// are polymorphic cache entries.
(function () {
  var a = { x: "a" };
  var b = { other: 0, x: "b" };
  var o = makeBig("p");
  o.x = 0;
  function get(o) { return o.x; }
  assert.sameValue(get(a), "a");
  assert.sameValue(get(b), "b");
  for (var i = 0; i < 10; i++) {
    o["added" + i] = i;
    o.x = i;
    assert.sameValue(get(o), i);
    assert.sameValue(get(a), "a");
    assert.sameValue(get(b), "b");
  }

  // Absent and prototype properties are keyed on the abandoned shapes too
  function getMissing(o) { return o.missing; }
  function getInherited(o) { return o.hasOwnProperty; }
  getMissing(a); getMissing(b); getInherited(a); getInherited(b);
  for (var i = 0; i < 10; i++) {
    o["more" + i] = i;
    assert.sameValue(getMissing(o), undefined);
    assert.sameValue(getInherited(o), Object.prototype.hasOwnProperty);
  }
  o.missing = "found";
  assert.sameValue(getMissing(o), "found");
})();

// Distinct map mode receivers that are not mutated each keep their own polymorphic cache entry,
// and mutating one of them does not disturb the other.
(function () {
  var o1 = makeBig("p");
  var o2 = makeBig("q");
  o1.x = 1;
  o2.x = 2;
  function get(o) { return o.x; }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(o1), 1);
    assert.sameValue(get(o2), 2);
  }

  for (var i = 0; i < 6; i++) {
    o1["added" + i] = i;
    assert.sameValue(get(o1), 1);
    assert.sameValue(get(o2), 2);
  }
})();

// A map mode receiver abandons its shape when it becomes a prototype object.
(function () {
  var o = makeBig("p");
  o.x = "proto";
  function get(o) { return o.x; }
  assert.sameValue(get(o), "proto");
  assert.sameValue(get(o), "proto");

  // Requesting a guard for the child's prototype chain converts `o` to a prototype object shape
  var child = Object.create(o);
  function getChild(o) { return o.x; }
  assert.sameValue(getChild(child), "proto");
  assert.sameValue(getChild(child), "proto");

  var other = makeBig("q");
  other.x = "other";
  assert.sameValue(get(other), "other");
  assert.sameValue(get(other), "other");
  assert.sameValue(get(o), "proto");

  o.x = "changed";
  assert.sameValue(get(o), "changed");
  assert.sameValue(getChild(child), "changed");
})();
