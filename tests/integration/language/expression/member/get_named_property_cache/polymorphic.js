/*---
description: >
  GetNamedProperty cache promotes to a polymorphic cache when a callsite sees several receiver
  shapes, and stays correct for every shape it holds.
---*/

// Two shapes at one callsite: both are cached and read the property from their own location.
(function () {
  function get(o) { return o.x; }
  var a = { x: 1 };
  var b = { pad: 0, x: 2 };
  for (var i = 0; i < 5; i++) {
    assert.sameValue(get(a), 1);
    assert.sameValue(get(b), 2);
  }
  a.x = 10;
  b.x = 20;
  assert.sameValue(get(a), 10);
  assert.sameValue(get(b), 20);
})();

// Four shapes fill the polymorphic cache exactly, in both fill order and reverse order.
(function () {
  function get(o) { return o.x; }
  var os = [
    { x: 1 },
    { p1: 0, x: 2 },
    { p1: 0, p2: 0, x: 3 },
    { p1: 0, p2: 0, p3: 0, x: 4 },
  ];
  for (var i = 0; i < os.length; i++) {
    assert.sameValue(get(os[i]), i + 1);
  }
  for (var round = 0; round < 3; round++) {
    for (var j = os.length - 1; j >= 0; j--) {
      assert.sameValue(get(os[j]), j + 1);
    }
  }
})();

// A fifth shape overflows the polymorphic cache. Caching stops but every shape still reads
// correctly, including the four that were cached before the overflow.
(function () {
  function get(o) { return o.x; }
  var four = [
    { x: 1 },
    { p1: 0, x: 2 },
    { p1: 0, p2: 0, x: 3 },
    { p1: 0, p2: 0, p3: 0, x: 4 },
  ];
  var fifth = { p1: 0, p2: 0, p3: 0, p4: 0, x: 5 };

  // Warm every entry of the polymorphic cache first.
  for (var round = 0; round < 2; round++) {
    for (var i = 0; i < four.length; i++) {
      assert.sameValue(get(four[i]), i + 1);
    }
  }

  // The fifth shape does not fit, so caching stops at this callsite.
  assert.sameValue(get(fifth), 5);

  for (var round2 = 0; round2 < 2; round2++) {
    for (var j = 0; j < four.length; j++) {
      assert.sameValue(get(four[j]), j + 1);
    }
    assert.sameValue(get(fifth), 5);
  }

  four[0].x = 100;
  assert.sameValue(get(four[0]), 100);
})();

// A single polymorphic cache holds entries of every kind at once: an own data property, an own
// accessor, a prototype data property and an absent property.
(function () {
  var ownData = { x: 1 };
  var ownAccessor = { pad: 0, get x() { return 2; } };
  var protoData = Object.create({ x: 3 });
  var absent = { pad: 0, other: 0 };

  function get(o) { return o.x; }

  for (var i = 0; i < 4; i++) {
    assert.sameValue(get(ownData), 1);
    assert.sameValue(get(ownAccessor), 2);
    assert.sameValue(get(protoData), 3);
    assert.sameValue(get(absent), undefined);
  }
})();

// Each polymorphic entry tracks its own property location, including properties that live in the
// external property array rather than the object's inline slots.
(function () {
  function make(n) {
    var o = {};
    for (var i = 0; i < n; i++) {
      o["f" + i] = i;
    }
    o.x = n;
    return o;
  }
  function get(o) { return o.x; }

  var os = [make(0), make(2), make(6), make(9)];
  for (var round = 0; round < 3; round++) {
    for (var i = 0; i < os.length; i++) {
      assert.sameValue(get(os[i]), [0, 2, 6, 9][i]);
    }
  }
})();

// A polymorphic accessor entry calls the getter of the matching shape with the correct receiver.
(function () {
  var seen = [];
  var a = { tag: "a", get x() { seen.push(this.tag); return this.tag; } };
  var b = { pad: 0, tag: "b", get x() { seen.push(this.tag); return this.tag; } };
  var c = Object.create({ get x() { seen.push(this.tag); return this.tag; } });
  c.tag = "c";

  function get(o) { return o.x; }

  assert.sameValue(get(a), "a");
  assert.sameValue(get(b), "b");
  assert.sameValue(get(c), "c");
  assert.sameValue(get(a), "a");
  assert.sameValue(get(b), "b");
  assert.sameValue(get(c), "c");
  assert.sameValue(seen.join(), "a,b,c,a,b,c");
})();

// Instances of different classes at one callsite each resolve the method from their own
// prototype, and redefining a method on one prototype only affects that class.
(function () {
  class A { m() { return "A"; } }
  class B { m() { return "B"; } }
  class C extends A {}
  class D extends A { m() { return "D"; } }

  function get(o) { return o.m; }
  var a = new A(), b = new B(), c = new C(), d = new D();

  for (var i = 0; i < 3; i++) {
    assert.sameValue(get(a).call(a), "A");
    assert.sameValue(get(b).call(b), "B");
    assert.sameValue(get(c).call(c), "A");
    assert.sameValue(get(d).call(d), "D");
  }

  // A.prototype is on the prototype chain of A, C and D instances, so this invalidates those
  // three entries even though D shadows the method with its own.
  A.prototype.m = function () { return "A2"; };
  assert.sameValue(get(a).call(a), "A2");
  assert.sameValue(get(c).call(c), "A2");
  assert.sameValue(get(b).call(b), "B");
  assert.sameValue(get(d).call(d), "D");
  assert.sameValue(get(a).call(a), "A2");
})();

// Two receivers whose properties were added in different orders have different shapes, so the
// callsite becomes polymorphic even though the objects look alike.
(function () {
  function get(o) { return o.x; }
  var a = {};
  a.x = 1;
  a.y = 2;
  var b = {};
  b.y = 2;
  b.x = 1;
  assert.sameValue(get(a), 1);
  assert.sameValue(get(b), 1);
  assert.sameValue(get(a), 1);
  a.x = 3;
  b.x = 4;
  assert.sameValue(get(a), 3);
  assert.sameValue(get(b), 4);
})();

// A prototype mutation invalidates the guard of one polymorphic entry. That entry is refilled
// while the other entries keep working.
(function () {
  var proto = { x: "proto" };
  var viaProto = Object.create(proto);
  var own = { x: "own" };

  function get(o) { return o.x; }

  assert.sameValue(get(own), "own");
  assert.sameValue(get(viaProto), "proto");
  assert.sameValue(get(own), "own");

  // Move the property one level further up the chain, invalidating the Proto entry's guard.
  Object.setPrototypeOf(proto, { x: "grandproto" });
  delete proto.x;
  assert.sameValue(get(viaProto), "grandproto");
  assert.sameValue(get(own), "own");
  assert.sameValue(get(viaProto), "grandproto");
})();

// An entry for an absent property is replaced in place, at the same shape, once the property
// appears on the prototype chain.
(function () {
  var proto = {};
  var absent = Object.create(proto);
  var other = { x: "other" };

  function get(o) { return o.x; }

  assert.sameValue(get(absent), undefined);
  assert.sameValue(get(other), "other");
  assert.sameValue(get(absent), undefined);

  proto.x = "found";
  assert.sameValue(get(absent), "found");
  assert.sameValue(get(absent), "found");
  assert.sameValue(get(other), "other");
})();

// A receiver that cannot be cached at all does not disturb the entries already in a polymorphic
// cache: the uncacheable receiver keeps taking the slow path while the others stay correct.
(function () {
  var traps = 0;
  var a = { x: 1 };
  var b = { pad: 0, x: 2 };
  var proxy = new Proxy({}, { get: function () { return ++traps; } });

  function get(o) { return o.x; }

  assert.sameValue(get(a), 1);
  assert.sameValue(get(b), 2);

  for (var i = 0; i < 3; i++) {
    assert.sameValue(get(proxy), i + 1);
    assert.sameValue(get(a), 1);
    assert.sameValue(get(b), 2);
  }
  assert.sameValue(traps, 3);

  a.x = 11;
  assert.sameValue(get(a), 11);
})();

// A dictionary (map mode) receiver is also uncacheable and mixes safely into a polymorphic
// callsite.
(function () {
  var dict = {};
  for (var i = 0; i < 80; i++) {
    dict["k" + i] = i;
  }
  dict.x = "dict";
  var a = { x: "a" };
  var b = { pad: 0, x: "b" };

  function get(o) { return o.x; }

  assert.sameValue(get(a), "a");
  assert.sameValue(get(b), "b");
  for (var j = 0; j < 3; j++) {
    assert.sameValue(get(dict), "dict");
    assert.sameValue(get(a), "a");
  }
  dict.x = "dict2";
  assert.sameValue(get(dict), "dict2");
  assert.sameValue(get(b), "b");
})();

// Array length is exotic and never cached, but mixing arrays into a polymorphic callsite must
// still report live lengths.
(function () {
  function get(o) { return o.length; }
  var a = { length: 1 };
  var b = { pad: 0, length: 2 };
  var arr = [0, 0, 0];

  assert.sameValue(get(a), 1);
  assert.sameValue(get(b), 2);
  assert.sameValue(get(arr), 3);
  arr.push(0);
  assert.sameValue(get(arr), 4);
  assert.sameValue(get(a), 1);
  assert.sameValue(get(b), 2);
  assert.sameValue(get(arr), 4);
})();

// A receiver that cannot be cached arriving while the callsite is still monomorphic stops
// caching there entirely. Later shapes never reach the polymorphic cache but still read
// correctly.
(function () {
  var traps = 0;
  function get(o) { return o.x; }
  var a = { x: 1 };
  var b = { pad: 0, x: 2 };
  var proxy = new Proxy({}, { get: function () { return ++traps; } });

  assert.sameValue(get(a), 1);
  assert.sameValue(get(proxy), 1);
  for (var i = 0; i < 3; i++) {
    assert.sameValue(get(a), 1);
    assert.sameValue(get(b), 2);
  }
  assert.sameValue(get(proxy), 2);
  assert.sameValue(traps, 2);

  b.x = 22;
  assert.sameValue(get(b), 22);
})();

// A getter that re-enters the same callsite with a different shape leaves both reads correct.
(function () {
  var plain = { pad: 0, x: "plain" };
  var reentrant = {
    get x() { return "re:" + get(plain); },
  };

  function get(o) { return o.x; }

  assert.sameValue(get(plain), "plain");
  assert.sameValue(get(reentrant), "re:plain");
  assert.sameValue(get(reentrant), "re:plain");
  assert.sameValue(get(plain), "plain");
})();

// A polymorphic cache survives garbage collection with all of its entries intact.
(function () {
  function get(o) { return o.x; }
  var a = { x: 1 };
  var b = { p1: 0, x: 2 };
  var c = Object.create({ x: 3 });
  var d = { p1: 0, p2: 0, other: 0 };

  assert.sameValue(get(a), 1);
  assert.sameValue(get(b), 2);
  assert.sameValue(get(c), 3);
  assert.sameValue(get(d), undefined);

  $262.gc();

  assert.sameValue(get(a), 1);
  assert.sameValue(get(b), 2);
  assert.sameValue(get(c), 3);
  assert.sameValue(get(d), undefined);

  $262.gc();

  a.x = 4;
  assert.sameValue(get(a), 4);
})();

// Receivers from another realm have different shapes, so a shared callsite goes polymorphic
// across realm boundaries.
(function () {
  var other = $262.createRealm();
  other.evalScript("globalThis.make = function () { return { x: 'other' }; };");

  function get(o) { return o.x; }

  var mine = { x: "mine" };
  var theirs = other.global.make();

  assert.sameValue(get(mine), "mine");
  assert.sameValue(get(theirs), "other");
  assert.sameValue(get(mine), "mine");
  assert.sameValue(get(theirs), "other");
})();
