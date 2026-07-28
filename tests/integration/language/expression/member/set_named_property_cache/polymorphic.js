/*---
description: >
  SetNamedProperty cache promotes to a polymorphic cache when a callsite sees several receiver
  shapes, and stays correct for every shape it holds.
flags: [noStrict]
---*/

// Two shapes at one callsite: each stores into its own property location.
(function () {
  function set(o, v) { o.x = v; }
  var a = { x: 0 };
  var b = { pad: 0, x: 0 };
  for (var i = 0; i < 5; i++) {
    set(a, i);
    set(b, i * 10);
  }
  assert.sameValue(a.x, 4);
  assert.sameValue(b.x, 40);
  assert.sameValue(a.pad, undefined);
  assert.sameValue(b.pad, 0);
})();

// Four shapes fill the polymorphic cache exactly, and stay correct in reverse order too.
(function () {
  function set(o, v) { o.x = v; }
  var os = [
    { x: 0 },
    { p1: 0, x: 0 },
    { p1: 0, p2: 0, x: 0 },
    { p1: 0, p2: 0, p3: 0, x: 0 },
  ];
  for (var i = 0; i < os.length; i++) {
    set(os[i], i);
  }
  for (var round = 0; round < 3; round++) {
    for (var j = os.length - 1; j >= 0; j--) {
      set(os[j], j * 100 + round);
      assert.sameValue(os[j].x, j * 100 + round);
    }
  }
})();

// A fifth shape overflows the polymorphic cache. Caching stops but every shape still stores
// correctly, including the four that were cached before the overflow.
(function () {
  function set(o, v) { o.x = v; }
  var four = [
    { x: 0 },
    { p1: 0, x: 0 },
    { p1: 0, p2: 0, x: 0 },
    { p1: 0, p2: 0, p3: 0, x: 0 },
  ];
  var fifth = { p1: 0, p2: 0, p3: 0, p4: 0, x: 0 };

  // Warm every entry of the polymorphic cache first.
  for (var round = 0; round < 2; round++) {
    for (var i = 0; i < four.length; i++) {
      set(four[i], i * 10 + round);
      assert.sameValue(four[i].x, i * 10 + round);
    }
  }

  // The fifth shape does not fit, so caching stops at this callsite.
  set(fifth, 50);
  assert.sameValue(fifth.x, 50);

  for (var round2 = 2; round2 < 4; round2++) {
    for (var j = 0; j < four.length; j++) {
      set(four[j], j * 10 + round2);
      assert.sameValue(four[j].x, j * 10 + round2);
    }
    set(fifth, 50 + round2);
    assert.sameValue(fifth.x, 50 + round2);
  }
})();

// A single polymorphic cache holds entries of every kind at once: an own data property, an own
// setter, a prototype setter and a store that adds a new property via a shape transition.
(function () {
  var ownSetterValues = [];
  var protoSetterValues = [];
  var protoReceivers = [];

  var accessorProto = {};
  Object.defineProperty(accessorProto, "x", {
    set: function (v) { protoSetterValues.push(v); protoReceivers.push(this); },
    configurable: true,
  });

  var ownData = { x: 0 };
  var ownSetter = { pad: 0, set x(v) { ownSetterValues.push(v); } };
  var viaProto = Object.create(accessorProto);
  function makeFresh() { return { q: 0 }; }

  function set(o, v) { o.x = v; }

  for (var i = 0; i < 4; i++) {
    set(ownData, i);
    set(ownSetter, i);
    set(viaProto, i);

    var fresh = makeFresh();
    set(fresh, i);
    assert.sameValue(fresh.x, i);
    assert.sameValue(fresh.q, 0);
  }

  assert.sameValue(ownData.x, 3);
  assert.sameValue(ownSetterValues.join(), "0,1,2,3");
  assert.sameValue(protoSetterValues.join(), "0,1,2,3");
  assert.sameValue(protoReceivers[0], viaProto);
  assert.sameValue(protoReceivers[3], viaProto);
  assert.sameValue(viaProto.hasOwnProperty("x"), false);
})();

// Transition stores from several different source shapes each append the new property in the
// right place, including when the new property lands in the external property array.
(function () {
  function make(n) {
    var o = {};
    for (var i = 0; i < n; i++) {
      o["f" + i] = i;
    }
    return o;
  }
  function set(o, v) { o.x = v; }

  var counts = [0, 3, 7, 10];
  for (var round = 0; round < 3; round++) {
    for (var i = 0; i < counts.length; i++) {
      var o = make(counts[i]);
      set(o, "v" + i);
      assert.sameValue(o.x, "v" + i);
      assert.sameValue(Object.keys(o).length, counts[i] + 1);
      if (counts[i] > 0) {
        assert.sameValue(o.f0, 0);
        assert.sameValue(o["f" + (counts[i] - 1)], counts[i] - 1);
      }
    }
  }
})();

// The property added by a polymorphic transition store has the attributes of a plain store.
(function () {
  function set(o, v) { o.x = v; }
  var a = { p1: 0 };
  var b = { p2: 0, p3: 0 };
  set(a, 1);
  set(b, 2);
  var a2 = { p1: 0 };
  var b2 = { p2: 0, p3: 0 };
  set(a2, 3);
  set(b2, 4);
  [a2, b2].forEach(function (o) {
    var desc = Object.getOwnPropertyDescriptor(o, "x");
    assert.sameValue(desc.writable, true);
    assert.sameValue(desc.enumerable, true);
    assert.sameValue(desc.configurable, true);
  });
})();

// A prototype setter added after the callsite is polymorphic must intercept later stores for the
// shape it applies to, without disturbing the other cached shapes.
(function () {
  var calls = 0;
  var proto = {};
  function make() { var o = Object.create(proto); o.pad = 0; return o; }
  function set(o, v) { o.x = v; }

  var plain = { x: 0 };
  set(plain, 1);
  set(make(), 1);
  set(plain, 2);

  Object.defineProperty(proto, "x", { set: function (v) { calls++; }, configurable: true });

  var late = make();
  set(late, 3);
  assert.sameValue(calls, 1);
  assert.sameValue(late.hasOwnProperty("x"), false);

  set(plain, 4);
  assert.sameValue(plain.x, 4);
})();

// A non-writable prototype data property blocks stores for one shape of a polymorphic callsite.
// The store cannot be cached, but the other entries keep working.
(function () {
  var proto = {};
  Object.defineProperty(proto, "x", { value: 99, writable: false, configurable: true });
  function make() { var o = Object.create(proto); o.pad = 0; return o; }

  function set(o, v) { o.x = v; }

  var a = { x: 0 };
  var b = { p1: 0, x: 0 };
  set(a, 1);
  set(b, 2);

  for (var i = 0; i < 3; i++) {
    var blocked = make();
    set(blocked, 5);
    assert.sameValue(blocked.hasOwnProperty("x"), false);
    assert.sameValue(blocked.x, 99);

    set(a, 10 + i);
    set(b, 20 + i);
    assert.sameValue(a.x, 10 + i);
    assert.sameValue(b.x, 20 + i);
  }
})();

// The blocked store above throws in strict mode, while the other shapes still store.
(function () {
  "use strict";
  var proto = {};
  Object.defineProperty(proto, "x", { value: 99, writable: false, configurable: true });
  function make() { var o = Object.create(proto); o.pad = 0; return o; }

  function set(o, v) { o.x = v; }

  var a = { x: 0 };
  var b = { p1: 0, x: 0 };
  set(a, 1);
  set(b, 2);

  for (var i = 0; i < 3; i++) {
    assert.throws(TypeError, function () { set(make(), 5); });
    set(a, 10 + i);
    set(b, 20 + i);
  }
  assert.sameValue(a.x, 12);
  assert.sameValue(b.x, 22);
})();

// A non-writable own data property is not cacheable and mixes safely into a polymorphic callsite.
(function () {
  function set(o, v) { o.x = v; }
  var a = { x: 0 };
  var b = { p1: 0, x: 0 };
  var frozen = { p1: 0, p2: 0, x: 7 };
  Object.defineProperty(frozen, "x", { writable: false });

  set(a, 1);
  set(b, 2);
  for (var i = 0; i < 3; i++) {
    set(frozen, 100);
    assert.sameValue(frozen.x, 7);
    set(a, 10 + i);
    set(b, 20 + i);
  }
  assert.sameValue(a.x, 12);
  assert.sameValue(b.x, 22);
})();

// A proxy receiver is never cacheable and must run its trap on every store, leaving the other
// polymorphic entries intact.
(function () {
  var traps = [];
  function set(o, v) { o.x = v; }
  var a = { x: 0 };
  var b = { p1: 0, x: 0 };
  var proxy = new Proxy({}, {
    set: function (t, k, v) { traps.push(v); return true; },
  });

  set(a, 1);
  set(b, 2);
  for (var i = 0; i < 3; i++) {
    set(proxy, i);
    set(a, 10 + i);
    set(b, 20 + i);
  }
  assert.sameValue(traps.join(), "0,1,2");
  assert.sameValue(a.x, 12);
  assert.sameValue(b.x, 22);
})();

// A receiver that cannot be cached arriving while the callsite is still monomorphic stops
// caching there entirely. Later shapes never reach a polymorphic cache but still store
// correctly.
(function () {
  var traps = [];
  function set(o, v) { o.x = v; }
  var a = { x: 0 };
  var b = { pad: 0, x: 0 };
  var proxy = new Proxy({}, {
    set: function (t, k, v) { traps.push(v); return true; },
  });

  set(a, 1);
  set(proxy, 2);
  for (var i = 0; i < 3; i++) {
    set(a, 10 + i);
    set(b, 20 + i);
    assert.sameValue(a.x, 10 + i);
    assert.sameValue(b.x, 20 + i);
  }
  set(proxy, 3);
  assert.sameValue(traps.join(), "2,3");
})();

// Array length stores are exotic and never cached, but keep their behavior at a polymorphic
// callsite.
(function () {
  function set(o, v) { o.length = v; }
  var a = { length: 0 };
  var b = { pad: 0, length: 0 };
  var arr = [1, 2, 3, 4, 5];

  set(a, 1);
  set(b, 2);
  set(arr, 3);
  assert.sameValue(arr.length, 3);
  assert.sameValue(arr[3], undefined);
  set(a, 11);
  set(arr, 1);
  assert.sameValue(arr.length, 1);
  assert.sameValue(a.length, 11);
  assert.sameValue(b.length, 2);
})();

// A dictionary (map mode) receiver is uncacheable and mixes safely into a polymorphic callsite.
(function () {
  var dict = {};
  for (var i = 0; i < 80; i++) {
    dict["k" + i] = i;
  }
  dict.x = 0;

  function set(o, v) { o.x = v; }
  var a = { x: 0 };
  var b = { pad: 0, x: 0 };

  set(a, 1);
  set(b, 2);
  for (var j = 0; j < 3; j++) {
    set(dict, j);
    assert.sameValue(dict.x, j);
    set(a, 10 + j);
    assert.sameValue(a.x, 10 + j);
  }
  assert.sameValue(b.x, 2);
})();

// A setter that re-enters the same callsite with a different shape leaves both stores correct.
(function () {
  var seen = [];
  var plain = { pad: 0, x: 0 };
  var reentrant = {
    set x(v) { seen.push(v); set(plain, v * 2); },
  };

  function set(o, v) { o.x = v; }

  set(plain, 1);
  set(reentrant, 5);
  assert.sameValue(plain.x, 10);

  set(reentrant, 7);
  assert.sameValue(plain.x, 14);
  set(plain, 3);
  assert.sameValue(plain.x, 3);
  set(reentrant, 9);
  assert.sameValue(plain.x, 18);
  assert.sameValue(seen.join(), "5,7,9");
})();

// A polymorphic cache survives garbage collection with all of its entries intact.
(function () {
  var setterCalls = 0;
  var accessorProto = {};
  Object.defineProperty(accessorProto, "x", {
    set: function (v) { setterCalls++; },
    configurable: true,
  });

  function set(o, v) { o.x = v; }
  var a = { x: 0 };
  var b = { p1: 0, x: 0 };
  var c = Object.create(accessorProto);
  function makeFresh() { return { q: 0 }; }

  set(a, 1);
  set(b, 2);
  set(c, 3);
  set(makeFresh(), 4);

  $262.gc();

  set(a, 5);
  set(b, 6);
  set(c, 7);
  var fresh = makeFresh();
  set(fresh, 8);

  $262.gc();

  assert.sameValue(a.x, 5);
  assert.sameValue(b.x, 6);
  assert.sameValue(setterCalls, 2);
  assert.sameValue(fresh.x, 8);
})();

// Receivers from another realm have different shapes, so a shared callsite goes polymorphic
// across realm boundaries.
(function () {
  var other = $262.createRealm();
  other.evalScript("globalThis.make = function () { return { x: 0 }; };");

  function set(o, v) { o.x = v; }

  var mine = { x: 0 };
  var theirs = other.global.make();

  set(mine, 1);
  set(theirs, 2);
  set(mine, 3);
  set(theirs, 4);
  assert.sameValue(mine.x, 3);
  assert.sameValue(theirs.x, 4);
})();
