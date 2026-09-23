/*---
description: >
  Construct instructions cache the receiver shape for a new.target whose `prototype` is an own data
  property, and receivers created from the cached shape behave like any other receiver.
---*/

// A warm construct site creates receivers with the constructor's prototype and own properties.
(function () {
  function Point(x, y) {
    this.x = x;
    this.y = y;
  }
  Point.prototype.sum = function () { return this.x + this.y; };

  for (var i = 0; i < 5; i++) {
    var p = new Point(i, 10);
    assert.sameValue(Object.getPrototypeOf(p), Point.prototype);
    assert(p instanceof Point);
    assert.sameValue(p.x, i);
    assert.sameValue(p.y, 10);
    assert.sameValue(p.sum(), i + 10);
    assert.compareArray(Object.keys(p), ["x", "y"]);
  }
})();

// Class constructors with fields are cached the same way.
(function () {
  class Counter {
    count = 0;
    constructor(step) {
      this.step = step;
    }
    increment() { this.count += this.step; return this.count; }
  }

  for (var i = 1; i <= 5; i++) {
    var c = new Counter(i);
    assert.sameValue(Object.getPrototypeOf(c), Counter.prototype);
    assert.sameValue(c.increment(), i);
    assert.sameValue(c.increment(), 2 * i);
    assert.compareArray(Object.keys(c), ["count", "step"]);
  }
})();

// Spread arguments use the varargs construct instruction, which is cached as well.
(function () {
  function Triple(a, b, c) {
    this.values = [a, b, c];
  }

  for (var i = 0; i < 5; i++) {
    var args = [i, i + 1, i + 2];
    var t = new Triple(...args);
    assert.sameValue(Object.getPrototypeOf(t), Triple.prototype);
    assert.compareArray(t.values, args);
  }
})();

// Each construct site has its own cache, and different constructors at different sites do not
// interfere with each other.
(function () {
  function A() { this.a = 1; }
  function B() { this.b = 2; }

  for (var i = 0; i < 5; i++) {
    var a1 = new A();
    var b1 = new B();
    var a2 = new A();
    assert.sameValue(Object.getPrototypeOf(a1), A.prototype);
    assert.sameValue(Object.getPrototypeOf(a2), A.prototype);
    assert.sameValue(Object.getPrototypeOf(b1), B.prototype);
    assert.sameValue(a1.a, 1);
    assert.sameValue(b1.b, 2);
    assert.sameValue(a1.b, undefined);
    assert.sameValue(b1.a, undefined);
  }
})();

// Receivers from a warm site are independent objects. Mutating one receiver (adding many
// properties, deleting, freezing, changing its prototype) does not affect the next receiver.
(function () {
  function Thing() { this.x = 1; }
  function make() { return new Thing(); }

  var first = make();
  var second = make();
  for (var i = 0; i < 100; i++) {
    second["p" + i] = i;
  }
  delete second.x;
  Object.freeze(second);

  var third = make();
  Object.setPrototypeOf(third, null);

  var fourth = make();
  assert.notSameValue(first, fourth);
  assert.compareArray(Object.keys(fourth), ["x"]);
  assert.sameValue(Object.getPrototypeOf(fourth), Thing.prototype);
  assert(Object.isExtensible(fourth));
  assert.sameValue(fourth.p0, undefined);

  fourth.y = 2;
  assert.sameValue(fourth.y, 2);
  assert.sameValue(second.p99, 99);
  assert.sameValue(Object.isFrozen(second), true);
  assert.sameValue(Object.getPrototypeOf(third), null);
})();

// Receivers stay correct when garbage collections happen between constructions.
(function () {
  function Node(value, next) {
    this.value = value;
    this.next = next;
  }

  var list = null;
  for (var i = 0; i < 10; i++) {
    list = new Node(i, list);
    if (i % 3 === 0) {
      $262.gc();
    }
  }

  for (var i = 9; i >= 0; i--) {
    assert.sameValue(Object.getPrototypeOf(list), Node.prototype);
    assert.sameValue(list.value, i);
    list = list.next;
  }
  assert.sameValue(list, null);
})();

// A constructor that returns an object replaces the receiver, and the site keeps working for
// constructions that use the receiver.
(function () {
  var replacement = { replaced: true };
  function Maybe(replace) {
    this.own = true;
    if (replace) {
      return replacement;
    }
  }

  for (var i = 0; i < 6; i++) {
    var result = new Maybe(i % 2 === 0);
    if (i % 2 === 0) {
      assert.sameValue(result, replacement);
    } else {
      assert.sameValue(Object.getPrototypeOf(result), Maybe.prototype);
      assert.sameValue(result.own, true);
    }
  }
})();

// new.target inside the constructor is the constructor for a warm site.
(function () {
  var seen = [];
  function Target() { seen.push(new.target); }

  for (var i = 0; i < 3; i++) {
    new Target();
  }
  assert.sameValue(seen.length, 3);
  assert.sameValue(seen[0], Target);
  assert.sameValue(seen[1], Target);
  assert.sameValue(seen[2], Target);
})();
