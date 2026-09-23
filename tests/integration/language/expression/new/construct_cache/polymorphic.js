/*---
description: >
  A single construct site that sees several new.targets refills its cache for each new.target and
  always creates receivers with the correct prototype and own properties.
---*/

// Distinct constructors alternating at one site.
(function () {
  function A() { this.kind = "a"; }
  function B() { this.kind = "b"; this.extra = 1; }
  function C() { this.kind = "c"; }
  var ctors = [A, B, C];

  function make(Ctor) { return new Ctor(); }
  for (var i = 0; i < 12; i++) {
    var Ctor = ctors[i % 3];
    var o = make(Ctor);
    assert.sameValue(Object.getPrototypeOf(o), Ctor.prototype);
    assert.sameValue(o.kind, Ctor === A ? "a" : Ctor === B ? "b" : "c");
    assert.sameValue(o.extra, Ctor === B ? 1 : undefined);
  }
})();

// Constructors with the same shape but different prototypes (fresh classes from a factory share
// bytecode and shape but each has its own prototype object).
(function () {
  function makeClass(id) {
    return class {
      constructor() { this.id = id; }
    };
  }
  var classes = [];
  for (var i = 0; i < 4; i++) {
    classes.push(makeClass(i));
  }

  function make(Ctor) { return new Ctor(); }
  for (var round = 0; round < 3; round++) {
    for (var i = 0; i < classes.length; i++) {
      var o = make(classes[i]);
      assert.sameValue(Object.getPrototypeOf(o), classes[i].prototype);
      assert.sameValue(o.id, i);
    }
  }
})();

// Constructors that share a prototype object but add different numbers of properties. Receivers
// are correct even when created from a shape cached for the other constructor.
(function () {
  function Small() { this.a = 1; }
  function Large() {
    for (var i = 0; i < 20; i++) {
      this["p" + i] = i;
    }
  }
  Large.prototype = Small.prototype;

  function make(Ctor) { return new Ctor(); }
  for (var round = 0; round < 3; round++) {
    var small = make(Small);
    var large = make(Large);
    var large2 = make(Large);
    assert.sameValue(Object.getPrototypeOf(small), Small.prototype);
    assert.sameValue(Object.getPrototypeOf(large), Small.prototype);
    assert.sameValue(small.a, 1);
    assert.sameValue(Object.keys(large).length, 20);
    assert.sameValue(large.p0, 0);
    assert.sameValue(large.p19, 19);
    assert.sameValue(large2.p19, 19);
  }
})();

// An inheritance helper that creates a fresh intermediate constructor for every class, as emitted
// by some transpilers.
(function () {
  function extend(Derived, Base) {
    function Intermediate() { this.constructor = Derived; }
    Intermediate.prototype = Base.prototype;
    Derived.prototype = new Intermediate();
  }

  function Base() {}
  Base.prototype.base = true;

  var classes = [];
  for (var i = 0; i < 4; i++) {
    var Derived = function () {};
    extend(Derived, Base);
    classes.push(Derived);
  }

  for (var i = 0; i < classes.length; i++) {
    var proto = classes[i].prototype;
    assert.sameValue(Object.getPrototypeOf(proto), Base.prototype);
    assert.sameValue(proto.constructor, classes[i]);
    var o = new classes[i]();
    assert.sameValue(Object.getPrototypeOf(o), proto);
    assert.sameValue(o.base, true);
  }
})();
