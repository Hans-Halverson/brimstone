/*---
description: >
  super() calls to a base class constructor use the construct cache keyed by new.target, so
  receivers get new.target's prototype no matter which derived class is being constructed.
---*/

// A derived class construction creates its receiver in the base class through super().
(function () {
  class Base {
    constructor(x) { this.x = x; }
  }
  class Derived extends Base {
    constructor(x) {
      super(x);
      this.y = x * 2;
    }
  }

  for (var i = 0; i < 5; i++) {
    var d = new Derived(i);
    assert.sameValue(Object.getPrototypeOf(d), Derived.prototype);
    assert(d instanceof Base);
    assert.sameValue(d.x, i);
    assert.sameValue(d.y, i * 2);
    assert.compareArray(Object.keys(d), ["x", "y"]);
  }
})();

// A super() call site near the root of a hierarchy sees every leaf class as new.target.
(function () {
  class Root {
    constructor() { this.root = true; }
  }
  class Node extends Root {
    constructor(kind) {
      super();
      this.kind = kind;
    }
  }
  class Leaf1 extends Node { constructor() { super(1); } }
  class Leaf2 extends Node { constructor() { super(2); this.second = true; } }
  class Leaf3 extends Node { constructor() { super(3); } }
  var leaves = [Leaf1, Leaf2, Leaf3, Node];

  for (var round = 0; round < 3; round++) {
    for (var i = 0; i < leaves.length; i++) {
      var o = i === 3 ? new Node(4) : new leaves[i]();
      assert.sameValue(Object.getPrototypeOf(o), leaves[i].prototype);
      assert.sameValue(o.root, true);
      assert.sameValue(o.kind, i + 1);
      assert.sameValue(o.second, i === 1 ? true : undefined);
    }
  }
})();

// Implicit derived constructors forward their arguments to the base class from the runtime, while
// explicit super(...args) uses the varargs construct instruction.
(function () {
  class Base {
    constructor(a, b) {
      this.a = a;
      this.b = b;
    }
  }
  class Implicit extends Base {}
  class Explicit extends Base {
    constructor(...args) { super(...args); }
  }

  for (var i = 0; i < 5; i++) {
    var implicit = new Implicit(i, i + 1);
    var explicit = new Explicit(i, i + 2);
    assert.sameValue(Object.getPrototypeOf(implicit), Implicit.prototype);
    assert.sameValue(Object.getPrototypeOf(explicit), Explicit.prototype);
    assert.sameValue(implicit.b, i + 1);
    assert.sameValue(explicit.b, i + 2);
  }
})();

// A different new.target passed through Reflect.construct reaches the super() call site.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }
  class Other {}

  for (var i = 0; i < 3; i++) {
    var o = Reflect.construct(Derived, [], Other);
    assert.sameValue(Object.getPrototypeOf(o), Other.prototype);
    assert.sameValue(o.base, true);

    var d = new Derived();
    assert.sameValue(Object.getPrototypeOf(d), Derived.prototype);
  }
})();

// A built-in constructor as new.target gives an ordinary receiver with that built-in's prototype.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }

  for (var i = 0; i < 3; i++) {
    var o = Reflect.construct(Derived, [], Array);
    assert.sameValue(Object.getPrototypeOf(o), Array.prototype);
    assert.sameValue(Array.isArray(o), false);
    assert.sameValue(o.base, true);
  }
})();

// Reassigning a function new.target's `prototype` is observed by a warm super() call site.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }
  function Target() {}

  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], Target)), Target.prototype);
  }

  var proto = { replaced: true };
  Target.prototype = proto;
  assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], Target)), proto);

  Target.prototype = 1;
  assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], Target)), Object.prototype);
})();

// A derived constructor that returns an object or undefined is unaffected by the cache.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  var replacement = { replaced: true };
  class ReturnsObject extends Base {
    constructor() {
      super();
      return replacement;
    }
  }
  class ReturnsUndefined extends Base {
    constructor() {
      super();
      return undefined;
    }
  }

  for (var i = 0; i < 3; i++) {
    assert.sameValue(new ReturnsObject(), replacement);
    var o = new ReturnsUndefined();
    assert.sameValue(Object.getPrototypeOf(o), ReturnsUndefined.prototype);
    assert.sameValue(o.base, true);
  }
})();
