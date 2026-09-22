/*---
description: >
  new.target values whose `prototype` cannot be cached (proxies, bound functions without an own
  data `prototype`, accessors) are handled by a full `prototype` lookup on every construction,
  including observable getter and trap calls. Bound functions with an own data `prototype` are
  cached.
---*/

// A proxy new.target without traps forwards the `prototype` lookup to its target.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }
  function Target() {}
  var proxy = new Proxy(Target, {});

  for (var i = 0; i < 3; i++) {
    var o = Reflect.construct(Derived, [], proxy);
    assert.sameValue(Object.getPrototypeOf(o), Target.prototype);
    assert.sameValue(o.base, true);
  }
})();

// A proxy new.target's get trap is called on every construction.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }
  var protos = [{ id: 0 }, { id: 1 }, { id: 2 }];
  var calls = 0;
  var proxy = new Proxy(function () {}, {
    get: function (target, key) {
      if (key === "prototype") {
        return protos[calls++];
      }
      return target[key];
    },
  });

  for (var i = 0; i < 3; i++) {
    var o = Reflect.construct(Derived, [], proxy);
    assert.sameValue(Object.getPrototypeOf(o), protos[i]);
  }
  assert.sameValue(calls, 3);
})();

// A bound function new.target has no own `prototype`, so the receiver gets %Object.prototype%.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }
  function F() {}
  var bound = F.bind(null);

  for (var i = 0; i < 3; i++) {
    var o = Reflect.construct(Derived, [], bound);
    assert.sameValue(Object.getPrototypeOf(o), Object.prototype);
    assert.sameValue(o.base, true);
  }
})();

// A bound function new.target with an own data `prototype` is cached, and reassigning that
// `prototype` is observed.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }
  var bound = function () {}.bind(null);
  var proto = { own: true };
  bound.prototype = proto;

  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], bound)), proto);
  }

  var replaced = { replaced: true };
  bound.prototype = replaced;
  assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], bound)), replaced);
})();

// A bound function new.target with an own accessor `prototype` calls the getter on every
// construction.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }
  var protos = [{ id: 0 }, { id: 1 }, { id: 2 }];
  var calls = 0;
  var bound = function () {}.bind(null);
  Object.defineProperty(bound, "prototype", {
    get: function () { return protos[calls++]; },
  });

  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], bound)), protos[i]);
  }
  assert.sameValue(calls, 3);
})();

// Bound function new.targets in map mode: without a `prototype`, with an accessor `prototype`, and
// with a data `prototype`.
(function () {
  class Base {
    constructor() { this.base = true; }
  }
  class Derived extends Base {
    constructor() { super(); }
  }
  function makeBig() {
    var bound = function () {}.bind(null);
    for (var i = 0; i < 80; i++) {
      bound["s" + i] = i;
    }
    return bound;
  }

  var missing = makeBig();
  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], missing)), Object.prototype);
  }

  var calls = 0;
  var accessorProto = { accessor: true };
  var accessor = makeBig();
  Object.defineProperty(accessor, "prototype", {
    get: function () { calls++; return accessorProto; },
  });
  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], accessor)), accessorProto);
  }
  assert.sameValue(calls, 3);

  var dataProto = { data: true };
  var data = makeBig();
  data.prototype = dataProto;
  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(Reflect.construct(Derived, [], data)), dataProto);
  }
})();

// A non-object `prototype` on a constructor from another realm falls back to that realm's
// %Object.prototype%.
(function () {
  var other = $262.createRealm().global;
  var OtherF = other.Function("this.x = 1;");
  OtherF.prototype = null;

  for (var i = 0; i < 3; i++) {
    var o = new OtherF();
    assert.sameValue(Object.getPrototypeOf(o), other.Object.prototype);
    assert.sameValue(o.x, 1);
  }

  var proto = {};
  OtherF.prototype = proto;
  assert.sameValue(Object.getPrototypeOf(new OtherF()), proto);
})();

// Constructing a base class directly through Reflect.construct creates the receiver outside of a
// construct instruction.
(function () {
  function F() { this.x = 1; }
  class Other {}

  for (var i = 0; i < 3; i++) {
    var o = Reflect.construct(F, [], Other);
    assert.sameValue(Object.getPrototypeOf(o), Other.prototype);
    assert.sameValue(o.x, 1);
  }
})();
