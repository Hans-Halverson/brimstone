/*---
description: DefineNamedProperty defines which cannot be cached stay correct at warm sites.
---*/

// A class field that replaces an accessor defined by the base constructor is not a plain store,
// and must replace the accessor with a data property every time without calling it.
(function () {
  var getterCalls = 0;
  class Base {
    constructor() {
      Object.defineProperty(this, "x", {
        get: function () { getterCalls++; return 1; },
        configurable: true,
      });
    }
  }
  class Sub extends Base {
    x = 5;
  }
  for (var i = 0; i < 3; i++) {
    var o = new Sub();
    assert.sameValue(o.x, 5);
    var desc = Object.getOwnPropertyDescriptor(o, "x");
    assert.sameValue(desc.writable, true);
    assert.sameValue(desc.get, undefined);
  }
  assert.sameValue(getterCalls, 0);
})();

// A class field that redefines a non-configurable non-writable own property throws every time.
(function () {
  class Base {
    constructor() {
      Object.defineProperty(this, "x", { value: 1, writable: false, configurable: false });
    }
  }
  class Sub extends Base {
    x = 5;
  }
  for (var i = 0; i < 3; i++) {
    assert.throws(TypeError, function () { new Sub(); });
  }
})();

// A non-extensible receiver at a warm class field site must throw.
(function () {
  var seal = false;
  class Base {
    constructor() {
      if (seal) {
        Object.preventExtensions(this);
      }
    }
  }
  class Sub extends Base {
    x = 1;
  }
  assert.sameValue(new Sub().x, 1);
  assert.sameValue(new Sub().x, 1);
  seal = true;
  assert.throws(TypeError, function () { new Sub(); });
})();

// An array index key in a literal is not cached but stays correct at a warm site.
(function () {
  function make(v) { return { "0": v, x: v + 1 }; }
  for (var i = 0; i < 3; i++) {
    var o = make(i);
    assert.sameValue(o[0], i);
    assert.sameValue(o.x, i + 1);
  }
})();

// A class field defined on a map mode receiver is not cached but stays correct.
(function () {
  class Base {
    constructor() {
      for (var i = 0; i < 70; i++) {
        this["k" + i] = i;
      }
    }
  }
  class Sub extends Base {
    x = 7;
  }
  for (var j = 0; j < 3; j++) {
    var o = new Sub();
    assert.sameValue(o.x, 7);
    assert.sameValue(o.k69, 69);
  }
})();
