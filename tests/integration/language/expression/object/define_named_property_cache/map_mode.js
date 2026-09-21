/*---
description: DefineNamedProperty stays correct for map mode receivers (objects with more properties than array mode allows), including defines that overwrite an existing entry, defines that move the receiver into map mode, and defines on non-extensible receivers.
---*/

// A class field that overwrites an existing default data property on a map mode receiver is
// cached as an in-place store and stays correct.
(function () {
  class Base {
    constructor() {
      for (var i = 0; i < 70; i++) {
        this["k" + i] = i;
      }
      this.x = "base";
    }
  }
  class Sub extends Base {
    x = "field";
  }
  for (var j = 0; j < 5; j++) {
    var o = new Sub();
    assert.sameValue(o.x, "field");
    assert.sameValue(o.k0, 0);
    assert.sameValue(o.k69, 69);
  }
})();

// A class field that overwrites a non-default property on a map mode receiver resets its
// attributes every time.
(function () {
  class Base {
    constructor() {
      for (var i = 0; i < 70; i++) {
        this["k" + i] = i;
      }
      Object.defineProperty(this, "x", { value: "base", writable: false, configurable: true });
    }
  }
  class Sub extends Base {
    x = "field";
  }
  for (var j = 0; j < 5; j++) {
    var o = new Sub();
    var desc = Object.getOwnPropertyDescriptor(o, "x");
    assert.sameValue(desc.value, "field");
    assert.sameValue(desc.writable, true);
    assert.sameValue(desc.enumerable, true);
  }
})();

// A class field that adds a new property to a map mode receiver.
(function () {
  class Base {
    constructor() {
      for (var i = 0; i < 70; i++) {
        this["k" + i] = i;
      }
    }
  }
  class Sub extends Base {
    added = 1;
  }
  for (var j = 0; j < 5; j++) {
    var o = new Sub();
    assert.sameValue(o.added, 1);
    assert.sameValue(Object.keys(o).length, 71);
  }
})();

// A class field define that moves the receiver from array mode into map mode.
(function () {
  function makeBase(count) {
    return class {
      constructor() {
        for (var i = 0; i < count; i++) {
          this["k" + i] = i;
        }
      }
    };
  }
  // Cover receivers on both sides of the array mode limit at the same define site
  for (var count = 60; count < 70; count++) {
    class Sub extends makeBase(count) {
      f1 = "a";
      f2 = "b";
      f3 = "c";
    }
    for (var j = 0; j < 3; j++) {
      var o = new Sub();
      assert.sameValue(o.f1, "a");
      assert.sameValue(o.f2, "b");
      assert.sameValue(o.f3, "c");
      assert.sameValue(o["k" + (count - 1)], count - 1);
      assert.sameValue(Object.keys(o).length, count + 3);
    }
  }
})();

// A class field define site warmed on extensible map mode receivers throws for a non-extensible
// receiver when the field is new, and still overwrites an existing field.
(function () {
  var lock = false;
  class Base {
    constructor() {
      for (var i = 0; i < 70; i++) {
        this["k" + i] = i;
      }
      this.existing = "base";
      if (lock) {
        Object.preventExtensions(this);
      }
    }
  }
  class AddsField extends Base {
    fresh = 1;
  }
  class OverwritesField extends Base {
    existing = "field";
  }

  for (var i = 0; i < 5; i++) {
    assert.sameValue(new AddsField().fresh, 1);
    assert.sameValue(new OverwritesField().existing, "field");
  }

  lock = true;
  assert.throws(TypeError, function () { new AddsField(); });

  var o = new OverwritesField();
  assert.sameValue(o.existing, "field");
  assert.sameValue(Object.isExtensible(o), false);
})();

// A class field repeatedly defined on the same map mode receiver, which is mutated between
// defines and so abandons its old shape each time. The dead cache entry is replaced in place.
(function () {
  var target = {};
  for (var i = 0; i < 70; i++) {
    target["k" + i] = i;
  }
  target.x = "initial";

  class Base {
    constructor() {
      return target;
    }
  }
  class Sub extends Base {
    x = "field";
  }

  for (var j = 0; j < 10; j++) {
    target["added" + j] = j;
    target.x = "reset" + j;
    assert.sameValue(new Sub(), target);
    assert.sameValue(target.x, "field");

    target.x = "again" + j;
    new Sub();
    assert.sameValue(target.x, "field");
  }

  assert.sameValue(target.k69, 69);
  assert.sameValue(target.added9, 9);
})();

// A class field define site whose first define adds the field to a map mode receiver keeps
// working, and caches, once later defines overwrite an existing field at the same site.
(function () {
  var addField = true;
  class Base {
    constructor() {
      for (var i = 0; i < 70; i++) {
        this["k" + i] = i;
      }
      if (!addField) {
        this.x = "base";
      }
    }
  }
  class Sub extends Base {
    x = "field";
  }

  // First define at the site adds the field
  var o = new Sub();
  assert.sameValue(o.x, "field");
  assert.sameValue(Object.keys(o).length, 71);

  // Later defines at the same site overwrite an existing field
  addField = false;
  for (var j = 0; j < 5; j++) {
    var o = new Sub();
    assert.sameValue(o.x, "field");
    assert.sameValue(o.k69, 69);
    assert.sameValue(Object.keys(o).length, 71);
  }

  // And adding still works afterwards
  addField = true;
  var o = new Sub();
  assert.sameValue(o.x, "field");
  assert.sameValue(Object.keys(o).length, 71);
})();
