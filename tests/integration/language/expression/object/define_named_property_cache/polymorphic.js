/*---
description: DefineNamedProperty cache handles a define site seeing receivers of multiple shapes.
---*/

// A base class field site sees one receiver shape per subclass and is promoted to a polymorphic
// cache that stays correct for all of them.
(function () {
  class Base {
    field = 42;
  }
  class SubA extends Base { constructor() { super(); this.a = 1; } }
  class SubB extends Base { constructor() { super(); this.b = 2; } }
  class SubC extends Base { constructor() { super(); this.c = 3; } }
  for (var i = 0; i < 3; i++) {
    assert.sameValue(new Base().field, 42);
    assert.sameValue(new SubA().field, 42);
    assert.sameValue(new SubB().field, 42);
    assert.sameValue(new SubC().field, 42);
  }
})();

// More shapes than the polymorphic cache can hold still define correctly.
(function () {
  class Base {
    field = "v";
  }
  var subclasses = [];
  for (var i = 0; i < 8; i++) {
    subclasses.push(class extends Base {});
  }
  for (var round = 0; round < 3; round++) {
    for (var j = 0; j < subclasses.length; j++) {
      var o = new subclasses[j]();
      assert.sameValue(o.field, "v");
      assert.sameValue(o instanceof subclasses[j], true);
    }
  }
})();

// Base's field define site sees two receiver shapes (Base and Shadow instances), while Shadow's
// field define site overwrites the own property that Base's field just defined.
(function () {
  class Base {
    x = 10;
  }
  class Shadow extends Base {
    x = 20;
  }
  for (var i = 0; i < 3; i++) {
    assert.sameValue(new Base().x, 10);
    assert.sameValue(new Shadow().x, 20);
  }
})();

// Mid's field define site overwrites an existing own property under multiple receiver shapes
// (Mid and Sub instances have different root shapes), exercising polymorphic own property hits.
(function () {
  class Base {
    x = 1;
  }
  class Mid extends Base {
    x = 2;
  }
  class Sub extends Mid {}
  for (var i = 0; i < 3; i++) {
    assert.sameValue(new Mid().x, 2);
    assert.sameValue(new Sub().x, 2);
  }
})();
