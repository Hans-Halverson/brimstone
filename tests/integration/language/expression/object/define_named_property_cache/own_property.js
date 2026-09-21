/*---
description: DefineNamedProperty cache handles defines which overwrite an existing own property.
---*/

// Duplicate keys in a literal overwrite the existing property at a warm site.
(function () {
  function make(v) { return { a: v, a: v * 10 }; }
  for (var i = 1; i < 5; i++) {
    var o = make(i);
    assert.sameValue(o.a, i * 10);
    assert.sameValue(Object.keys(o).join(), "a");
  }
})();

// The last of three duplicate keys wins and the overwrite keeps default attributes.
(function () {
  function make(v) { return { a: v, b: 0, a: v + 1, a: v + 2 }; }
  for (var i = 0; i < 5; i++) {
    var o = make(i);
    assert.sameValue(o.a, i + 2);
    assert.sameValue(Object.keys(o).join(), "a,b");
    var desc = Object.getOwnPropertyDescriptor(o, "a");
    assert.sameValue(desc.writable, true);
    assert.sameValue(desc.enumerable, true);
    assert.sameValue(desc.configurable, true);
  }
})();

// A class field that shadows a field already defined by the base class overwrites it.
(function () {
  class Base {
    x = 1;
  }
  class Sub extends Base {
    x = 2;
  }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(new Base().x, 1);
    assert.sameValue(new Sub().x, 2);
  }
})();
