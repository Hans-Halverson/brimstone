/*---
description: DefineNamedProperty cache handles defines which add a new property via a shape transition.
---*/

// A warm object literal site defines properties via cached shape transitions.
(function () {
  function make(v) {
    return { left: v, right: v + 1, key: v + 2, value: v + 3 };
  }
  for (var i = 0; i < 5; i++) {
    var o = make(i);
    assert.sameValue(o.left, i);
    assert.sameValue(o.right, i + 1);
    assert.sameValue(o.key, i + 2);
    assert.sameValue(o.value, i + 3);
    assert.sameValue(Object.keys(o).join(), "left,right,key,value");
  }
})();

// The defined properties have default attributes.
(function () {
  function make(v) { return { x: v }; }
  make(1);
  make(2);
  var desc = Object.getOwnPropertyDescriptor(make(3), "x");
  assert.sameValue(desc.value, 3);
  assert.sameValue(desc.writable, true);
  assert.sameValue(desc.enumerable, true);
  assert.sameValue(desc.configurable, true);
})();

// A warm class field site defines fields on fresh instances.
(function () {
  class Point {
    x = 1;
    y = 2;
  }
  for (var i = 0; i < 5; i++) {
    var p = new Point();
    assert.sameValue(p.x, 1);
    assert.sameValue(p.y, 2);
    assert.sameValue(Object.keys(p).join(), "x,y");
  }
})();

// A warm static class field site defines fields on fresh constructors.
(function () {
  function make(v) {
    return class {
      static tag = v;
    };
  }
  for (var i = 0; i < 5; i++) {
    assert.sameValue(make(i).tag, i);
  }
})();

// Defines around the array mode property limit grow the properties array correctly.
(function () {
  function make(v) {
    return {
      w0: v, w1: v, w2: v, w3: v, w4: v, w5: v, w6: v, w7: v,
      w8: v, w9: v, w10: v, w11: v, w12: v, w13: v, w14: v, w15: v,
      w16: v, w17: v, w18: v, w19: v, w20: v, w21: v, w22: v, w23: v,
      w24: v, w25: v, w26: v, w27: v, w28: v, w29: v, w30: v, w31: v,
      w32: v, w33: v, w34: v, w35: v, w36: v, w37: v, w38: v, w39: v,
      w40: v, w41: v, w42: v, w43: v, w44: v, w45: v, w46: v, w47: v,
      w48: v, w49: v, w50: v, w51: v, w52: v, w53: v, w54: v, w55: v,
      w56: v, w57: v, w58: v, w59: v, w60: v, w61: v, w62: v, w63: v,
      w64: v, w65: v,
    };
  }
  for (var i = 0; i < 3; i++) {
    var o = make(i);
    assert.sameValue(o.w0, i);
    assert.sameValue(o.w63, i);
    assert.sameValue(o.w65, i);
    assert.sameValue(Object.keys(o).length, 66);
  }
})();

// Many static fields on fresh constructors overflow into the named properties array and grow it
// at warm define sites.
(function () {
  function make(v) {
    return class {
      static s1 = v; static s2 = v; static s3 = v; static s4 = v;
      static s5 = v; static s6 = v; static s7 = v; static s8 = v;
      static s9 = v; static s10 = v;
    };
  }
  for (var i = 0; i < 3; i++) {
    var klass = make(i);
    assert.sameValue(klass.s1, i);
    assert.sameValue(klass.s9, i);
    assert.sameValue(klass.s10, i);
  }
})();

// A cached literal define stays correct across a garbage collection.
(function () {
  function make(v) { return { x: v, y: v + 1 }; }
  make(1);
  $262.gc();
  var o = make(2);
  assert.sameValue(o.x, 2);
  assert.sameValue(o.y, 3);
})();
