/*---
description: >
  Construct cache supports new.target constructors in map mode (constructors with more properties
  than array mode allows) and stays correct across prototype reassignment and map mutations.
---*/

// A constructor with many static properties is in map mode and its site is cached.
(function () {
  function F(x) { this.x = x; }
  for (var i = 0; i < 80; i++) {
    F["s" + i] = i;
  }

  for (var i = 0; i < 5; i++) {
    var o = new F(i);
    assert.sameValue(Object.getPrototypeOf(o), F.prototype);
    assert.sameValue(o.x, i);
  }
  assert.sameValue(F.s79, 79);
})();

// A class with many static members is in map mode, and its subclasses construct receivers through
// the class's super() call site.
(function () {
  class Big {
    constructor(x) { this.x = x; }
  }
  for (var i = 0; i < 80; i++) {
    Big["s" + i] = i;
  }
  class Sub extends Big {
    constructor(x) {
      super(x);
      this.sub = true;
    }
  }

  for (var i = 0; i < 5; i++) {
    var big = new Big(i);
    var sub = new Sub(i);
    assert.sameValue(Object.getPrototypeOf(big), Big.prototype);
    assert.sameValue(Object.getPrototypeOf(sub), Sub.prototype);
    assert.sameValue(sub.x, i);
    assert.sameValue(sub.sub, true);
    assert.sameValue(Sub.s0, 0);
  }
})();

// Reassigning `prototype` of a map mode constructor is observed by a warm site.
(function () {
  function F() {}
  for (var i = 0; i < 80; i++) {
    F["s" + i] = i;
  }
  function make() { return new F(); }
  make();
  make();

  var proto = { replaced: true };
  F.prototype = proto;
  assert.sameValue(Object.getPrototypeOf(make()), proto);
  assert.sameValue(Object.getPrototypeOf(make()), proto);

  F.prototype = null;
  assert.sameValue(Object.getPrototypeOf(make()), Object.prototype);

  F.prototype = proto;
  assert.sameValue(Object.getPrototypeOf(make()), proto);
})();

// Adding and deleting properties on a map mode constructor (which may move `prototype` within the
// map) does not return a stale prototype.
(function () {
  function F() {}
  for (var i = 0; i < 80; i++) {
    F["s" + i] = i;
  }
  var proto = F.prototype;
  function make() { return new F(); }
  make();
  make();

  for (var i = 0; i < 200; i++) {
    F["t" + i] = i;
  }
  assert.sameValue(Object.getPrototypeOf(make()), proto);

  for (var i = 0; i < 80; i++) {
    delete F["s" + i];
  }
  assert.sameValue(Object.getPrototypeOf(make()), proto);
  assert.sameValue(Object.getPrototypeOf(make()), proto);

  var replaced = {};
  F.prototype = replaced;
  assert.sameValue(Object.getPrototypeOf(make()), replaced);
})();

// A constructor that moves into map mode after its site was cached.
(function () {
  function F() { this.x = 1; }
  var proto = F.prototype;
  function make() { return new F(); }
  make();
  make();

  for (var i = 0; i < 80; i++) {
    F["s" + i] = i;
  }
  for (var i = 0; i < 3; i++) {
    var o = make();
    assert.sameValue(Object.getPrototypeOf(o), proto);
    assert.sameValue(o.x, 1);
  }
})();

// A map mode constructor so large that its `prototype` property is outside the range that can be
// cached still constructs correctly.
//
// 8192 properties grows the map to 16384 entries, placing `prototype` well past the largest
// cacheable byte offset. Single character keys minimize allocations per property.
(function () {
  function F() { this.x = 1; }
  for (var i = 0; i < 8192; i++) {
    F[String.fromCharCode(0x100 + i)] = i;
  }
  var proto = { big: true };
  F.prototype = proto;

  for (var i = 0; i < 3; i++) {
    var o = new F();
    assert.sameValue(Object.getPrototypeOf(o), proto);
    assert.sameValue(o.x, 1);
  }
})();
