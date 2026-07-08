/*---
description: GetNamedProperty cache handles data and accessor properties from the prototype chain.
---*/

// Prototype data property.
(function () {
  function get(o) { return o.x; }
  var proto = { x: 1 };
  var o = Object.create(proto);
  assert.sameValue(get(o), 1);
  assert.sameValue(get(o), 1);
  proto.x = 2;
  assert.sameValue(get(o), 2);
})();

// An own property shadows the prototype.
(function () {
  function get(o) { return o.x; }
  var proto = { x: 1 };
  var o = Object.create(proto);
  get(o); get(o);
  o.x = 5;
  assert.sameValue(get(o), 5);
})();

// Two receivers sharing a prototype have the same shape.
(function () {
  function get(o) { return o.x; }
  var proto = { x: 7 };
  var a = Object.create(proto);
  var b = Object.create(proto);
  assert.sameValue(get(a), 7);
  assert.sameValue(get(b), 7);
})();

// Prototype getter.
(function () {
  var seenThis;
  function get(o) { return o.x; }
  var proto = { get x() { seenThis = this; return 42; } };
  var o = Object.create(proto);
  assert.sameValue(get(o), 42);
  assert.sameValue(get(o), 42);
  assert.sameValue(seenThis, o);
})();

// Setter-only accessor on the prototype reads as undefined.
(function () {
  function get(o) { return o.x; }
  var proto = { set x(v) {} };
  var o = Object.create(proto);
  assert.sameValue(get(o), undefined);
  assert.sameValue(get(o), undefined);
})();

// A cached prototype getter binds `this` to the actual receiver, not the one that filled the cache.
(function () {
  function get(o) { return o.x; }
  var proto = { get x() { return this.tag; } };
  var a = Object.create(proto); a.tag = "a";
  var b = Object.create(proto); b.tag = "b";
  assert.sameValue(get(a), "a");
  assert.sameValue(get(a), "a");
  assert.sameValue(get(b), "b");
})();

// Replacing the getter in place on the prototype is seen at a warm callsite.
(function () {
  function get(o) { return o.x; }
  var proto = { get x() { return 1; } };
  var o = Object.create(proto);
  get(o); get(o);
  Object.defineProperty(proto, "x", { get: function () { return 2; }, configurable: true });
  assert.sameValue(get(o), 2);
})();

// A prototype data property read stays correct across a garbage collection.
(function () {
  function get(o) { return o.x; }
  var proto = { x: 42 };
  var o = Object.create(proto);
  assert.sameValue(get(o), 42);
  $262.gc();
  assert.sameValue(get(o), 42);
})();
