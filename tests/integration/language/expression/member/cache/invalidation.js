/*---
description: GetNamedProperty cache misses and refills on shape transitions, prototype mutations, and polymorphism.
---*/

// Data property redefined as an accessor changes the shape, the cache must switch to the getter.
(function () {
  function get(o) { return o.x; }
  var o = { x: 1 };
  get(o); get(o);
  Object.defineProperty(o, "x", { get: function () { return 99; } });
  assert.sameValue(get(o), 99);
})();

// Accessor redefined as a data property changes the shape, the cache must switch to the new value.
(function () {
  function get(o) { return o.x; }
  var o = { get x() { return 1; } };
  get(o); get(o);
  Object.defineProperty(o, "x", { value: 5, configurable: true });
  assert.sameValue(get(o), 5);
})();

// Replacing the prototype invalidates a not-found lookup.
(function () {
  function get(o) { return o.x; }
  var o = {};
  get(o); get(o);
  Object.setPrototypeOf(o, { x: 11 });
  assert.sameValue(get(o), 11);
})();

// A shadowing property added to an intermediate prototype overrides a deeper holder.
(function () {
  function get(o) { return o.x; }
  var top = { x: 1 };
  var mid = Object.create(top);
  var o = Object.create(mid);
  assert.sameValue(get(o), 1);
  assert.sameValue(get(o), 1);
  mid.x = 2;
  assert.sameValue(get(o), 2);
})();

// Removing the prototype's property falls through to the next prototype.
(function () {
  function get(o) { return o.x; }
  var top = { x: 1 };
  var mid = Object.create(top);
  mid.x = 2;
  var o = Object.create(mid);
  assert.sameValue(get(o), 2);
  assert.sameValue(get(o), 2);
  delete mid.x;
  assert.sameValue(get(o), 1);
})();

// One callsite, many shapes: the cache fails and uses the slow path.
(function () {
  function get(o) { return o.x; }
  assert.sameValue(get({ x: 1 }), 1);
  assert.sameValue(get({ x: 2, y: 0 }), 2);
  assert.sameValue(get({ x: 3 }), 3);
  assert.sameValue(get(Object.create({ x: 4 })), 4);
  assert.sameValue(get({ x: 5 }), 5);
})();

// Changing an intermediate prototype's own prototype is reflected at a warm callsite.
(function () {
  function get(o) { return o.x; }
  var top = { x: 1 };
  var mid = Object.create(top);
  var o = Object.create(mid);
  assert.sameValue(get(o), 1);
  assert.sameValue(get(o), 1);
  Object.setPrototypeOf(mid, { x: 2 });
  assert.sameValue(get(o), 2);
})();

// Freezing the receiver transitions its shape, so the cached entry must miss.
(function () {
  function get(o) { return o.x; }
  var o = { x: 1 };
  get(o); get(o);
  Object.freeze(o);
  assert.sameValue(get(o), 1);
})();
