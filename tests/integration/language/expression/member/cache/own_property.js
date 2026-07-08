/*---
description: GetNamedProperty cache handles own data and accessor properties.
---*/

// Own data property.
(function () {
  function get(o) { return o.x; }
  var o = { x: 1 };
  assert.sameValue(get(o), 1);
  assert.sameValue(get(o), 1);
  o.x = 2;
  assert.sameValue(get(o), 2);
})();

// Deleting the property transitions the shape, so the cached entry must miss.
(function () {
  function get(o) { return o.x; }
  var o = { x: 1 };
  get(o); get(o);
  delete o.x;
  assert.sameValue(get(o), undefined);
})();

// Adding a second property after warming grows the shape but keeps x reachable.
(function () {
  function get(o) { return o.x; }
  var o = { x: 1 };
  get(o); get(o);
  o.y = 2;
  assert.sameValue(get(o), 1);
})();

// Own getters.
(function () {
  var seenThis;
  function get(o) { return o.x; }
  var o = {
    n: 10,
    get x() { seenThis = this; return this.n; },
  };
  assert.sameValue(get(o), 10);
  o.n = 20;
  assert.sameValue(get(o), 20);
  assert.sameValue(seenThis, o);
})();

// Getter replaced in place keeps the same shape.
(function () {
  function get(o) { return o.x; }
  var o = { get x() { return 1; } };
  get(o); get(o);
  Object.defineProperty(o, "x", { get: function () { return 2; } });
  assert.sameValue(get(o), 2);
})();

// Getter removed in place leaves a setter-only accessor, returns undefined.
(function () {
  function get(o) { return o.x; }
  var o = { get x() { return 1; } };
  get(o); get(o);
  Object.defineProperty(o, "x", { get: undefined });
  assert.sameValue(get(o), undefined);
})();

// Setter-only accessor reads as undefined through the cache.
(function () {
  function get(o) { return o.x; }
  var o = { set x(v) {} };
  assert.sameValue(get(o), undefined);
  assert.sameValue(get(o), undefined);
})();

// A cached getter that throws propagates the exception on a warm cache hit.
(function () {
  var shouldThrow = false;
  function get(o) { return o.x; }
  var o = { get x() { if (shouldThrow) { throw new TypeError("boom"); } return 1; } };
  assert.sameValue(get(o), 1);
  assert.sameValue(get(o), 1);
  shouldThrow = true;
  assert.throws(TypeError, function () { get(o); });
})();

// An own data property read stays correct across a garbage collection.
(function () {
  function get(o) { return o.x; }
  var o = { x: 1 };
  assert.sameValue(get(o), 1);
  $262.gc();
  assert.sameValue(get(o), 1);
})();
