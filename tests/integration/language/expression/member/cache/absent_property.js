/*---
description: GetNamedProperty cache handles absent properties.
---*/

// Missing property, then added to the receiver: the shape transition forces a miss.
(function () {
  function get(o) { return o.x; }
  var o = { y: 1 };
  assert.sameValue(get(o), undefined);
  assert.sameValue(get(o), undefined);
  o.x = 3;
  assert.sameValue(get(o), 3);
})();

// Missing property, then added to the prototype: the guard must invalidate so the new value is seen.
(function () {
  function get(o) { return o.x; }
  var proto = {};
  var o = Object.create(proto);
  get(o); get(o);
  proto.x = 8;
  assert.sameValue(get(o), 8);
})();

// Not found across a deep chain, then added at the top: the guard spans the whole chain.
(function () {
  function get(o) { return o.x; }
  var top = {};
  var mid = Object.create(top);
  var o = Object.create(mid);
  get(o); get(o);
  top.x = 1;
  assert.sameValue(get(o), 1);
})();

// Null-prototype receiver: an absent property is undefined with no guard needed.
(function () {
  function get(o) { return o.x; }
  var o = Object.create(null);
  o.y = 1;
  assert.sameValue(get(o), undefined);
  assert.sameValue(get(o), undefined);
})();

// An unrelated property added to the prototype invalidates the not-found guard, but the property stays absent.
(function () {
  function get(o) { return o.x; }
  var proto = {};
  var o = Object.create(proto);
  assert.sameValue(get(o), undefined);
  assert.sameValue(get(o), undefined);
  proto.y = 1;
  assert.sameValue(get(o), undefined);
  proto.x = 9;
  assert.sameValue(get(o), 9);
})();

// A not-found lookup stays undefined across a garbage collection.
(function () {
  function get(o) { return o.x; }
  var proto = { y: 1 };
  var o = Object.create(proto);
  assert.sameValue(get(o), undefined);
  $262.gc();
  assert.sameValue(get(o), undefined);
})();
