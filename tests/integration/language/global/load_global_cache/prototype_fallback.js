/*---
description: >
  Deleting a cached own global property falls back to a property inherited from the global
  object's prototype chain, and inherited properties are never stale-cached.
---*/

// Delete an own property that shadows an Object.prototype property.
(function () {
  Object.defineProperty(Object.prototype, "pfall1", {
    value: "inherited",
    writable: true,
    configurable: true,
  });
  globalThis.pfall1 = "own";
  function get() { return pfall1; }
  assert.sameValue(get(), "own");
  assert.sameValue(get(), "own");

  delete globalThis.pfall1;
  assert.sameValue(get(), "inherited");

  // Mutations of the prototype property are observed (the site must not have cached it).
  Object.prototype.pfall1 = "inherited2";
  assert.sameValue(get(), "inherited2");

  // Redefining the own property is observed again.
  globalThis.pfall1 = "own2";
  assert.sameValue(get(), "own2");

  delete globalThis.pfall1;
  delete Object.prototype.pfall1;
})();

// A name that resolves through the prototype chain from the first execution.
(function () {
  Object.defineProperty(Object.prototype, "pfall2", {
    value: 1,
    writable: true,
    configurable: true,
  });
  function get() { return pfall2; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);

  // A later own property wins over the inherited one.
  globalThis.pfall2 = 2;
  assert.sameValue(get(), 2);

  // Deleting the own property falls back to the inherited one again.
  delete globalThis.pfall2;
  assert.sameValue(get(), 1);

  delete Object.prototype.pfall2;
})();

// An inherited accessor is called with the global object as receiver on every access.
(function () {
  var receivers = [];
  Object.defineProperty(Object.prototype, "pfall3", {
    get: function () {
      receivers.push(this);
      return receivers.length;
    },
    configurable: true,
  });
  function get() { return pfall3; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 2);
  assert.sameValue(receivers[0], globalThis);
  assert.sameValue(receivers[1], globalThis);
  delete Object.prototype.pfall3;
})();
