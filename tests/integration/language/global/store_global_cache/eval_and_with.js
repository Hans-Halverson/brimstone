/*---
description: >
  Globals created or written through indirect eval and with(globalThis) blocks stay coherent
  with warm LoadGlobal/StoreGlobal cache sites.
flags: [noStrict]
---*/

// Indirect eval runs in the global scope: a var it creates becomes visible to a warm
// typeof (LoadGlobalOrUnresolved) site and writable through a warm StoreGlobal site.
(function () {
  function type() { return typeof ind1; }
  assert.sameValue(type(), "undefined");
  assert.sameValue(type(), "undefined");

  (0, eval)("var ind1 = 'created';");
  assert.sameValue(type(), "string");

  function set(v) { ind1 = v; }
  set("a");
  set("b");
  assert.sameValue(globalThis.ind1, "b");
})();

// An indirect eval assignment writes through to a property a warm load site reads.
(function () {
  globalThis.ind2 = 1;
  function get() { return ind2; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  (0, eval)("ind2 = 2;");
  assert.sameValue(get(), 2);
})();

// Stores inside with(globalThis) write the same property cells warm cache sites read.
(function () {
  globalThis.w1 = 1;
  function get() { return w1; }
  assert.sameValue(get(), 1);
  assert.sameValue(get(), 1);
  with (globalThis) {
    w1 = 2;
  }
  assert.sameValue(get(), 2);
})();

// A with(globalThis) read observes a delete, and the warm cached site then reports the name
// as unresolved.
(function () {
  globalThis.w2 = "x";
  function get() { return w2; }
  get();
  get();
  delete globalThis.w2;
  with (globalThis) {
    assert.sameValue(typeof w2, "undefined");
  }
  assert.throws(ReferenceError, function () { get(); });
})();
