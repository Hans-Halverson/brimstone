/*---
description: >
  Enumeration of the global object stays correct when properties are shadowed by lexical
  bindings, deleted during for-in, added during for-in, or deleted and re-added.
---*/

// A property shadowed by a global lexical binding is still an own property of the global
// object with its original value, and still enumerates.
(function () {
  globalThis.enum1 = "property";
  function get() { return enum1; }
  get();
  get();
  $262.evalScript("let enum1 = 'lexical';");
  assert.sameValue(get(), "lexical");

  assert.sameValue(Object.keys(globalThis).indexOf("enum1") >= 0, true);
  assert.sameValue(Object.getOwnPropertyDescriptor(globalThis, "enum1").value, "property");
  assert.sameValue(globalThis.enum1, "property");
})();

// Properties deleted during for-in enumeration before being visited must not be visited.
(function () {
  globalThis.del1 = 1;
  globalThis.del2 = 2;
  globalThis.del3 = 3;
  var seen = [];
  for (var key in globalThis) {
    if (key === "del1") {
      delete globalThis.del2;
      delete globalThis.del3;
    }
    if (key.slice(0, 3) === "del") {
      seen.push(key);
    }
  }
  assert.sameValue(seen.join(","), "del1");
  delete globalThis.del1;
})();

// Adding many properties during for-in (forcing the global properties map to grow) must not
// crash or visit stale entries.
(function () {
  globalThis.grow0 = 0;
  var added = false;
  var seenGrow0 = false;
  for (var key in globalThis) {
    if (!added) {
      added = true;
      for (var i = 1; i <= 100; i++) {
        globalThis["grow" + i] = i;
      }
    }
    if (key === "grow0") {
      seenGrow0 = true;
    }
  }
  assert.sameValue(seenGrow0, true);
  for (var i = 0; i <= 100; i++) {
    delete globalThis["grow" + i];
  }
})();

// Deleting and re-adding a property moves it to the end of the creation order.
(function () {
  globalThis.ord1 = 1;
  globalThis.ord2 = 2;
  globalThis.ord3 = 3;
  delete globalThis.ord2;
  globalThis.ord2 = 22;
  var ordKeys = Object.getOwnPropertyNames(globalThis).filter(function (k) {
    return k.slice(0, 3) === "ord";
  });
  assert.sameValue(ordKeys.join(","), "ord1,ord3,ord2");
  delete globalThis.ord1;
  delete globalThis.ord2;
  delete globalThis.ord3;
})();
