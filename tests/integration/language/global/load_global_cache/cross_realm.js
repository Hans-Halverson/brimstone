/*---
description: >
  LoadGlobal/StoreGlobal caches inside a function created in another realm resolve against that
  function's own realm, and cached accessor hits use that realm's global object as receiver.
---*/

// A warm function from another realm keeps reading its own realm's global.
(function () {
  var other = $262.createRealm();
  other.evalScript("var cr1 = 'other'; function getCr1() { return cr1; }");
  globalThis.cr1 = "main";
  var getOther = other.global.getCr1;
  function getMain() { return cr1; }

  for (var i = 0; i < 3; i++) {
    assert.sameValue(getOther(), "other");
    assert.sameValue(getMain(), "main");
  }

  // Mutating each realm's global is observed only by that realm's functions.
  other.global.cr1 = "other2";
  globalThis.cr1 = "main2";
  assert.sameValue(getOther(), "other2");
  assert.sameValue(getMain(), "main2");
})();

// Store caches write to the defining realm's global.
(function () {
  var other = $262.createRealm();
  other.evalScript("var cr2 = 0; function setCr2(v) { cr2 = v; }");
  globalThis.cr2 = "main";
  var setOther = other.global.setCr2;
  setOther(1);
  setOther(2);
  setOther(3);
  assert.sameValue(other.global.cr2, 3);
  assert.sameValue(globalThis.cr2, "main");
})();

// A cached accessor hit passes the defining realm's global as the receiver.
(function () {
  var other = $262.createRealm();
  other.evalScript(
    "var receivers = [];" +
      "Object.defineProperty(globalThis, 'cr3', {" +
      "  get: function () { receivers.push(this); return receivers.length; }," +
      "  configurable: true," +
      "});" +
      "function getCr3() { return cr3; }"
  );
  globalThis.cr3 = "main";
  var getOther = other.global.getCr3;
  assert.sameValue(getOther(), 1);
  // Second call is a warm cache accessor hit.
  assert.sameValue(getOther(), 2);
  // Assert inside the other realm that both receivers were that realm's global object.
  other.evalScript(
    "if (receivers.length !== 2 || receivers[0] !== globalThis || receivers[1] !== globalThis) {" +
      "  throw new Error('cached accessor got wrong receiver');" +
      "}"
  );
})();
