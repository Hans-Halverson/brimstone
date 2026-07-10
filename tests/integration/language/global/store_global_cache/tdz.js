/*---
description: >
  A warm StoreGlobal cache site must throw a ReferenceError when assigning while the name is in
  the TDZ of a global lexical binding, and must write to the lexical binding after it is
  initialized.
---*/

// let binding: assignment during the TDZ throws and must not modify the shadowed property.
(function () {
  globalThis.stdz1 = "property";
  function set(v) { stdz1 = v; }
  set("a");
  set("b");
  assert.sameValue(globalThis.stdz1, "b");

  globalThis.checkStdz1 = function () {
    assert.throws(ReferenceError, function () { set("during-tdz"); });
  };
  $262.evalScript("checkStdz1(); let stdz1 = 'lexical';");
  // The global object property was not modified by the TDZ store attempt.
  assert.sameValue(globalThis.stdz1, "b");

  // After initialization, stores go to the lexical binding, not the shadowed property.
  set("c");
  assert.sameValue(globalThis.stdz1, "b");
  globalThis.readStdz1 = function () { return stdz1; };
  assert.sameValue(readStdz1(), "c");
})();

// const binding: assignment after initialization must throw TypeError at a warm site.
(function () {
  globalThis.stdz2 = "property";
  function set(v) { stdz2 = v; }
  set("a");
  set("b");

  globalThis.checkStdz2 = function () {
    assert.throws(ReferenceError, function () { set("during-tdz"); });
  };
  $262.evalScript("checkStdz2(); const stdz2 = 'lexical';");
  assert.throws(TypeError, function () { set("c"); });
  assert.sameValue(globalThis.stdz2, "b");
})();
