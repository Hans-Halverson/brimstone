/*---
description: >
  A warm LoadGlobal cache site must throw a ReferenceError when the name enters the TDZ of a
  global lexical binding declared by a later script, and must read the lexical binding after it
  is initialized.
---*/

// let binding: the TDZ window is between declaration instantiation and initialization.
(function () {
  globalThis.ltdz1 = "property";
  function get() { return ltdz1; }
  assert.sameValue(get(), "property");
  assert.sameValue(get(), "property");

  globalThis.checkLtdz1 = function () {
    // The binding is registered but uninitialized: the cached site must throw, not fall back
    // to the shadowed global object property.
    assert.throws(ReferenceError, function () { get(); });
  };
  $262.evalScript("checkLtdz1(); let ltdz1 = 'lexical';");
  assert.sameValue(get(), "lexical");
})();

// const binding.
(function () {
  globalThis.ctdz1 = "property";
  function get() { return ctdz1; }
  get(); get();

  globalThis.checkCtdz1 = function () {
    assert.throws(ReferenceError, function () { get(); });
  };
  $262.evalScript("checkCtdz1(); const ctdz1 = 'lexical';");
  assert.sameValue(get(), "lexical");
})();

// A class declaration also creates a shadowing lexical binding with a TDZ.
(function () {
  globalThis.kls1 = "property";
  function get() { return kls1; }
  get(); get();

  globalThis.checkKls1 = function () {
    assert.throws(ReferenceError, function () { get(); });
  };
  $262.evalScript("checkKls1(); class kls1 {}");
  assert.sameValue(typeof get(), "function");
})();

// typeof through a warm cache site must also throw during the TDZ (typeof is only safe for
// unresolved names, not uninitialized lexical bindings).
(function () {
  globalThis.ttdz1 = "property";
  function type() { return typeof ttdz1; }
  assert.sameValue(type(), "string");
  assert.sameValue(type(), "string");

  globalThis.checkTtdz1 = function () {
    assert.throws(ReferenceError, function () { type(); });
  };
  $262.evalScript("checkTtdz1(); let ttdz1 = 42;");
  assert.sameValue(type(), "number");
})();
