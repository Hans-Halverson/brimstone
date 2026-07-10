/*---
description: >
  Global var and function declaration instantiation respects the global object's integrity
  level, matching CanDeclareGlobalVar / CanDeclareGlobalFunction / CreateGlobalFunctionBinding.
---*/

// Set up existing bindings before locking the global object.
$262.evalScript("var existingVar = 1;");
$262.evalScript("function existingFn() { return 1; }");
Object.defineProperty(globalThis, "nonConfigData", {
  value: 2,
  writable: true,
  enumerable: true,
  configurable: false,
});
Object.defineProperty(globalThis, "nonConfigFrozen", {
  value: 3,
  writable: false,
  enumerable: false,
  configurable: false,
});
Object.defineProperty(globalThis, "nonConfigAccessor", {
  get: function () { return 4; },
  configurable: false,
});

Object.preventExtensions(globalThis);

// A new global var cannot be created on a non-extensible global object.
assert.throws(TypeError, function () { $262.evalScript("var newVar = 1;"); });
assert.sameValue("newVar" in globalThis, false);

// A new global function cannot be created either.
assert.throws(TypeError, function () { $262.evalScript("function newFn() {}"); });
assert.sameValue("newFn" in globalThis, false);

// Redeclaring existing var and function bindings is still allowed.
$262.evalScript("var existingVar;");
assert.sameValue(existingVar, 1);
$262.evalScript("function existingFn() { return 2; }");
assert.sameValue(existingFn(), 2);

// A function can be declared over an existing non-configurable but writable and enumerable
// data property.
$262.evalScript("function nonConfigData() { return 5; }");
assert.sameValue(nonConfigData(), 5);

// ...but not over a non-configurable property that is non-writable or an accessor.
assert.throws(TypeError, function () { $262.evalScript("function nonConfigFrozen() {}"); });
assert.sameValue(nonConfigFrozen, 3);
assert.throws(TypeError, function () { $262.evalScript("function nonConfigAccessor() {}"); });
assert.sameValue(nonConfigAccessor, 4);

// A var declaration over any existing own property (even frozen) is allowed and is a no-op.
$262.evalScript("var nonConfigFrozen;");
assert.sameValue(nonConfigFrozen, 3);

// Warm caches observing a function redeclaration over an existing binding stay correct.
(function () {
  function call() { return existingFn(); }
  assert.sameValue(call(), 2);
  assert.sameValue(call(), 2);
  $262.evalScript("function existingFn() { return 3; }");
  assert.sameValue(call(), 3);
})();
