/*---
description: >
  Assignment hazards in class extends clauses and computed keys, which are evaluated together.
---*/

function Base() {}

// Assignment in a class expression's extends clause or computed key
function inExtends(a) {
  return a + (class extends (a = 5, Object) {}, 0);
}
assert.sameValue(inExtends(1), 1);

function inMethodKey(a) {
  return a + (class { [(a = 5, "m")]() {} }, 0);
}
assert.sameValue(inMethodKey(1), 1);

function inMethodKeyTopLevel(a) {
  return a + (class { [a = 5]() {} }, 0);
}
assert.sameValue(inMethodKeyTopLevel(1), 1);

function inFieldKeyUpdate(a) {
  return a + (class { [a++] = 1 }, 0);
}
assert.sameValue(inFieldKeyUpdate(1), 1);

// Class expression followed by a read and an assignment in the enclosing expression
function afterExtends(a) {
  return (class extends Object {}, a) + (a = 5);
}
assert.sameValue(afterExtends(1), 6);

function afterComputedKey(a) {
  return (class { ["m"]() {} }, a) + (a = 5);
}
assert.sameValue(afterComputedKey(1), 6);

function inPatternDefault(a) {
  let [x = (class { ["f"] = 1 }, a) + (a = 5)] = [];
  return x;
}
assert.sameValue(inPatternDefault(1), 6);

// Extends value held while computed keys are evaluated
function declMethodKey(a) {
  class C extends a { [(a = null, "m")]() {} }
  return Object.getPrototypeOf(C);
}
assert.sameValue(declMethodKey(Base), Base);

function declTopLevelKey() {
  let a = Base;
  class C extends a { [a = "m"]() {} }
  return Object.getPrototypeOf(C);
}
assert.sameValue(declTopLevelKey(), Base);

function declFieldKey(a) {
  class C extends a { [(a = null, "f")] = 1 }
  return Object.getPrototypeOf(C);
}
assert.sameValue(declFieldKey(Base), Base);

function exprMethodKey(a) {
  const C = class extends a { [(a = null, "m")]() {} };
  return Object.getPrototypeOf(C);
}
assert.sameValue(exprMethodKey(Base), Base);
