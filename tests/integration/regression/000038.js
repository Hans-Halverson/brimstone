/*---
description: >
  Logical assignment to a binding that is not in a fixed register evaluates to the old value on the
  short circuit path.
---*/

// Global binding, right side is a parameter
var globalOr = 1;
function globalOrParam(param) {
  return globalOr ||= param;
}
assert.sameValue(globalOrParam(2), 1);
assert.sameValue(globalOr, 1);

// Global binding, right side is a local
var globalOrLocal = 1;
function globalOrLocalFn(param) {
  var local = param;
  return globalOrLocal ||= local;
}
assert.sameValue(globalOrLocalFn(2), 1);

// Captured scope binding
function scopeOr(param) {
  let captured = 1;
  (() => captured)();
  return captured ||= param;
}
assert.sameValue(scopeOr(2), 1);

// Const register binding is not written on the short circuit path
function constOr(param) {
  const immutable = 1;
  return immutable ||= param;
}
assert.sameValue(constOr(2), 1);

// Named function expression's own name
var fnName = function self(param) {
  return self ||= param;
};
assert.sameValue(typeof fnName(2), "function");

// Nullish coalescing and logical and
var globalNullish = 0;
function globalNullishParam(param) {
  return globalNullish ??= param;
}
assert.sameValue(globalNullishParam(2), 0);

var globalAnd = 0;
function globalAndParam(param) {
  return globalAnd &&= param;
}
assert.sameValue(globalAndParam(2), 0);

// Assignment path still stores and returns the new value
var globalAssigned = 0;
function globalAssignedParam(param) {
  return globalAssigned ||= param;
}
assert.sameValue(globalAssignedParam(2), 2);
assert.sameValue(globalAssigned, 2);
