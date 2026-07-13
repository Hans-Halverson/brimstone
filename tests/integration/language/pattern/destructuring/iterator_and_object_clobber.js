/*---
description: Iterator and object destructuring when the pattern reassigns the source iterator/object
---*/

// Later targets read the original object, not the reassigned source
function objectPropertyAfterClobber() {
  let a = { a: 1, b: 2, c: 3 };
  let b, c;
  ({ a, b, c } = a);
  return [a, b, c];
}
assert.compareArray(objectPropertyAfterClobber(), [1, 2, 3]);

// Array bound targets are iterator-safe, so only the result value is observable
function arrayAssignmentResult() {
  let a = [10, 20];
  let b;
  const source = a;
  const result = ([a, b] = a);
  return result === source;
}
assert.sameValue(arrayAssignmentResult(), true);

// Object assignment result is the original object, not the reassigned source
function objectAssignmentResult() {
  let a = { a: 1, b: 2 };
  let b;
  const source = a;
  const result = ({ a, b } = a);
  return result === source;
}
assert.sameValue(objectAssignmentResult(), true);

// Same clobber through the hoisted-live var declaration path
function varDeclarationClobber() {
  var a = { a: 1, b: 2 };
  var { a, b } = a;
  return b;
}
assert.sameValue(varDeclarationClobber(), 2);

// For-update destructuring that walks its own source chain
function forUpdateWalk() {
  const root = { key: "root", parentPath: null };
  const mid = { key: "mid", parentPath: root };
  const leaf = { key: "leaf", parentPath: mid };

  const seen = [];
  for (let { parentPath, key } = leaf; parentPath; { parentPath, key } = parentPath) {
    seen.push(key);
  }
  return seen;
}
assert.compareArray(forUpdateWalk(), ["leaf", "mid"]);

// Aliased target read before the clobber when it is not first
function aliasedTargetNotFirst() {
  let a = { a: 1, b: 2 };
  let b;
  ({ b, a } = a);
  return [b, a];
}
assert.compareArray(aliasedTargetNotFirst(), [2, 1]);

// Array bound targets read from the iterator captured up front
function arrayBoundVariables() {
  let a = [10, 20];
  let b;
  [a, b] = a;
  return [a, b];
}
assert.compareArray(arrayBoundVariables(), [10, 20]);

// Reading the source under let is a TDZ access, not a clobber
function letTdzThrows() {
  let { a, b } = a;
  return b;
}
assert.throws(ReferenceError, letTdzThrows);